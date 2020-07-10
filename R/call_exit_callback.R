
#' @importFrom instrumentr get_data set_data get_id get_name get_parameters
#' @importFrom instrumentr get_arguments get_position get_expression
#' @importFrom instrumentr is_evaluated get_frame_position get_environment
#' @importFrom instrumentr get_caller is_successful
call_exit_callback <- function(context, application, package, func, call) {
  call_id <- get_id(call)
  call_package <- get_name(package)
  call_name <- get_name(func)
  call_env <- get_environment(call)
  call_expression <- get_expression(call)

  application_frame_position <- get_frame_position(application)

  call_frame_position <- get_frame_position(call)

  ## eval, evalq and local use `envir` parameter name to denote environment
  ## eval.parent uses `p` to denote evaluation environment
  envir_name <- if (call_name == "eval.parent") "p" else "envir"

  eval_env <- get(envir_name, envir = call_env)
  environment_class <- NA
  if (is.environment(eval_env)) {
    environment_class <- classify_environment(
      application_frame_position,
      call_frame_position,
      call_env,
      eval_env
    )
  }
  # TODO resolve environments if it is an integer (sys.call)

  caller <- get_caller(call)
  caller_expression <- caller$call_expression
  caller_package <- caller$package_name
  caller_name <- caller$function_name
  caller_srcref <- get_call_srcref(sys.call(call_frame_position))

  # eval: expr, envir, enclos
  # evalq: expr, envir, enclos
  # eval.parent: expr, n
  # local: expr, envir
  params <- get_parameters(call)
  names(params) <- sapply(params, get_name)

  # all evals define expr
  arg <- get_arguments(params$expr)[[1]]
  expr_expression <- get_expression(arg)
  expr_forced <- is_evaluated(arg)
  expr_resolved <- if (expr_forced) {
    expr_resolved <- resolve_expr(call_env$expr, eval_env)
  } else {
    .Empty
  }
  expr_resolved_type <- if (is_empty(expr_resolved)) {
    NA
  } else {
    sexp_typeof(expr_resolved)
  }

  # FIXME: this is wrong we have to trace parse and str2...
  expr_from_parse <- is.expression(expr_resolved) &&
    !is.null(attr(expr_resolved, "srcref"))
  expr_type <- typeof(expr_resolved)

  envir_expression <- .Empty
  envir_forced <- NA
  enclos_expression <- .Empty
  enclos_forced <- NA

  if (call_name == "eval.parent") {
    arg <- get_arguments(params$n)[[1]]
    expr <- get_expression(arg)

    envir_forced <- is_evaluated(arg)
    if(!identical(expr, .DefaultArgs[[call_name]]$n)) {
      envir_expression <- substitute(parent.frame(1 + N), list(N=expr))
    }
    envir_default <- call_env$n == 1
  } else {
    arg <- get_arguments(params$envir)[[1]]
    expr <- get_expression(arg)

    envir_forced <- is_evaluated(arg)
    if (!identical(expr, .DefaultArgs[[call_name]]$envir)) {
      envir_expression <- expr
    }
    # TODO: check if the given environment is the same as the default one
  }

  if (call_name %in% c("eval", "evalq")) {
    arg <- get_arguments(params$enclos)[[1]]
    expr <- get_expression(arg)

    enclos_forced <- is_evaluated(arg)
    if (!identical(expr, .DefaultArgs[[call_name]]$enclos)) {
      enclos_expression <- expr
    }
    # TODO: check if the given environment is the same as the default one
  }

  trace <- data.frame(
    call_id,
    package_name=call_package,
    function_name=call_name,
    call_expression=expr_to_string(call_expression),
    caller_expression=expr_to_string(caller_expression),
    caller_package,
    caller_name,
    caller_srcref,
    environment_class,
    successful=is_successful(call),

    expr_expression=expr_to_string(expr_expression),
    expr_resolved=expr_to_string(expr_resolved),
    expr_resolved_type,
    expr_from_parse,
    expr_forced,
    expr_type,

    envir_expression=expr_to_string(envir_expression),
    envir_forced,
    envir_type=env_type_to_string(eval_env),

    enclos_expression=expr_to_string(enclos_expression),
    enclos_forced,
    enclos_type=env_type_to_string(call_env$enclos)
  )

  assign(as.character(get_id(arg)), trace, envir=get_data(context))
}

resolve_expr <- function(x, env) {
  if (is.symbol(x)) {
    y <- get0(as.character(x), env, ifnotfound=.Empty)
    if (is.symbol(y)) {
      resolve_expr(y, env)
    } else {
      y
    }
  } else if (is.language(x)) {
    x
  } else {
    .Empty
  }
}

env_type_to_string <- function(env) {
  if (is.environment(env)) {
    NA
  } else {
    typeof(env)
  }
}

expr_to_string <- function(expr) {
  if (is_empty(expr)) {
    NA
  } else if (is.expression(expr)) {
    expr_to_string(expr[[1]])
  } else if (is.language(expr)) {
    paste(deparse(expr), collapse = "\n")
  } else {
    NA
  }
}

get_call_srcref <- function(call) {
  srcref <- attr(call, "srcref")

  call_srcref <- if (!is.null(srcref)) {
    filename <- getSrcFilename(srcref)
    filename <- if (is.null(filename)) "<unknown>" else filename

    first_line <- srcref[1]
    last_line <- srcref[3]
    first_col <- srcref[5]
    last_col <- srcref[6]

    paste0(
      c(filename, first_line, first_col, last_line, last_col),
      collapse=":"
    )
  } else {
    NA
  }

  call_srcref
}

get_loaded_package_environments <- function() { ## nolint
    envs <- list()
    env_names <- c()

    env <- .GlobalEnv

    while (!identical(env, emptyenv())) {
        env <- parent.env(env)
        envs <- c(envs, env)
        env_name <- environmentName(env)
        env_names <- c(env_names, env_name)
    }

    names(envs) <- env_names

    envs
}

classify_environment <- function(application_frame_position, ##nolint
                                 call_frame_position,
                                 callee_env,
                                 eval_env) {

    ## check bases cases

    ## The environments of primitive functions of base package are NULL
    if (is.null(eval_env)) {
        return("base")
    }

    if (identical(eval_env, emptyenv())) {
        return("empty")
    }

    if (identical(eval_env, callee_env)) {
        return("callee")
    }

    if (identical(eval_env, .GlobalEnv)) {
        return("global")
    }

    ## check if environment is a package environment

    package_envs <- get_loaded_package_environments()
    package_env_names <- names(package_envs)

    index <- length(package_envs)

    while (index != 0) {
        env <- package_envs[[index]]
        env_name <- package_env_names[index]

        if (identical(eval_env, env)) {
            return(env_name)
        }

        index <- index - 1
    }

    ## check if environment is a caller environment

    frames <- sys.frames()

    parents <- sys.parents()

    index <- parents[call_frame_position]

    parent_count <- 0

    while (index != 0) {
        parent_count <- parent_count + 1

        if (identical(frames[[index]], eval_env)) {
            break
        }

        index <- parents[index]
    }

    ## this means the eval_env did not belong to any of the parent callers
    ## it has to be a new.env and we need to recursively classify its parent env
    if (index == 0) {
        parent_class <- classify_environment(application_frame_position,
                                             call_frame_position,
                                             callee_env,
                                             parent.env(eval_env))
        return(paste("new", parent_class, sep = "+"))
    }
    ## this means the eval_env is one of the parent caller's environments
    else {
        return(paste("caller", parent_count, sep = "-"))
    }

    return("unhandled")
}
