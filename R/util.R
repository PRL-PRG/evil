sexp_typeof <- function(x)
  .Call(C_sexp_typeof, x)

mark_parsed_expression <- function(x, parse_fun_name)
  .Call(C_mark_parsed_expression, x, parse_fun_name)

resolve_expr <- function(x, env) {
  if (!is.environment(env)) return(x)

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
  } else {
    paste(deparse(expr), collapse = "\n")
  }
}

get_call_srcref <- function(call) {
  srcref <- attr(call, "srcref")

  eval_call_srcref <- if (!is.null(srcref)) {
    file <- getSrcFilename(srcref)
    file <- if (is.null(file)) {
      "<unknown>"
    } else {
      dir <- getSrcDirectory(srcref)
      file.path(dir, file)
    }

    first_line <- srcref[1]
    last_line <- srcref[3]
    first_col <- srcref[5]
    last_col <- srcref[6]

    paste0(
      c(file, first_line, first_col, last_line, last_col),
      collapse=":"
    )
  } else {
    NA
  }

  eval_call_srcref
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
                                 eval_call_frame_position,
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

    index <- parents[eval_call_frame_position]

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
                                             eval_call_frame_position,
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
