#' @export
#' @importFrom instrumentr create_context
eval_tracer <- function() {
  functions <- c(
    "base::eval",
    "base::evalq",
    "base::eval.parent",
    "base::local",

    "base::parse",
    "base::str2expression",
    "base::str2lang"
  )

  create_context(
    call_exit_callback = trace_eval_callback,
    functions = functions
  )
}

#' @importFrom instrumentr get_data set_data get_id get_name get_parameters
#' @importFrom instrumentr get_arguments get_position get_expression
#' @importFrom instrumentr is_evaluated get_frame_position get_environment
#' @importFrom instrumentr get_caller is_successful
#' @importFrom digest sha1
trace_eval_callback <- function(context, application, package, func, call) {
  call_name <- get_name(func)
  if (call_name %in% c("parse", "str2expression", "str2lang")) {
    retval <- returnValue()
    mark_parsed_expression(retval, call_name)
    return()
  }

  eval_call_id <- get_id(call)
  eval_function <- get_name(func)
  eval_call_env <- get_environment(call)
  eval_call_expression <- get_expression(call)
  eval_call_srcref <- get_call_srcref(eval_call_expression)
  eval_call_frame_position <- get_frame_position(call)

  ## browser(expr=startsWith(expr_to_string(eval_call_expression), "evalq(as.list(NULL)"))

  application_frame_position <- get_frame_position(application)

  ## eval, evalq and local use `envir` parameter name to denote environment
  ## eval.parent uses `p` to denote evaluation environment
  envir_name <- if (eval_function == "eval.parent") "p" else "envir"

  eval_env <- get(envir_name, envir = eval_call_env)
  environment_class <- NA
  # TODO resolve environments if it is an integer (sys.call)
  if (is.environment(eval_env)) {
    environment_class <- classify_environment(
      application_frame_position,
      eval_call_frame_position,
      eval_call_env,
      eval_env
    )
  }
  enclos_env <- eval_call_env$enclos

  caller <- get_caller(call)
  caller_expression <- caller$call_expression
  caller_package <- caller$package_name
  caller_function <- caller$function_name
  caller_srcref <- get_call_srcref(caller_expression)

  caller_expression_2 <- .Empty
  caller_srcref_2 <- NA

  if (identical(eval_call_expression, caller_expression)) {
    for (idx in seq(sys.nframe()-2, 1, -1)) {
      tmp <- sys.call(idx)
      if (is.call(tmp) &&
            !(as.character(tmp[[1]]) %in% c("tryCatchList", "tryCatchOne", "doTryCatch"))) {
        caller_expression_2 <- tmp
        break
      }
    }
    if (!is_empty(caller_expression_2)) {
      caller_srcref_2 <- get_call_srcref(caller_expression_2)
    }
  }

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
    expr_resolved <- resolve_expr(eval_call_env$expr, eval_env)
  } else {
    .Empty
  }

  expr_parsed <- attr(expr_resolved, "._evil_parsed_expression")
  if (is.null(expr_parsed)) {
    expr_parsed <- NA
  }

  envir_expression <- .Empty
  envir_forced <- NA
  enclos_expression <- .Empty
  enclos_forced <- NA

  if (eval_function == "eval.parent") {
    arg <- get_arguments(params$n)[[1]]
    expr <- get_expression(arg)

    envir_forced <- is_evaluated(arg)
    if(!identical(expr, .DefaultArgs[[eval_function]]$n)) {
      envir_expression <- substitute(parent.frame(1 + N), list(N=expr))
    }
    envir_default <- eval_call_env$n == 1
  } else {
    arg <- get_arguments(params$envir)[[1]]
    expr <- get_expression(arg)

    envir_forced <- is_evaluated(arg)
    if (!identical(expr, .DefaultArgs[[eval_function]]$envir)) {
      envir_expression <- expr
    }
    # TODO: check if the given environment is the same as the default one
  }

  if (eval_function %in% c("eval", "evalq")) {
    arg <- get_arguments(params$enclos)[[1]]
    expr <- get_expression(arg)

    enclos_forced <- is_evaluated(arg)
    if (!identical(expr, .DefaultArgs[[call_name]]$enclos)) {
      enclos_expression <- expr
    }
    # TODO: check if the given environment is the same as the default one
  }

  # TODO: move to S3
  expr_repr <- function(e) {
    if (!is_empty(e)) {
      s <- expr_to_string(e)
    
      list(
        text=if (is.language(e)) s else NA,
        hash=sha1(s),
        length=nchar(s),
        type=sexp_typeof(e)
      )
    } else {
      list(text=NA, hash=NA, length=NA, type=NA)
    }
  }

  expr_expression_repr <- expr_repr(expr_expression)
  expr_resolved_repr <- expr_repr(expr_resolved)

  trace <- data.frame(
    eval_call_id,
    eval_function=call_name,
    eval_call_expression=expr_to_string(eval_call_expression),
    eval_call_srcref,
    caller_package,
    caller_function,
    caller_expression=expr_to_string(caller_expression),
    caller_srcref,
    caller_expression_2=expr_to_string(caller_expression_2),
    caller_expression_2_raw=if (!is_empty(caller_expression_2)) {
      capture.output(print(caller_expression_2))
    } else {
      NA
    },
    caller_srcref_2,
    environment_class,
    successful=is_successful(call),

    expr_expression        = expr_expression_repr$text,
    expr_expression_hash   = expr_expression_repr$hash,
    expr_expression_length = expr_expression_repr$length,
    expr_expression_type   = expr_expression_repr$type,

    expr_resolved        = expr_resolved_repr$text,
    expr_resolved_hash   = expr_resolved_repr$hash,
    expr_resolved_length = expr_resolved_repr$length,
    expr_resolved_type   = expr_resolved_repr$type,

    expr_parsed,
    expr_forced,

    envir_expression=expr_to_string(envir_expression),
    envir_forced,
    envir_type=sexp_typeof(eval_env),

    enclos_expression=expr_to_string(enclos_expression),
    enclos_forced,
    enclos_type=sexp_typeof(enclos_env)
  )

  assign(as.character(get_id(arg)), trace, envir=get_data(context))
}

#' @export
trace_eval <- function(code, ...) {
  trace_code(context=eval_tracer(), code=code, ...)
}
