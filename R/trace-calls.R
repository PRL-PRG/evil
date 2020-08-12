#' @export
#' @importFrom instrumentr create_context
call_tracer <- function() {
  functions <- c(
    "base::replicate"
  )

  create_context(
    call_exit_callback = trace_call_callback,
    functions = functions
  )
}

#' @importFrom instrumentr get_data set_data get_id get_name get_parameters
#' @importFrom instrumentr get_arguments get_position get_expression
#' @importFrom instrumentr is_evaluated get_frame_position get_environment
#' @importFrom instrumentr get_caller is_successful
trace_call_callback <- function(context, application, package, func, call) {
  call_name <- get_name(func)

  call_env <- get_environment(call)
  call_expression <- get_expression(call)

  call_srcref <- get_call_srcref(call_expression)
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
  expr_parsed <- attr(expr_resolved, "._evil_parsed_expression")
  if (is.null(expr_parsed)) {
    expr_parsed <- NA
  }

  trace <- data.frame(
    a=1,
    call_name,
    call_expression=expr_to_string(call_expression),
    call_srcref,
    successful=is_successful(call),
    expr_expression=expr_to_string(expr_expression),
    expr_expression_type=sexp_typeof(expr_expression),
    expr_resolved=expr_to_string(expr_resolved),
    expr_resolved_type,
    expr_parsed,
    expr_forced
  )

  assign(as.character(get_id(call)), trace, envir=get_data(context))
}
