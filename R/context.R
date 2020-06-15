
#' @export
#' @importFrom instrumentr create_context
create_evil_context <- function() {
    create_context(application_load_callback = application_load_callback,
                   call_exit_callback = call_exit_callback,
                   functions = c("base::eval", "base::evalq", "base::eval.parent", "base::local"))
}

#' @importFrom instrumentr set_data
application_load_callback <- function(context, application) {
    data <- data.frame(call_id = integer(0),
                       callee_package = character(0),
                       callee_name = character(0),
                       ##TODO: caller_package = character(0),
                       ##TODO: caller_name = character(0),
                       argument_position = integer(0),
                       argument_name = character(0),
                       argument_expr = character(0),
                       is_evaluated = logical(0),
                       stringsAsFactors = FALSE)

    set_data(context, data)
}

#' @importFrom instrumentr get_data set_data get_id get_name get_parameters
#' @importFrom instrumentr get_arguments get_position get_expression
#' @importFrom instrumentr is_evaluated to_string
call_exit_callback <- function(context, application, package, func, call) {
    data <- get_data(context)

    call_id <- get_id(call)
    callee_package <- get_name(package)
    callee_name <- get_name(func)

    for (parameter in get_parameters(call)) {
        parameter_name <- get_name(parameter)
        parameter_position <- get_position(parameter)
        ## NOTE: eval parameters don't have more than one argument
        ##       because they are never varargs
        argument <- get_arguments(parameter)[[1]]
        argument_expr <- paste(deparse(get_expression(argument)), collapse = "\n")
        evaluated <- is_evaluated(argument)

        data[nrow(data) + 1, ] <- list(call_id,
                                      callee_package,
                                      callee_name,
                                      parameter_position,
                                      parameter_name,
                                      argument_expr,
                                      evaluated)

    }

    set_data(context, data)
}
