
application_unload_callback <- function(context, application) {

    data <- get_data(context)
    calls <- data$calls
    arguments <- data$arguments

    new_data <- list(calls = create_call_table(calls),
                     arguments = create_argument_table(arguments))

    set_data(context, new_data)

}

create_call_table <- function(calls) {

    call_count <- length(calls)

    call_id <- integer(call_count)
    package_name <- character(call_count)
    function_name <- character(call_count)
    call_expression <- character(call_count)
    caller_expression <- character(call_count)
    caller_package <- character(call_count)
    caller_name <- character(call_count)
    environment_class <- character(call_count)

    index <- 0

    for(key in ls(calls)) {
        index <- index + 1

        call <- get(key, calls)

        call_id[index] <- call$call_id
        package_name[index] <- call$package_name
        function_name[index] <- call$function_name
        call_expression[index] <- call$call_expression
        caller_expression[index] <- call$caller_expression
        caller_package[index] <- call$caller_package
        caller_name[index] <- call$caller_name
        environment_class[index] <- call$environment_class
    }

    call_df <- data.frame(call_id = call_id,
                          package_name = package_name,
                          function_name = function_name,
                          call_expression = call_expression,
                          caller_expression = caller_expression,
                          caller_package = caller_package,
                          caller_name = caller_name,
                          environment_class = environment_class,
                          stringsAsFactors = FALSE)

    call_df
}

create_argument_table <- function(arguments) {

    argument_count <- length(arguments)

    call_id <- integer(argument_count)
    package_name <- character(argument_count)
    function_name <- character(argument_count)
    argument_position <- integer(argument_count)
    argument_name <- character(argument_count)
    argument_expr <- character(argument_count)
    is_evaluated <- logical(argument_count)

    index <- 0

    for(key in ls(arguments)) {
        index <- index + 1

        argument <- get(key, arguments)

        call_id[index] <- argument$call_id
        package_name[index] <- argument$package_name
        function_name[index] <- argument$function_name
        argument_position[index] <- argument$argument_position
        argument_name[index] <- argument$argument_name
        argument_expr[index] <- argument$argument_expr
        is_evaluated[index] <- argument$is_evaluated
    }

    argument_df <- data.frame(call_id = call_id,
                              package_name = package_name,
                              function_name = function_name,
                              argument_position = argument_position,
                              argument_name = argument_name,
                              argument_expr = argument_expr,
                              is_evaluated = is_evaluated,
                              stringsAsFactors = FALSE)

    argument_df
}
