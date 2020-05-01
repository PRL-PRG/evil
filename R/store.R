
add_entry <- function(package_name, function_name, expression, stack_frame) {
    row <- data.frame(package_name = package_name,
                      function_name = function_name,
                      expression = expression,
                      stack_frame = stack_frame)

    state$eval_calls <- rbind(state$eval_calls, row)

    invisible(NULL)
}

#' @export
get_eval_calls <- function() {
    state$eval_calls
}

#' @export
clear_eval_calls <- function() {
    state$eval_calls <- data.frame(package_name = character(0),
                                   function_name = character(0),
                                   expression = character(0),
                                   stack_frame = character(0))
}
