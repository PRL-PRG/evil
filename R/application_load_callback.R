
#' @importFrom instrumentr set_data
application_load_callback <- function(context, application) {

    calls <- new.env(parent = emptyenv())

    arguments <- new.env(parent = emptyenv())

    data <- list(calls = calls, arguments = arguments)

    set_data(context, data)
}
