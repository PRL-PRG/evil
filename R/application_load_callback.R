
#' @importFrom instrumentr set_data
application_load_callback <- function(context, application) {
  data <- new.env(parent = emptyenv())
  set_data(context, data)
}
