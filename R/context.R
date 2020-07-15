
#' @export
#' @importFrom instrumentr create_context
create_evil_context <- function() {
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
    application_load_callback = application_load_callback,
    application_unload_callback = application_unload_callback,
    call_exit_callback = call_exit_callback,
    functions = functions
  )
}
