
#' @export
#' @importFrom methods is
#' @importFrom instrumentr set_application_load_callback set_application_unload_callback
#' @importFrom instrumentr set_data get_data trace_code get_frame_position
trace_code <- function(code,
                       envir = parent.frame(),
                       quote=TRUE,
                       packages = read_system_file("core.txt")) {

    context <- create_tracer(packages)

    if (!is(context, "instrumentr_context")) {
        stop("context is not a valid instrumentr context")
    }

    message("*** keep.source: ", getOption("keep.source"))

    if (typeof(packages) != "character") {
        stop("expected a character vector of package names for argument 'package'")
    }

    if (quote) {
        code <- substitute(code)
    }

    result <- instrumentr::trace_code(context, code, envir, quote = FALSE)
    data <- instrumentr::get_data(context)
    list(result = result, tables = data$tables)
}

read_system_file <- function(filename) {
    readLines(system.file("extdata", filename, package = "evil"))
}
