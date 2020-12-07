
#' @export
#' @importFrom methods is
#' @importFrom instrumentr set_application_load_callback set_application_unload_callback
#' @importFrom instrumentr set_data get_data trace_code get_frame_position
trace_code <- function(code,
                       envir = parent.frame(),
                       quote=TRUE,
                       packages = read_system_file("corpus.txt")) {

    context <- create_tracer(packages)

    if (!is(context, "instrumentr_context")) {
        stop("context is not a valid instrumentr context")
    }

    message("*** keep.source: ", getOption("keep.source"))

    if (!is.null(packages) && !is.character(packages)) {
        stop("expected null or a character vector of package names for argument 'package'")
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


#' @export
#' @importFrom instrumentr is_error get_error get_source get_message get_call
write_trace <- function(traces, writer) {

    if (is_error(traces$result)) {
        e <- get_error(traces$result)

        error_df <- data.frame(
            error_source <- get_source(e),
            error_message <- get_message(e),
            error_call <- paste(deparse(get_call(e)), sep = "\n")
        )

        writer("ERROR", error_df)
    }

    for(table_name in names(traces$tables)) {

        table <- traces$tables[[table_name]]
        filename <- table_name

        if(is.data.frame(table)) {
            writer(filename, table)
        }
        else {
            error_df <- data.frame(
                source = NA,
                message = paste("expected traces data to be data.frame, found ", sexp_typeof(table)),
                call = NA
            )
            filename <- paste0(filename, "-ERROR")
            writer(filename, error_df)
        }
    }

    invisible(NULL)
}

#' @export
trace_to_file <- function(code,
                          envir = parent.frame(),
                          quote = TRUE,
                          packages = read_system_file("corpus.txt"),
                          writer) {

    if (quote) {
        code <- substitute(code)
    }

    traces <- trace_code(code, envir, quote = FALSE, packages)
    write_trace(traces, writer)
}
