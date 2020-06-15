
#' @export
#' @importFrom instrumentr trace_code
#' @importFrom utils write.csv
trace_eval <- function(code,
                       envir = parent.frame(),
                       quote = TRUE) {
    if (quote) {
        code <- substitute(code)
    }

    context <- create_evil_context()

    result <- trace_code(code, context, envir, quote = FALSE)

    data <- get_data(context)

    list(result = result, data = data)
}


#' @export
#' @importFrom instrumentr is_error get_error get_source get_message get_call
#' @importFrom utils write.csv
write_eval_traces <- function(trace, datadir = file.path(getwd(), ".evil")) {
    ## create datadir
    dir.create(datadir, showWarnings = FALSE)

    ## write eval data
    data_file_path <- file.path(datadir, "arguments.csv")
    write.csv(trace$data, data_file_path, row.names = FALSE)

    ## handle error
    if (is_error(trace$result)) {
        status_file <- file.path(datadir, "ERROR")
        error <- get_error(trace$result)
        error_data <- data.frame(source = get_source(error),
                                 message = get_message(error),
                                 call = paste(deparse(get_call(error)), sep = "\n"))
        write.csv(error_data, status_file, row.names = FALSE)
    }

    trace
}
