
#' @export
#' @importFrom lightr trace_code
#' @importFrom utils write.csv
trace_eval <- function(code, envir=parent.frame(), datafile) {
    code <- substitute(code)
    context <- create_evil_context()
    result <- trace_code(code, context, envir)
    eval_data <- get_data(context)
    write.csv(eval_data, datafile, row.names = FALSE)
    result
}
