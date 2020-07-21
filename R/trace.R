
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

    result <- trace_code(context, code, envir, quote = FALSE)

    data <- get_data(context)

    list(result = result, data = data)
}


#' @export
#' @importFrom instrumentr is_error get_error get_source get_message get_call
#' @importFrom streamr write_table
write_eval_traces <- function(trace, datadir = file.path(getwd(), ".evil")) {
  if (is.null(trace$data) && !is_error(trace$result) ) {
    return(trace)
  }

  dir.create(datadir, showWarnings = FALSE)

  if (is.data.frame(trace$data)) {
    calls_file_path <- file.path(datadir, "calls")
    ## write.csv(trace$data, paste0(calls_file_path, ".csv"), row.names = FALSE)
    saveRDS(trace$data,  paste0(calls_file_path, ".RDS"))
    write_table(trace$data, calls_file_path)
  }

  error_df <- if (is_error(trace$result)) {
    e <- get_error(trace$result)
    data.frame(
      source = get_source(e),
      message = get_message(e),
      call = paste(deparse(get_call(e)), sep = "\n")
    )
  } else if (!is.data.frame()) {
    data.frame(
      source=NA,
      message=paste("exprected trace data to be data.frame, found ", sexp_typeof(trace$data)),
      call=NA
    )
  } else {
    NULL
  }

  if (!is.null(error_df)) {
    error_file_name <- file.path(datadir, "ERROR")
    # write.csv(error_df, patse0(error_file_name, ".csv"), row.names = FALSE)
    saveRDS(error_df,  paste0(error_file_name, ".RDS"))
    write_table(error_df, error_file_name)
  }

  trace
}
