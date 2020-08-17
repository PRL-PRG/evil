#' @export
#' @importFrom instrumentr set_application_load_callback set_application_unload_callback
#' @importFrom instrumentr set_data get_data trace_code
trace_code <- function(context, code, envir=parent.frame(), quote=TRUE) {
  if (!is(context, "instrumentr_context")) {
    stop("context is not a valid instrumentr context")
  }

  if (quote) {
    code <- substitute(code)
  }

  set_application_load_callback(context, function(context, application) {
    data <- new.env(parent = emptyenv())
    set_data(context, data)
  })

  set_application_unload_callback(context, function(context, application) {
    data <- get_data(context)
    new_data <- do.call(rbind, as.list(data))
    set_data(context, new_data)
  })

  result <- instrumentr::trace_code(context, code, envir, quote = FALSE)

  list(result = result, data = instrumentr::get_data(context))
}

#' @export
trace_file <- function(context, file) {
  # TODO - one has to create a new file, wrap and run
  # in an external process
  code <- parse(file)
  trace_code(context, code, quote=FALSE)
}

# TODO: clean up the next two functions

#' @export
trace_to_file <- function(path, context, quote, code) {
  traces <- trace_code(code, context=context, quote=quote)
  write_eval_traces(traces, path)
}

#' @export
write_eval_traces <- function(traces, datadir) {
  save_traces(traces, file.path(datadir, "calls.fst"))
}

#' @export
#' @importFrom instrumentr is_error get_error get_source get_message get_call
#' @importFrom streamr write_table
#' @importFrom fst write_fst
save_traces <- function(traces, file) {
  if (is.null(traces$data) && !is_error(traces$result) ) {
    return(traces)
  }

  dir <- dirname(file)
  dir.create(dir, showWarnings = FALSE)

  if (is.data.frame(traces$data)) {
    fst::write_fst(traces$data, file, compress=100)
    #write_table(traces$data, file)
  }

  error_df <- if (is_error(traces$result)) {
    e <- get_error(traces$result)
    data.frame(
      source = get_source(e),
      message = get_message(e),
      call = paste(deparse(get_call(e)), sep = "\n")
    )
  } else if (!is.data.frame(traces$data)) {
    data.frame(
      source=NA,
      message=paste("exprected traces data to be data.frame, found ", sexp_typeof(traces$data)),
      call=NA
    )
  } else {
    NULL
  }

  if (!is.null(error_df)) {
    error_file_name <- paste0(tools::file_path_sans_ext(file), "-ERROR")
    fst::write_fst(error_df, error_file_name, compress=100)
    #write_table(error_df, error_file_name)
  }

  traces
}
