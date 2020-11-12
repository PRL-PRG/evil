#' @export
#' @importFrom instrumentr set_application_load_callback set_application_unload_callback
#' @importFrom instrumentr set_data get_data trace_code
trace_code <- function(context, code, envir=parent.frame(), quote=TRUE) {
    if (!is(context, "instrumentr_context")) {
        stop("context is not a valid instrumentr context")
    }

    message("*** keep.source: ", getOption("keep.source"))

    if (quote) {
        code <- substitute(code)
    }

    set_application_load_callback(context, function(context, application) {
        data <- new.env(parent = emptyenv())
        data$counters <- list()
        data$calls <- new.env(parent = emptyenv())
        ## NOTE: this set of counters is the global count of all operations.
        ##       new entries on top of this will be specific to eval calls.
        push_counters(data, get_id(application), get_environment(application))
        set_data(context, data)
    })

    set_application_unload_callback(context, function(context, application) {
        data <- get_data(context)
        data$calls <- do.call(rbind, as.list(data$calls))
        ## at this point, there should be only one frame in the counter stack
        ## because other frames are popped out as evals exit.
        counters <- data$counters[[1]]
        data$counters <- NULL
        counters$call_id <- NULL
        counters$eval_env <- NULL
        data$program <- as.data.frame(counters)
    })

    result <- instrumentr::trace_code(context, code, envir, quote = FALSE)

    list(result = result, data = instrumentr::get_data(context))
}

#' @export
trace_file <- function(context, file) {
                                        # TODO - one has to create a new file, wrap and run
                                        # in an external process
    code <- parse(file, keep.source=TRUE)
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
    save_traces(traces, file.path(datadir, "calls.fst"), file.path(datadir, "program.fst"))
}

#' @export
#' @importFrom instrumentr is_error get_error get_source get_message get_call
#' @importFrom fst write_fst
save_traces <- function(traces, calls_file, program_file) {
    if (is.null(traces$data$calls) && !is_error(traces$result)) {
        return(traces)
    }

    calls_dir <- dirname(calls_file)

    dir.create(calls_dir, showWarnings = FALSE, recursive=TRUE)

    program_dir <- dirname(program_file)
    dir.create(program_dir, showWarnings = FALSE, recursive=TRUE)

    ## if trace$data$calls is a data frame, then we assume
    ## that traces$data$program is also a valid data frame
    if (is.data.frame(traces$data$calls)) {
        fst::write_fst(traces$data$calls, calls_file, compress=100)
        fst::write_fst(traces$data$program, program_file, compress=100)
                                        #write_table(traces$data, file)
    }

    error_df <- if (is_error(traces$result)) {
                    e <- get_error(traces$result)
                    data.frame(
                        source = get_source(e),
                        message = get_message(e),
                        call = paste(deparse(get_call(e)), sep = "\n")
                    )
                } else if (!is.data.frame(traces$data$calls)) {
                    data.frame(
                        source=NA,
                        message=paste("exprected traces data to be data.frame, found ", sexp_typeof(traces$data$calls)),
                        call=NA
                    )
                } else {
                    NULL
                }

    if (!is.null(error_df)) {
        error_file_name <- paste0(tools::file_path_sans_ext(calls_file), "-ERROR")
        fst::write_fst(error_df, error_file_name, compress=100)
                                        #write_table(error_df, error_file_name)
    }

    invisible(traces)
}
