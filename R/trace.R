#' @export
trace_eval <- function(code, ...) {
    trace_code(context=eval_tracer(), code=code, ...)
}

#' @export
#' @importFrom methods is
#' @importFrom instrumentr set_application_load_callback set_application_unload_callback
#' @importFrom instrumentr set_data get_data trace_code get_frame_position
trace_code <- function(context, code, envir=parent.frame(), quote=TRUE, packages) {
    if (!is(context, "instrumentr_context")) {
        stop("context is not a valid instrumentr context")
    }

    message("*** keep.source: ", getOption("keep.source"))

    if (missing(packages)) {
        packages <- readLines(system.file("extdata", "corpus.txt", package="evil"))
    }

    if (typeof(packages) != "character") {
        stop("expected a character vector of package names for argument 'package'")
    }

    if (quote) {
        code <- substitute(code)
    }

    set_application_load_callback(context, function(context, application) {
        data <- new.env(parent = emptyenv())
        data$packages <- packages
        data$counters <- list()
        .Call(C_initialize_tables, data)
        data$calls <- new.env(parent = emptyenv())
        ## NOTE: this set of counters is the global count of all operations.
        ##       new entries on top of this will be specific to eval calls.
        push_counters(data, get_id(application), get_environment(application), get_frame_position(application))
        set_data(context, data)
    })

    set_application_unload_callback(context, function(context, application) {
        data <- get_data(context)
        calls <- do.call(rbind, as.list(data$calls))
        data$calls <- NULL
        ## at this point, there should be only one frame in the counter stack
        ## because other frames are popped out as evals exit.
        counters <- data$counters[[1]]
        data$counters <- NULL
        counters$call_id <- NULL
        counters$eval_env <- NULL
        program <- as.data.frame(counters)

        data$tables <- c(list(calls = calls, program = program),
                         .Call(C_get_tables_as_data_frames, data))
    })

    result <- instrumentr::trace_code(context, code, envir, quote = FALSE)
    data <- instrumentr::get_data(context)
    list(result = result, tables = data$tables)
}

#' @export
#' @importFrom instrumentr is_error get_error get_source get_message get_call
#' @importFrom fst write_fst
#' @importFrom tools file_path_sans_ext
write_traces <- function(traces, data_dir) {

    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

    if (is_error(traces$result)) {
        e <- get_error(traces$result)
        error_df <- data.frame(
            error_source <- get_source(e),
            error_message <- get_message(e),
            error_call <- paste(deparse(get_call(e)), sep = "\n")
        )
        error_file_path <- file.path(data_dir, "ERROR")
        write_fst(error_df, error_file_name, compress=100)
    }


    for(table_name in names(traces$tables)) {
        table <- traces$tables[[table_name]]
        path <- file.path(data_dir, paste0(table_name, ".fst"))

        if(is.data.frame(table)) {
            write_fst(table, path)
        } else {
            error_df <- data.frame(
                source = NA,
                message = paste("expected traces data to be data.frame, found ", sexp_typeof(table)),
                call = NA
            )
            error_file_name <- paste0(file_path_sans_ext(path), "-ERROR")
            write_fst(error_df, error_file_name, compress=100)
        }
    }

    invisible(traces)
}

#' @export
trace_to_file <- function(data_dir, code) {
  write_traces(
    trace_eval(code=code, quote=TRUE),
    data_dir=data_dir
  )
}
