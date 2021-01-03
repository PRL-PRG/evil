parse_evals_to_trace <- function(evals_to_trace) {
    if (is.null(evals_to_trace) || length(evals_to_trace) == 0) {
        return(NULL)
    }
    if (evals_to_trace == "") {
        stop("evals_to_trace should not be an empty string")
    }

    evals_to_trace <- strsplit(evals_to_trace, "::", fixed=TRUE)
    evals_to_trace <- lapply(evals_to_trace, function(x) if (length(x) == 1) c(x, NA) else x)
    evals_to_trace <- do.call(rbind, evals_to_trace)
    evals_to_trace <- as.data.frame(evals_to_trace)
    colnames(evals_to_trace) <- c("package", "fun")

    evals_to_trace
}

#' @param eval_to_trace - a vector of <package> or <package>::<function> to
#'   trace, or "global" or NULL
#' @export
#' @importFrom methods is
#' @importFrom instrumentr set_application_load_callback
#'   set_application_unload_callback
#' @importFrom instrumentr set_data get_data trace_code get_frame_position
trace_code <- function(code,
                       envir = parent.frame(),
                       quote=TRUE,
                       evals_to_trace=NULL) {

    evals_to_trace <- parse_evals_to_trace(evals_to_trace)
    packages <- NULL

    if (!is.null(evals_to_trace)) {
        packages <- unique(evals_to_trace$package)
        evals_to_trace <- subset(evals_to_trace, package != "global")
    }

    context <- create_tracer(packages)

    if (!is(context, "instrumentr_context")) {
        stop("context is not a valid instrumentr context")
    }

    if (!is.null(packages) && !is.character(packages)) {
        stop("expected null or a character vector of package names for argument 'package'")
    }

    if (quote) {
        code <- substitute(code)
    }

    code <- wrap_evals(code, create_csid_prefix("global", "main"))
    setup_eval_wrapping_hook(evals_to_trace)

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
}
