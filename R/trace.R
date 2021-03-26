parse_evals_to_trace <- function(evals_to_trace) {
    if (is.null(evals_to_trace)) {
        return(NULL)
    }

    if (length(evals_to_trace) == 1 && trimws(evals_to_trace, "both") == "") {
        return(NULL)
    }

    evals_to_trace <- strsplit(evals_to_trace, "::", fixed=TRUE)
    evals_to_trace <- lapply(evals_to_trace, function(x) if (length(x) == 1) c(x, NA) else x)
    evals_to_trace <- do.call(rbind, evals_to_trace)
    evals_to_trace <- as.data.frame(evals_to_trace)
    colnames(evals_to_trace) <- c("package", "fun")

    evals_to_trace
}

force_lazy_loaded_functions <- function() {
    packages <- search()

    for(package in packages) {
        if(startsWith(package, "package:")) {
            ns <- getNamespace(substr(package, 9, nchar(package)))
            names <- ls(ns, all.names = TRUE)
            Map(function(name) get(name, envir = ns), names)
        }
    }

    NULL
}

#' @param eval_to_trace :
#' "" or "all" - all evals,
#' "base" - evals from base packages only,
#' "global" - evals from code,
#' "packages" - evals from packages other than base
#'
#' @export
#' @importFrom methods is
#' @importFrom instrumentr set_application_load_callback
#'   set_application_unload_callback
#' @importFrom instrumentr set_data get_data trace_code get_frame_position
trace_code <- function(code,
                       envir = parent.frame(),
                       quote=TRUE,
                       evals_to_trace=c("all", "base", "global", "packages")) {

    force_lazy_loaded_functions()

    evals_to_trace <- trimws(evals_to_trace)
    evals_to_trace <- if (length(evals_to_trace) == 1 && evals_to_trace == "") {
        "all"
    } else {
        match.arg(evals_to_trace)
    }

    context <- create_tracer(evals_to_trace)

    if (!is(context, "instrumentr_context")) {
        stop("context is not a valid instrumentr context")
    }

    if (quote) {
        code <- substitute(code)
    }

    code <- wrap_evals(code, create_csid_prefix("global", "main"))

    result <- instrumentr::trace_code(context, code, envir, quote = FALSE)
    data <- instrumentr::get_data(context)

    require(dplyr)
    data$tables$writes <- left_join(
        data$tables$writes,
        select(data$tables$calls, eval_call_id, eval_call_srcref),
        by=c("eval_id"="eval_call_id")
    )

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

