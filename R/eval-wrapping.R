.EvalFunctions <- c("eval", "evalq", "eval.parent", "local")
.NsGetFunctions <- c("::", ":::")

create_csid_prefix <- function(package_name, fun_name) {
    paste0("::", package_name, "::", fun_name, "::")
}

#' @export
setup_eval_wrapping_hook_from_file <- function(file) {
  setup_eval_wrapping_hook(parse_evals_to_trace(readLines(file)))
}

#' @param evals_to_trace a data frame with package and fun. If fun is NA, all
#'   functions from the corresponding package will be wrapped
#' @export
setup_eval_wrapping_hook <- function(evals_to_trace) {
    if (is.null(evals_to_trace)) {
        return(NULL)
    }

    if (is.character(evals_to_trace)) {
      evals_to_trace <- data.frame(package=evals_to_trace, fun=NA)
    }

    if (!is.data.frame(evals_to_trace) || nrow(evals_to_trace) == 0) {
      return(NULL)
    }

    traced_packages <- unique(evals_to_trace$package)

    handle_package <- function(package_name, ...) {
        #make sure we disable provenance tracing for that
        in_r_tracing(FALSE)
        on.exit(in_r_tracing(TRUE))
        package_env <- getNamespace(package_name)

        entry <- subset(evals_to_trace, package == package_name)
        funs_names <- if (any(is.na(entry))) {
                          ls(envir=package_env, all.names=TRUE)
                      } else {
                          entry$fun
                      }
        
        wrapped <- 0
        for (fun_name in funs_names) {
            fun <- get0(fun_name, envir=package_env, mode="function")
            if (can_be_wrapped(fun)) {
                ## cat("*** wrapping function", package_name, "::", fun_name, "\n")
                wrap_function_evals(fun, create_csid_prefix(package_name, fun_name))
                wrapped <- wrapped + 1
            }
        }

        ## cat("Wrapped", wrapped, "/", length(funs_names), "functions from", package_name, "\n")

        setHook(packageEvent(package_name, "onLoad"), NULL, "replace")
    }

    remove_packages <- c("tools:callr", "tools:rstudio", "instrumentr")
    loaded_packages <- setdiff(loadedNamespaces(), remove_packages)

    for (package in intersect(traced_packages, loaded_packages)) {
        handle_package(package)
    }

    for (package in setdiff(traced_packages, loaded_packages)) {
        setHook(packageEvent(package, "onLoad"), handle_package)
    }
    in_r_tracing(FALSE)
}

can_be_wrapped <- function(f) {
    is.function(f) && !is.primitive(f)
}

wrap_function_evals <- function(fun, csid_prefix) {
    body <- wrap_evals(body(fun), csid_prefix)
    injectr:::reassign_function_body(fun, body)
}

wrap_evals <- function(expr, csid_prefix) {
  id <- 1L

  wrap <- function(expr) {
    if (typeof(expr) == "language") {
      if (is.call(expr)) {
        fun <- expr[[1L]]
        if (length(fun) == 3L && length(fun[[1L]]) == 1L && as.character(fun[[1L]]) %in% .NsGetFunctions) {
          fun_str <- as.character(fun[-1L])
          if (fun_str[1L] == "base" && fun_str[2L] %in% .EvalFunctions) {
            csid <- paste0(csid_prefix, id)
            attr(expr, "csid") <- csid
            id <<- id + 1L
          }
        } else if (length(fun) == 1L && as.character(fun) %in% .EvalFunctions) {
          csid <- paste0(csid_prefix, id)
          attr(expr, "csid") <- csid
          id <<- id + 1L
        } else {
          for (i in seq_along(expr)) {
            if (typeof(expr[[i]]) == "language") {
              expr[[i]] <- wrap(expr[[i]])
            }
          }
        }
      }
    }
    expr
  }

  wrap(expr)
}
