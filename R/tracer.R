
#' @importFrom instrumentr create_context set_data
create_tracer <- function(evals_to_trace) {
    functions <- c(
        "base::eval",
        "base::evalq",
        "base::eval.parent",
        "base::local",

        "base::match.call"
    )

    context <- create_context(
        application_load_callback = application_load_callback,
        application_attach_callback = application_attach_callback,
        application_unload_callback = application_unload_callback,
        call_entry_callback = call_entry_callback,
        call_exit_callback = call_exit_callback,
        # builtin_call_entry_callback = .Call(C_get_builtin_call_entry_callback),
        # special_call_entry_callback = .Call(C_get_special_call_entry_callback),
        closure_call_entry_callback = .Call(C_get_closure_call_entry_callback),
        closure_call_exit_callback = .Call(C_get_closure_call_exit_callback),
        context_entry_callback = .Call(C_get_context_entry_callback),
        context_exit_callback = .Call(C_get_context_exit_callback),
        context_jump_callback = .Call(C_get_context_jump_callback),
        eval_entry_callback = .Call(C_get_eval_entry_callback),
        variable_definition_callback = .Call(C_get_variable_definition_callback),
        variable_assignment_callback = .Call(C_get_variable_assignment_callback),
        variable_removal_callback = .Call(C_get_variable_removal_callback),
        variable_lookup_callback = .Call(C_get_variable_lookup_callback),
        gc_allocation_callback = .Call(C_get_gc_allocation_callback),
        gc_unmark_callback = .Call(C_get_gc_unmark_callback),
        functions = functions
    )

    data <- new.env(parent = emptyenv())
    data$evals_to_trace <- evals_to_trace
    .Call(C_tracer_data_initialize, data)
    data$calls <- new.env(parent = emptyenv())
    data$match.call <- new.env(parent = emptyenv())
    data$unique_resolved_expressions <- new.env(parent = emptyenv())
    set_data(context, data)

    context
}


#' @importFrom instrumentr get_variable_definition_callback
#' @importFrom instrumentr get_variable_assignment_callback
#' @importFrom instrumentr get_variable_removal_callback
#' @importFrom instrumentr get_variable_lookup_callback
#' @importFrom instrumentr activate deactivate reinstate
set_variable_callback_status <- function(context, status) {
  return()

  fun <- if (status == "activate") {
    activate
  } else if (status == "deactivate") {
    deactivate
  } else if (status == "reinstate") {
    reinstate
  } else {
    stop("invalid callback status")
  }

  fun(get_variable_definition_callback(context))
  fun(get_variable_assignment_callback(context))
  fun(get_variable_removal_callback(context))
  fun(get_variable_lookup_callback(context))
}

#' @importFrom instrumentr get_data get_environment
#' @importFrom instrumentr get_id get_frame_position
application_load_callback <- function(context, application) {

    ## NOTE: this set of counters is the global count of all operations.
    ##       new entries on top of this will be specific to eval calls.
    data <- get_data(context)
    set_variable_callback_status(context, "deactivate")

    .state$tracer_data_add_package <- function(pkgname, libname) {
        .Call(C_tracer_data_add_package, data, pkgname)
    }

    installed_packages <- unname(installed.packages()[,1])
    for(package in installed_packages) {
        setHook(packageEvent(package, "onLoad"), .state$tracer_data_add_package, "append")
    }

    # taint results from parsing functions
    parse_funs_names <- c("parse", "str2expression", "str2lang")
    data$parse_funs <- lapply(parse_funs_names, function(name) {
      fun <- get(name, envir=baseenv(), mode="function")
      fun_dup <- injectr::create_duplicate(fun)
      code <- substitute(evil:::mark_parsed_expression(returnValue(), deparse1(match.call())))
      injectr::inject_code(code=code, fun=fun, where="onexit")
      fun_dup
    })
    names(data$parse_funs) <- parse_funs_names
}


#' @importFrom instrumentr get_data
application_attach_callback <- function(context, application) {

    ## NOTE: initialize function table with functions from
    ## all package at this point because instrumented function
    ## objects have been modified by instrumenter now.
    data <- get_data(context)
    .Call(C_function_table_initialize, data)
}


#' @importFrom instrumentr get_data
application_unload_callback <- function(context, application) {
    set_variable_callback_status(context, "reinstate")

    data <- get_data(context)
    parse_funs <- data$parse_funs
    for (name in names(parse_funs)) {
      fun = get(name, envir=baseenv(), mode="function")
      fun_orig = parse_funs[[name]]
      injectr:::reassign_function_body(fun, body(fun_orig))
    }

    calls <- do.call(rbind, as.list(data$calls))


    ## NOTE: remove package load hooks
    installed_packages <- unname(installed.packages()[,1])
    for(package in installed_packages) {
        setHook(packageEvent(package, "onLoad"), NULL, "append")
    }

    ## NOTE: if there are no eval calls, create an empty data frame
    ## with correct number of and type of columns
    if (is.null(calls)) {
        calls <- create_call_row()
    }

    data$calls <- NULL

    expr_df <- if (length(data$unique_resolved_expressions) != 0) {
        exprs <- as.list(data$unique_resolved_expressions)
        exprs <- sapply(exprs, function(x) {
          if (is.null(x)) "NULL"
          else if (!is.character(x)) paste("INVALID:", typeof(x))
          else x
        })

        data.frame(
          expr_resolved_hash=names(exprs),
          expr_resolved=exprs
        )
    } else {
        data.frame(expr_resolved_hash = character(0), expr_resolved = character(0))
    }

    data$unique_resolved_expressions <- NULL

    dependencies <- data.frame(package = loadedNamespaces())

    tables <- .Call(C_tracer_data_finalize, data)

    tables$code <- merge(tables$code, calls, by = "eval_call_id")

    tables$reflection <- merge(tables$reflection, calls, by = "eval_call_id")

    data$tables <- c(list(dependencies = dependencies, calls = calls, resolved_expressions = expr_df), tables)
}

#' @importFrom instrumentr get_frame_position get_name get_caller
#' @importFrom instrumentr get_data get_environment get_id
call_entry_callback <- function(context, application, package, func, call) {
  call_name <- get_name(func)

  if (!(call_name %in% c("eval", "evalq"))) {
    return()
  }

  ## ignore eval if coming from a package outside of package list
  caller <- get_caller(call)
  caller_package <- caller$package_name
  data <- get_data(context)

  if (!should_trace(caller_package, data$evals_to_trace)) {
    return()
  }

  eval_call_env <- get_environment(call)

  ## WARN: The way eval works, it will first force this argument and then eval
  ## the result. To make sure we only capture events from evaluation of the
  ## result, we force the argument here manually.

  force(eval_call_env$expr)

  eval_env <- get("envir", envir = eval_call_env)

  ## NOTE: this is done to force enclose env
  enclose_env <- get("enclos", envir = eval_call_env)

  eval_frame_depth <- get_frame_position(call)

  .Call(C_tracer_data_eval_call_entry, get_data(context), get_id(call), eval_env, eval_frame_depth)

  set_variable_callback_status(context, "activate")
}

#' @importFrom instrumentr get_data set_data get_id get_name get_parameters
#' @importFrom instrumentr get_arguments get_position get_expression
#' @importFrom instrumentr is_evaluated get_frame_position get_environment
#' @importFrom instrumentr get_caller is_successful
#' @importFrom digest sha1
call_exit_callback <- function(context, application, package, func, call) {
    set_variable_callback_status(context, "reinstate")

    call_name <- get_name(func)
    data <- get_data(context)

    if (call_name == "match.call") {
        retval <- returnValue()
        # data$match.call is rather used as a set than a hashmap
        for (k in seq_along(retval)) { # cannot directly iterate a call list
          rv <- retval[[k]]
          if (!is.null(rv)) {
            data$match.call[[injectr::sexp_address(rv)]] <- TRUE
          }
        }
        return()
    }

    caller <- get_caller(call)
    caller_def_srcdir <- getSrcDirectory(caller$definition)
    caller_def_srcfile <- getSrcFilename(caller$definition)
    caller_package <- caller$package_name

    # Hardcode R6
    # eval in this case originated in a class rather than from
    # a package so the caller_package is set to the class name
    # instead. This concrete one comes from:
    #
    # generator_funs$get_inherit <- function() {
    #   The NULL arg speeds up eval a tiny bit
    #   eval(inherit, parent_env, NULL)
    # }
    #
    # from: R6/R/generator_funs.R
    #
    if (length(caller_def_srcdir) == 1 && length(caller_def_srcfile) == 1) {
      if (endsWith(caller_def_srcdir, "/R6/R") && (caller_def_srcfile == "generator_funs.R")) {
        caller_package <- "R6"
      }
    }

    if (!should_trace(caller_package, data$evals_to_trace)) {
      return()
    }

    eval_call_id <- get_id(call)
    eval_function <- get_name(func)
    eval_call_env <- get_environment(call)
    eval_call_expression <- get_expression(call)
    eval_call_srcref <- {
        csid <- attr(eval_call_expression, "csid")
        if (!is.null(csid)) {
            csid
        } else {
            get_call_srcref(eval_call_expression)
        }
    }
    eval_call_frame_position <- get_frame_position(call)

    caller_expression <- caller$call_expression
    caller_function <- caller$function_name
    caller_srcref <- get_call_srcref(caller_expression)

    application_frame_position <- get_frame_position(application)

    interp_eval <- if ((call_name %in% c("eval", "evalq"))) {
        .Call(C_tracer_data_eval_call_exit, data)
    } else {
        NA_integer_
    }

    ## eval, evalq and local use `envir` parameter name to denote environment
    ## eval.parent uses `p` to denote evaluation environment
    envir_name <- if (eval_function == "eval.parent") "p" else "envir"

    eval_env <- get(envir_name, envir = eval_call_env)
    environment_class <- NA_character_
    # TODO resolve environments if it is an integer (sys.call)
    if (is.environment(eval_env)) {
      environment_class <- tryCatch({
        classify_environment(
            eval_call_frame_position,
            eval_call_env,
            eval_env
        )
      }, error=function(e) "error")
    }
    enclos_env <- eval_call_env$enclos

    enclosure_class <- NA_character_
    if(sexp_typeof(eval_env) %in% c("list", "NULL", "pairlist") && is.environment(enclos_env)) {
      enclosure_class <- tryCatch({
        classify_environment(
          eval_call_frame_position,
          eval_call_env,
          enclos_env
        )
      }, error=function(e) "error")
    }

    # eval: expr, envir, enclos
    # evalq: expr, envir, enclos
    # eval.parent: expr, n
    # local: expr, envir
    params <- get_parameters(call)
    names(params) <- unlist(unname(Map(get_name, params)))

    # all evals define expr
    arg <- get_arguments(params$expr)[[1]]
    expr_expression <- get_expression(arg)
    expr_forced <- is_evaluated(arg)
    expr_resolved <- if (expr_forced) {
        expr_resolved <- eval_call_env$expr
    } else {
        .Empty
    }

    # Argument used parse/str2lanf/str2expression?
    expr_parsed_expression <- attr(expr_resolved, "._evil_parsed_expression")
    ## browser(expr=eval_function==caller_function && !is.null(expr_parsed_expression))
    if (is.null(expr_parsed_expression)) {
        expr_parsed_expression <- .Empty
    }

    # Argument results from a match.call?
    expr_match_call <- from_match.call(expr_resolved, data$match.call)

    envir_expression <- .Empty
    envir_forced <- NA
    enclos_expression <- .Empty
    enclos_forced <- NA

    if (eval_function == "eval.parent") {
        arg <- get_arguments(params$n)[[1]]
        expr <- get_expression(arg)

        envir_forced <- is_evaluated(arg)
        if (!identical(expr, .DefaultArgs[[eval_function]]$n)) {
            envir_expression <- substitute(parent.frame(1 + N), list(N = expr))
        }
        envir_default <- eval_call_env$n == 1
    } else {
        arg <- get_arguments(params$envir)[[1]]
        expr <- get_expression(arg)

        envir_forced <- is_evaluated(arg)
        if (identical(expr, .DefaultArgs[[eval_function]]$envir) &&
              identical(sys.frame(-2), eval_call_env$envir)) {
          envir_expression <- NA
        } else {
          envir_expression <- expr
        }
    }

    if (eval_function %in% c("eval", "evalq")) {
        arg <- get_arguments(params$enclos)[[1]]
        expr <- get_expression(arg)

        enclos_forced <- is_evaluated(arg)
        if (!identical(expr, .DefaultArgs[[call_name]]$enclos)) {
            enclos_expression <- expr
        }
        # TODO: check if the given environment is the same as the default one
    }

    expr_return <- returnValue()


    # TODO: move to S3
    expr_repr <- function(e) {
        if (!is_empty(e)) {
            s <- expr_to_string(e)
            full_text <- if (is.language(e)) s else NA_character_

            list(
                fulltext = full_text,
                text = strtrim(full_text, 360),
                hash = sha1(full_text),
                length = nchar(full_text),
                type = sexp_typeof(e),
                tag = sexp_typeof(e, tag = TRUE)
            )
        } else {
            list(
                text = NA_character_,
                hash = NA_character_,
                length = NA_integer_,
                type = NA_character_,
                tag = NA_character_
            )
        }
    }

    expr_expression_repr <- expr_repr(expr_expression)
    expr_resolved_repr <- expr_repr(expr_resolved)

    expr_expression_function <- NA_character_
    expr_expression_args_num <- NA_integer_

    if (is.call(expr_expression)) {
        expr_expression_function <- expr_to_string(expr_expression[[1]])
        expr_expression_args_num <- as.integer(length(expr_expression) - 1)
    }

    expr_resolved_function <- NA_character_
    expr_resolved_args_num <- NA_integer_

    if (is.call(expr_resolved)) {
        expr_resolved_function <- expr_to_string(expr_resolved[[1]])
        expr_resolved_args_num <- length(expr_resolved) - 1
    }

    if (!exists(expr_resolved_repr$hash, where = data$unique_resolved_expressions)) {
      data$unique_resolved_expressions[[expr_resolved_repr$hash]] <- expr_resolved_repr$fulltext
    }

    expr_return_repr <- expr_repr(expr_return)

    trace <- create_call_row(
        eval_call_id,
        eval_function = call_name,
        eval_call_expression = expr_to_string(eval_call_expression),
        eval_call_srcref,
        caller_package,
        caller_function,
        caller_expression = expr_to_string(caller_expression),
        caller_srcref,
        environment_class,
        successful = is_successful(call),
        expr_expression = expr_expression_repr$text,
        expr_expression_hash = expr_expression_repr$hash,
        expr_expression_length = expr_expression_repr$length,
        expr_expression_type = expr_expression_repr$type,
        expr_expression_type_tag = expr_expression_repr$tag,
        expr_expression_nodes = get_ast_size(substitute(expr_expression)),
        expr_expression_function,
        expr_expression_args_num,

        expr_resolved = expr_resolved_repr$text,
        expr_resolved_hash = expr_resolved_repr$hash,
        expr_resolved_length = expr_resolved_repr$length,
        expr_resolved_type = expr_resolved_repr$type,
        expr_resolved_type_tag = expr_resolved_repr$tag,
        expr_resolved_nodes = get_ast_size(substitute(expr_resolved)),
        expr_resolved_function,
        expr_resolved_args_num = as.integer(expr_resolved_args_num),

        expr_return = expr_return_repr$text,
        expr_return_hash = expr_return_repr$hash,
        expr_return_length = expr_return_repr$length,
        expr_return_type = expr_return_repr$type,
        expr_return_type_tag = expr_return_repr$tag,

        expr_parsed_expression = expr_to_string(expr_parsed_expression),
        expr_forced,

        envir_expression = expr_to_string(envir_expression),
        envir_forced,
        envir_type = sexp_typeof(eval_env),

        expr_match_call,

        enclos_expression = expr_to_string(enclos_expression),
        enclos_forced,
        enclos_type = sexp_typeof(enclos_env),
        enclosure_class = enclosure_class,
        interp_eval = interp_eval
    )

    assign(as.character(get_id(arg)), trace, envir = get_data(context)$calls)
}

should_trace <- function(caller_package, evals_to_trace) {
  switch(
    evals_to_trace,
    all=TRUE,
    base={
      caller_package %in% .base_packages
    },
    global={
      caller_package == "global"
    },
    packages={
      caller_package != "global" && !(caller_package %in% .base_packages)
    }
  )
}
