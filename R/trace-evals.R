#' @export
#' @importFrom instrumentr create_context
eval_tracer <- function() {
    functions <- c(
        "base::eval",
        "base::evalq",
        "base::eval.parent",
        "base::local",

        "base::parse",
        "base::str2expression",
        "base::str2lang"
    )

    create_context(
        call_entry_callback = trace_eval_entry_callback,
        call_exit_callback = trace_eval_callback,
        builtin_call_entry_callback = .Call(evil:::C_get_builtin_call_entry_callback),
        special_call_entry_callback = .Call(evil:::C_get_special_call_entry_callback),
        closure_call_entry_callback = .Call(evil:::C_get_closure_call_entry_callback),
        eval_entry_callback = .Call(evil:::C_get_eval_entry_callback),
        gc_allocation_callback = .Call(evil:::C_get_gc_allocation_callback),
        variable_definition_callback = .Call(evil:::C_get_variable_definition_callback),
        variable_assignment_callback = .Call(evil:::C_get_variable_assignment_callback),
        variable_removal_callback = .Call(evil:::C_get_variable_removal_callback),
        variable_lookup_callback = .Call(evil:::C_get_variable_lookup_callback),
        functions = functions
    )
}

create_counters <- function(call_id, eval_env) {
    list(call_id = call_id,

         eval_env = eval_env,

         direct_builtin = 0,
         indirect_builtin = 0,

         direct_special = 0,
         indirect_special = 0,

         direct_closure = 0,
         indirect_closure = 0,

         direct_interpreter_eval = 0,
         indirect_interpreter_eval = 0,

         direct_c_call = 0,
         indirect_c_call = 0,

         direct_allocation = 0,
         indirect_allocation = 0,

         direct_writes = 0,
         indirect_writes = 0)
}

push_counters <- function(context_data, call_id, eval_env) {
    context_data$counters[[length(context_data$counters) + 1]] <- create_counters(call_id, eval_env)
}

pop_counters <- function(context_data) {
    counters <- context_data$counters[[length(context_data$counters)]]
    context_data$counters[[length(context_data$counters)]] <- NULL
    counters
}

trace_eval_entry_callback <- function(context, application, package, func, call) {

    call_name <- get_name(func)

    if(!(call_name %in% c("eval", "evalq"))) {
        return()
    }

    eval_call_env <- get_environment(call)

    eval_env <- get("envir", envir = eval_call_env)

    push_counters(get_data(context), get_id(call), eval_env)


## NOTE: logic for computing eval.parent environments
###n <- get("n", envir = eval_call_env)
###parents <- sys.parents()
###parent_index <- eval_call_frame_position
###while(n > 0 && parent_index != 0) {
###    parent_index <- parents[parent_index]
###    n <- n - 1
###}
###if (parent_index == 0) {
###    globalenv()
###} else {
###    sys.frames()[[parent_index]]
###}
}


#' @importFrom instrumentr get_data set_data get_id get_name get_parameters
#' @importFrom instrumentr get_arguments get_position get_expression
#' @importFrom instrumentr is_evaluated get_frame_position get_environment
#' @importFrom instrumentr get_caller is_successful
#' @importFrom digest sha1
#' @importFrom purrr detect_index discard map_chr
trace_eval_callback <- function(context, application, package, func, call) {
    call_name <- get_name(func)
    if (call_name %in% c("parse", "str2expression", "str2lang")) {
        expression <- get_expression(call)
        retval <- returnValue()
        mark_parsed_expression(retval, expression)
        return()
    }

    eval_call_id <- get_id(call)
    eval_function <- get_name(func)
    eval_call_env <- get_environment(call)
    eval_call_expression <- get_expression(call)
    eval_call_srcref <- get_call_srcref(eval_call_expression)
    eval_call_frame_position <- get_frame_position(call)

    application_frame_position <- get_frame_position(application)

    ## eval, evalq and local use `envir` parameter name to denote environment
    ## eval.parent uses `p` to denote evaluation environment
    envir_name <- if (eval_function == "eval.parent") "p" else "envir"

    eval_env <- get(envir_name, envir = eval_call_env)
    environment_class <- NA
                                        # TODO resolve environments if it is an integer (sys.call)
    if (is.environment(eval_env)) {
        environment_class <- classify_environment(
            application_frame_position,
            eval_call_frame_position,
            eval_call_env,
            eval_env
        )
    }
    enclos_env <- eval_call_env$enclos

    caller <- get_caller(call)
    caller_expression <- caller$call_expression
    caller_package <- caller$package_name
    caller_function <- caller$function_name
    caller_srcref <- get_call_srcref(caller_expression)
    
    envir_from_arg <- NA
    if(is.environment(eval_env))
    {
        args_caller <- names(formals(caller$definition))
        for(arg_caller in args_caller)
        {
            arg_val <- NULL
            arg_val <- try(get0(arg_caller, envir = caller$environment), silent = TRUE)
            # if a caller arg is a parent  of envir 
            # (which would mean it was built with new.env probably )
            # or is equal to envir
            # We could check if it's a parent of a parent but I don;t think it's probable... (TODO though)
            if(!is.null(arg_val) && is.environment(arg_val) && 
               (identical(arg_val, eval_env) || identical(arg_val, parent.env(eval_env))))
            {
                envir_from_arg <- arg_caller
            }
        }
    }
    


                                        # drop the first one - the call to this function
    caller_stack_expression <- NA
    caller_stack_expression_raw <- NA
    caller_stack_expression_srcref <- NA

    caller_stack <- rev(sys.calls())[-1]

    ## FIXME: this is wrong, cuts too much
    ## caller_stack_instrumentr_idx <- purrr::detect_index(
    ##   caller_stack,
    ##   ~is.symbol(.[[1]]) && .[[1]] == "trace_code.instrumentr_context"
    ## )

    ## if (caller_stack_instrumentr_idx > 1) {
    ##   caller_stack <- caller_stack[1:caller_stack_instrumentr_idx-1]
    ## }

    caller_stack <- purrr::discard(
                               caller_stack,
                               ~ is.symbol(.[[1]]) && as.character(.[[1]]) %in% c(
                                                                                    "tryCatch",
                                                                                    "tryCatchList",
                                                                                    "tryCatchOne",
                                                                                    "doTryCatch",
                                                                                    "doWithOneRestart",
                                                                                    "withRestarts",
                                                                                    "withOneRestart",
                                                                                    "withCallingHandlers"
                                                                                )
                           )

    caller_stack_expression <- paste(
        map_chr(caller_stack, ~expr_to_string(., max_length=80, one_line=TRUE)),
        collapse="\n"
    )
    caller_stack_expression_raw <- paste(
        map_chr(caller_stack, ~expr_to_string(., max_length=80, raw=TRUE, one_line=TRUE)),
        collapse="\n"
    )
    caller_stack_expression_srcref <- paste(
        map_chr(caller_stack, get_call_srcref),
        collapse="\n"
    )
    ## }

                                        #browser()

                                        # eval: expr, envir, enclos
                                        # evalq: expr, envir, enclos
                                        # eval.parent: expr, n
                                        # local: expr, envir
    params <- get_parameters(call)
    names(params) <- map_chr(params, get_name)

                                        # all evals define expr
    arg <- get_arguments(params$expr)[[1]]
    expr_expression <- get_expression(arg)
    expr_forced <- is_evaluated(arg)
    expr_resolved <- if (expr_forced) {
                         expr_resolved <- resolve_expr(eval_call_env$expr, eval_env)
                     } else {
                         .Empty
                     }

    expr_parsed_expression <- attr(expr_resolved, "._evil_parsed_expression")

    ##browser(expr=eval_function==caller_function && !is.null(expr_parsed_expression))

    if (is.null(expr_parsed_expression)) {
        expr_parsed_expression <- .Empty
    }

    envir_expression <- .Empty
    envir_forced <- NA
    enclos_expression <- .Empty
    enclos_forced <- NA

    if (eval_function == "eval.parent") {
        arg <- get_arguments(params$n)[[1]]
        expr <- get_expression(arg)

        envir_forced <- is_evaluated(arg)
        if(!identical(expr, .DefaultArgs[[eval_function]]$n)) {
            envir_expression <- substitute(parent.frame(1 + N), list(N=expr))
        }
        envir_default <- eval_call_env$n == 1
    } else {
        arg <- get_arguments(params$envir)[[1]]
        expr <- get_expression(arg)

        envir_forced <- is_evaluated(arg)
        if (!identical(expr, .DefaultArgs[[eval_function]]$envir)) {
            envir_expression <- expr
        }
                                        # TODO: check if the given environment is the same as the default one
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

                                        # TODO: move to S3
    expr_repr <- function(e) {
        if (!is_empty(e)) {
            s <- expr_to_string(e)

            list(
                text=if (is.language(e)) s else NA,
                hash=sha1(s),
                length=nchar(s),
                type=sexp_typeof(e),
                nodes=get_ast_size(substitute(quote(e)))
            )
        } else {
            list(text=NA, hash=NA, length=NA, type=NA, nodes=NA)
        }
    }

    expr_expression_repr <- expr_repr(expr_expression)
    expr_resolved_repr <- expr_repr(expr_resolved)

    expr_expression_function <- NA
    expr_expression_args_num <- NA

    if (is.call(expr_expression)) {
        expr_expression_function <- expr_to_string(expr_expression[[1]])
        expr_expression_args_num <- length(expr_expression) - 1
    }

    expr_resolved_function <- NA
    expr_resolved_args_num <- NA

    if (is.call(expr_resolved)) {
        expr_resolved_function <- expr_to_string(expr_resolved[[1]])
        expr_resolved_args_num <- length(expr_resolved) - 1
    }

    counters <- if (call_name == "eval" || call_name == "evalq") {
                    pop_counters(get_data(context))
                } else {
                    create_counters(eval_call_id, NULL)
                }

    trace <- data.frame(
        eval_call_id,
        eval_function=call_name,
        eval_call_expression=expr_to_string(eval_call_expression),
        eval_call_srcref,
        caller_package,
        caller_function,
        caller_expression=expr_to_string(caller_expression),
        caller_srcref,
        caller_stack_expression,
        caller_stack_expression_raw,
        caller_stack_expression_srcref,
        environment_class,
        successful=is_successful(call),

        expr_expression        = expr_expression_repr$text,
        expr_expression_hash   = expr_expression_repr$hash,
        expr_expression_length = expr_expression_repr$length,
        expr_expression_type   = expr_expression_repr$type,
        expr_expression_nodes   = expr_expression_repr$nodes,
        expr_expression_function,
        expr_expression_args_num,

        expr_resolved        = expr_resolved_repr$text,
        expr_resolved_hash   = expr_resolved_repr$hash,
        expr_resolved_length = expr_resolved_repr$length,
        expr_resolved_type   = expr_resolved_repr$type,
        expr_resolved_nodes   = expr_resolved_repr$nodes,
        expr_resolved_function,
        expr_resolved_args_num,

        expr_parsed_expression = expr_to_string(expr_parsed_expression),
        expr_forced,

        envir_expression=expr_to_string(envir_expression),
        envir_forced,
        envir_type=sexp_typeof(eval_env),
        envir_from_arg,

        enclos_expression=expr_to_string(enclos_expression),
        enclos_forced,
        enclos_type=sexp_typeof(enclos_env),

        direct_builtin = counters$direct_builtin,
        indirect_builtin = counters$indirect_builtin,

        direct_special = counters$direct_special,
        indirect_special = counters$indirect_special,

        direct_closure = counters$direct_closure,
        indirect_closure = counters$indirect_closure,

        direct_interpreter_eval = counters$direct_interpreter_eval,
        indirect_interpreter_eval = counters$indirect_interpreter_eval,

        direct_c_call = counters$direct_c_call,
        indirect_c_call = counters$indirect_c_call,

        direct_allocation = counters$direct_allocation,
        indirect_allocation = counters$indirect_allocation,

        direct_writes = counters$direct_writes,
        indirect_writes = counters$indirect_writes
    )

    assign(as.character(get_id(arg)), trace, envir=get_data(context)$calls)
}

#' @export
trace_eval <- function(code, ...) {
    trace_code(context=eval_tracer(), code=code, ...)
}
