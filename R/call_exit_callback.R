
#' @importFrom instrumentr get_data set_data get_id get_name get_parameters
#' @importFrom instrumentr get_arguments get_position get_expression
#' @importFrom instrumentr is_evaluated get_frame_position get_environment
#' @importFrom instrumentr get_caller
call_exit_callback <- function(context, application, package, func, call) {

    data <- get_data(context)
    calls <- data$calls
    arguments <- data$arguments

    call_id <- get_id(call)
    call_package <- get_name(package)
    call_name <- get_name(func)
    call_env <- get_environment(call)
    call_expression <- expr_to_string(get_expression(call))

    application_frame_position <- get_frame_position(application)

    call_frame_position <- get_frame_position(call)

    ## eval, evalq and local use `envir` parameter name to denote environment
    ## eval.parent uses `p` to denote evaluation environment
    envir_name <- if (call_name == "eval.parent") "p" else "envir"

    eval_envir <- get(envir_name, envir = call_env)
    environment_class <- classify_environment(application_frame_position,
                                              call_frame_position,
                                              call_env,
                                              eval_envir)

    caller <- get_caller(call)

    caller_expression <- expr_to_string(caller$call_expression)
    caller_package <- caller$package_name
    caller_name <- caller$function_name

    assign(as.character(call_id),
           list(call_id = call_id,
                package_name = call_package,
                function_name = call_name,
                call_expression = call_expression,
                caller_expression = caller_expression,
                caller_package = caller_package,
                caller_name = caller_name,
                environment_class = environment_class),
           calls)

    for (parameter in get_parameters(call)) {
        parameter_name <- get_name(parameter)
        parameter_position <- get_position(parameter)
        ## NOTE: eval parameters don't have more than one argument
        ##       because they are never varargs
        argument <- get_arguments(parameter)[[1]]
        argument_expr <- expr_to_string(get_expression(argument))
        evaluated <- is_evaluated(argument)


        assign(as.character(get_id(argument)),
               list(call_id = call_id,
                    package_name = call_package,
                    function_name = call_name,
                    argument_position = parameter_position,
                    argument_name = parameter_name,
                    argument_expr = argument_expr,
                    is_evaluated = evaluated),
               arguments)
    }

    data <- list(calls = calls, arguments = arguments)
    set_data(context, data)
}

expr_to_string <- function(expr) {
    paste(deparse(expr), collapse = "\n")
}

get_loaded_package_environments <- function() {
    envs <- list()
    env_names <- c()

    env <- .GlobalEnv

    while (!identical(env, emptyenv())) {
        env <- parent.env(env)
        envs <- c(envs, env)
        env_name <- environmentName(env)
        env_names <- c(env_names, env_name)
    }

    names(envs) <- env_names

    envs
}

classify_environment <- function(application_frame_position,
                                 call_frame_position,
                                 callee_env,
                                 eval_env) {

    ## check bases cases

    ## The environments of primitive functions of base package are NULL
    if (is.null(eval_env)) {
        return ("base")
    }

    if (identical(eval_env, emptyenv())) {
        return("empty")
    }

    if (identical(eval_env, callee_env)) {
        return("callee")
    }

    if (identical(eval_env, .GlobalEnv)) {
        return("global")
    }

    ## check if environment is a package environment

    package_envs <- get_loaded_package_environments()
    package_env_names <- names(package_envs)

    index <- length(package_envs)

    while (index != 0) {
        env <- package_envs[[index]]
        env_name <- package_env_names[index]

        if (identical(eval_env, env)) {
            return(env_name)
        }

        index <- index - 1
    }

    ## check if environment is a caller environment

    frames <- sys.frames()

    parents <- sys.parents()

    index <- parents[call_frame_position]

    parent_count <- 0

    while (index != 0) {
        parent_count <- parent_count + 1

        if (identical(frames[[index]], eval_env)) {
            break
        }

        index <- parents[index]
    }

    ## this means the eval_env did not belong to any of the parent callers
    ## it has to be a new.env and we need to recursively classify its parent env
    if (index == 0) {
        parent_class <- classify_environment(application_frame_position,
                                             call_frame_position,
                                             callee_env,
                                             parent.env(eval_env))
        return(paste("new", parent_class, sep = "+"))
    }
    ## this means the eval_env is one of the parent caller's environments
    else {
        return(paste("caller", parent_count, sep = "-"))
    }

    return("unhandled")
}
