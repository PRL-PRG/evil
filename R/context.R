
#' @export
#' @importFrom instrumentr create_context
create_evil_context <- function() {
    functions <- c("base::eval", "base::evalq", "base::eval.parent", "base::local")
    create_context(application_load_callback = application_load_callback,
                   call_exit_callback = call_exit_callback,
                   functions = functions)
}

#' @importFrom instrumentr set_data
application_load_callback <- function(context, application) {

    calls <- data.frame(call_id = integer(0),
                        callee_package = character(0),
                        callee_name = character(0),
                        call_expression = character(0),
                        ## TODO: caller_expression = character(0),
                        ## TODO: caller_package = character(0),
                        ## TODO: caller_name = character(0),
                        environment_class = character(0),
                        stringsAsFactors = FALSE)


    arguments <- data.frame(call_id = integer(0),
                            callee_package = character(0),
                            callee_name = character(0),
                            ##TODO: caller_package = character(0),
                            ##TODO: caller_name = character(0),
                            argument_position = integer(0),
                            argument_name = character(0),
                            argument_expr = character(0),
                            is_evaluated = logical(0),
                            stringsAsFactors = FALSE)

    data <- list(calls = calls, arguments = arguments)

    set_data(context, data)
}

#' @importFrom instrumentr get_data set_data get_id get_name get_parameters
#' @importFrom instrumentr get_arguments get_position get_expression
#' @importFrom instrumentr is_evaluated get_frame_position get_environment
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

    calls[nrow(calls) + 1, ] <- list(call_id,
                                     call_package,
                                     call_name,
                                     call_expression,
                                     environment_class)

    for (parameter in get_parameters(call)) {
        parameter_name <- get_name(parameter)
        parameter_position <- get_position(parameter)
        ## NOTE: eval parameters don't have more than one argument
        ##       because they are never varargs
        argument <- get_arguments(parameter)[[1]]
        argument_expr <- expr_to_string(get_expression(argument))
        evaluated <- is_evaluated(argument)

        arguments[nrow(arguments) + 1, ] <- list(call_id,
                                                 call_package,
                                                 call_name,
                                                 parameter_position,
                                                 parameter_name,
                                                 argument_expr,
                                                 evaluated)

    }

    data <- list(calls = calls, arguments = arguments)
    set_data(context, data)
}

get_loaded_package_environments <- function() {
    envs <- list()
    env_names <- c()

    env <- .GlobalEnv

    while (!identical(env, emptyenv())) {
        env <- parent.env(env)
        envs <- c(envs, env)
        env_name <- attr(env, "name")
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
}

expr_to_string <- function(expr) paste(deparse(expr), collapse = "\n")
