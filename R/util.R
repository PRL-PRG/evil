sexp_typeof <- function(x, tag = FALSE) {
  if (!tag & typeof(x) == "expression") {
    paste(unlist(Map(sexp_typeof, x)), collapse = ";", sep = ";")
  }
  else {
    .Call(C_sexp_typeof, x)
  }
}

mark_parsed_expression <- function(x, parse_fun_name) {
  .Call(C_mark_parsed_expression, x, parse_fun_name)
}

get_ast_size <- function(expr) {
  .Call(C_get_ast_size, expr)
}


resolve_expr <- function(x, env) {
  if (!is.environment(env)) {
    return(x)
  }

  if (is.symbol(x)) {
    y <- get0(as.character(x), env, ifnotfound = .Empty)
    if (is.symbol(y)) {
      resolve_expr(y, env)
    } else {
      y
    }
  } else {
    x
  }
}

env_type_to_string <- function(env) {
  if (is.environment(env)) {
    NA
  } else {
    typeof(env)
  }
}

#' @importFrom utils capture.output
expr_to_string <- function(e,
                           max_length = Inf,
                           one_line = FALSE) {
  return_symbol <- "\u23CE" # unicode return symbol
  ellipsis <- "\u2026" # unicode horizontal ellipsis

  if (is.null(e) || is_empty(e)) {
    NA_character_
  } else if (is.expression(e) && length(e) == 1) {
    expr_to_string(e[[1]])
  } else {
    cuttof <- 500L
    nlines <- -1L
    if (!is.infinite(max_length)) {
      cuttof <- 120L
      nlines <- 1 + max_length %/% cuttof
    }

    s <- deparse1(e, width.cutoff = cuttof, nlines = nlines, collapse = "\n")


    if (one_line) {
      s <- gsub("\n", return_symbol, s)
    }
    s
  }
}


#' @importFrom utils getSrcDirectory getSrcFilename
get_call_srcref <- function(call) {
  srcref <- getSrcref(call)

  if (is.integer(srcref) && length(srcref) >= 6) {
    file <- getSrcFilename(srcref)
    file <- if (is.null(file)) {
      "<unknown>"
    } else {
      dir <- getSrcDirectory(srcref)
      file.path(dir, file)
    }

    first_line <- srcref[1]
    last_line <- srcref[3]
    first_col <- srcref[5]
    last_col <- srcref[6]

    paste0(c(file, first_line, first_col, last_line, last_col),
      collapse = ":"
    )
  } else {
    NA_character_
  }
}

get_loaded_package_environments <- function() {
  ## nolint
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

is_package_environment <- function(eval_env) {
  ## nolint
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

  return(NULL)
}

is_base_case_env <- function(callee_env, eval_env) {
  ## nolint
  ## The environments of primitive functions of base package are NULL
  if (is.null(eval_env)) {
    return("base")
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
  return(NULL)
}

classify_environment <- function(application_frame_position,
                                 ## nolint
                                 eval_call_frame_position,
                                 callee_env,
                                 eval_env) {
  ## check if environment is a caller environment

  frames <- sys.frames()

  parents <- sys.parents()

  index <- parents[eval_call_frame_position]

  parent_count <- 0

  seen_parents <- integer(0)
  while (index != 0) {
    parent_count <- parent_count + 1

    if (index %in% seen_parents) {
      index <- -1
      break
    }
    if (identical(frames[[index]], eval_env)) {
      break
    }

    seen_parents <- c(index, seen_parents)

    index <- parents[index]
  }

  # env can be both a caller-n and global so we compute both
  base_case_env <- is_base_case_env(callee_env, eval_env)
  package_env_name <- is_package_environment(eval_env)

  ## this means the eval_env did not belong to any of the parent callers
  ## if it is not a base case or a package environment
  ## it has to be a new.env and we need to recursively classify its parent env
  if (index == 0) {
    ## check bases cases or package environment
    if (!is.null(base_case_env) || !is.null(package_env_name)) {
      specific_class <- paste0(base_case_env, package_env_name)
      return(paste("caller", parent_count, specific_class, sep = "-"))
    }

    parent_class <-
      classify_environment(
        application_frame_position,
        eval_call_frame_position,
        callee_env,
        parent.env(eval_env)
      )
    return(paste("new", parent_class, sep = "+"))
  } else if (index == -1) {
    ## check bases cases
    if (!is.null(base_case_env)) {
      return(base_case_env)
    }

    ## check if environment is a package environment
    if (!is.null(package_env_name)) {
      return(package_env_name)
    }
    ## this means that there was a loop in the frames
    return("loop")
  } else {
    specific_class <- paste0(base_case_env, package_env_name)
    ## this means the eval_env is one of the parent caller's environments
    return(paste("caller", parent_count, specific_class, sep = "-"))
  }

  return("unhandled")
}



from_match.call <- function(expr_resolved, addresses_set) {
  if (is.call(expr_resolved)) {
    for (i in seq_along(expr_resolved)) {
      t <- expr_resolved[[i]]
      if (!missing(t) && !is.null(t) && exists(injectr::sexp_address(t), where=addresses_set)) {
        # We don't remove the set of addresses from the hashmap; It could be used
        # by another eval

        # get the name of the function called (including namespace)
        # it is actually redundant with expr_resolved_function (which is also more accurate)
        return("match.call")
      }
    }
  }
  return(NA_character_)
}
