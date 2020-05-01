state <- new.env(parent = emptyenv())
state$eval_calls <- data.frame(package_name = character(0),
                               function_name = character(0),
                               expression = character(0),
                               stack_frame = character(0))

state$base_eval <- base::eval

state$evil_eval <-
  function(expr, envir = parent.frame(), enclos = if (is.list(envir) ||
                                                      is.pairlist(envir)) parent.frame() else baseenv()) {

      evil:::set_base_eval()
      serialized_expr <-deparse(substitute(expr))
      stack_frames <- sys.calls()
      stack_frames <- paste(unlist(Map(deparse, stack_frames)), sep=" ", collapse="\n")
      evil:::add_entry("base", "eval", serialized_expr, stack_frames)
      evil:::set_evil_eval()

      .Internal(eval(expr, envir, enclos))
  }

environment(state$evil_eval) <- environment(state$base_eval)


set_base_eval <- function() {
    unlockBinding("eval", baseenv())
    assign("eval", state$base_eval, envir = baseenv())
    lockBinding("eval", baseenv())
}

set_evil_eval <- function() {
    unlockBinding("eval", baseenv())
    assign("eval", state$evil_eval, envir = baseenv())
    lockBinding("eval", baseenv())
}
