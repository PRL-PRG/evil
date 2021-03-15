.DefaultArgs <- list(
    eval=formals(eval),
    evalq=formals(evalq),
    eval.parent=formals(eval.parent),
    local=formals(local)
)

.Empty <- new.env(parent=emptyenv())

.state <- new.env(parent=emptyenv())

.base_packages <- c(
  "base",
  "compiler",
  "graphics",
  "grDevices",
  "grid",
  "methods",
  "parallel",
  "splines",
  "stats",
  "stats4",
  "tcltk",
  "tools",
  "utils"
)

is_empty <- function(x) identical(x, .Empty)
