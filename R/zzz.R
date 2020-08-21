.DefaultArgs <- list(
    eval=formals(eval),
    evalq=formals(evalq),
    eval.parent=formals(eval.parent),
    local=formals(local)
)

.Empty <- new.env(parent=emptyenv())

is_empty <- function(x) identical(x, .Empty)
