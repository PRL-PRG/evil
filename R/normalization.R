#' @export
normalize_expr <- function(expr) {
    .Call(C_normalize_expr, expr)
}