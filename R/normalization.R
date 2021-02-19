#' @export
normalize <- function(h, expr, trimmed_expr) {
    .Call(C_normalize, h, expr, trimmed_expr)
}


#' @export
normalize_expr <- function(expr) {
    .Call(C_normalize_expr, expr)
}
