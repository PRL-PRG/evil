#' @export
normalize <- function(h, expr) {
    .Call(C_normalize, h, expr)
}


#' @export
normalize_expr <- function(expr) {
    .Call(C_normalize_expr, expr)
}
