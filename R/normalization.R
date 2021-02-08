#' @export
normalize_expr <- function(expr) {
    .Call(C_normalize_expr, expr)
}

#' @export
build_tree <- function(expr) {
    .Call(C_build_tree, expr)
}

#' @export
tree_to_string <- function(expr) {
    .Call(C_tree_to_string, expr)
}

#' @export
simplify_tree <- function(expr) {
    .Call(C_simplify, expr)
}

#' @export
normalize_stats_expr <- function(expr) {
    .Call(C_normalize_stats_expr, expr)
}