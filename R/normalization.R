#' @export
normalize <- function(h, expr) {
    .Call(C_normalize, h, expr)
}
