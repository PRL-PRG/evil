test_that("sexp_typeof works for NULL", {
  expect_equal(sexp_typeof(NULL), "NULL")
})

test_that("get_ast_size", {
    expect_equal(get_ast_size(NULL), 1)

    expect_equal(get_ast_size(1), 1)

    expect_equal(get_ast_size(1L), 1)

    expect_equal(get_ast_size(quote(f(1,2,3))), 4)

    expect_equal(get_ast_size(list(1,2,3)), 1)

    x <- 1:1000000
    expect_equal(get_ast_size(x), 1)

    expect_equal(get_ast_size(function(x) x + 1), 1)

    ## NOTE: function asts contain a hidden 4th element that is being counted here
    expect_equal(get_ast_size(quote(function(x) x + 1)), 6)

    ## TODO: function asts contain a hidden 4th element that is being counted here
    expect_equal(get_ast_size(expression(x = 1 + 2, y = 8 + 9)), 7)
})

## test_that("x", {
##   r <- trace_code({
##     g <- function(x) {
##       print(x)
##     }

##     f <- function(x) {
##       call <- match.call()
##       call$x <- data.frame(x=rnorm(10))
##       print(call)
##       call[[1L]] <- as.name("g")
##       eval(call)
##     }

##     f(1)
##   }, evals_to_trace="evil")
##   browser()
##   1
## })

## test_that("expr_to_string", {
##   r <- trace_code({
##     library(A3)
##     require(randomForest)
##     data(housing)
##     x <- a3(MED.VALUE ~ NOX + PUPIL.TEACHER + ROOMS + AGE + HIGHWAY + 0,
##             housing, randomForest, p.acc = NULL, n.folds = 2)

##     plotSlopes(x)
##   }, evals_to_trace="randomForest")
##   browser()
##   1
## })
# run/trace-eval/A3/examples/plotSlopes.Rd.R/A3/examples/plotSlopes.Rd.R
