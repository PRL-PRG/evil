test_that("sexp_typeof works for NULL", {
  expect_equal(sexp_typeof(NULL), "NULL")
})

test_that("adresses are well detected", {
  ca <- call("f", quote(x), 1)
  addr_map <- new.env(parent = emptyenv())
  addresses <- vapply(ca, injectr::sexp_address, "")
  # add addresses to the map
  for (k in addresses) {
    addr_map[[k]] <- TRUE
  }

  # transform a bit
  ca[[1]] <- "g"

  from_addr <- from_match.call(ca, addr_map)

  expect(!is.na(from_addr), "Not detected: NA")
  expect_equal(from_addr, "g")
})

test_that("get_ast_size", {
  expect_equal(get_ast_size(NULL), 1)

  expect_equal(get_ast_size(1), 1)

  expect_equal(get_ast_size(1L), 1)

  expect_equal(get_ast_size(quote(f(1, 2, 3))), 4)

  expect_equal(get_ast_size(list(1, 2, 3)), 1)

  x <- 1:1000000
  expect_equal(get_ast_size(x), 1)

  expect_equal(get_ast_size(function(x) x + 1), 1)

  ## NOTE: function asts contain a hidden 4th element that is being counted here
  expect_equal(get_ast_size(quote(function(x) x + 1)), 6)

  ## TODO: function asts contain a hidden 4th element that is being counted here
  expect_equal(get_ast_size(expression(x = 1 + 2, y = 8 + 9)), 7)
})

test_that("Normalization works", {
  expect_equal(normalize_expr(expression(1 + 1)), "NUM")
  expect_equal(normalize_expr(quote(1 + 1)), "NUM")
  expect_equal(normalize_expr(quote(TRUE && FALSE)), "BOOL")
  expect_equal(normalize_expr(quote(paste0("Hello, ", "World"))), "STR")
  expect_equal(normalize_expr(quote(x)), "VAR")
  expect_equal(normalize_expr(quote(x + y)), "OP(VAR)")
  expect_equal(normalize_expr(quote(x + 1)), "OP(VAR)")

  expect_equal(normalize_expr(quote(c(1, 2, 3, 4))), "c(NUM)")
  expect_equal(normalize_expr(quote(list(1, 2, 3, 4))), "list(NUM)")
  expect_equal(normalize_expr(quote(c(TRUE, FALSE, FALSE, TRUE))), "c(BOOL)")
  expect_equal(normalize_expr(quote(c("a", "e", "i", "o"))), "c(STR)")
  expect_equal(normalize_expr(quote(list(1, "hello"))), "list(NUM, STR)")
  expect_equal(normalize_expr(quote(c(1, 2, x, 4))), "c(VAR)")

  # Stress test: more than 100 characters to allocate
  # 6 x pattern 1,  "test", TRUE, "test2",
  expect_equal(
    normalize_expr(quote(list(
      1,  "test", TRUE, "test2",
      1,  "test", TRUE, "test2",
      1,  "test", TRUE, "test2",
      1,  "test", TRUE, "test2",
      1,  "test", TRUE, "test2",
      1,  "test", TRUE, "test2"
    ))),
    "list(NUM, STR, BOOL, STR, NUM, STR, BOOL, STR, NUM, STR, BOOL, STR, NUM, STR, BOOL, STR, NUM, STR, BOOL, STR, NUM, STR, BOOL, STR)"
  )

  # Crushing consecutive same types in c and list
  expect_equal(
    normalize_expr(quote(c(1, 2, 3, "hi", "test", 4, "true"))),
    "c(NUM, STR, NUM, STR)"
  )

  # Namespace name is ignored
  expect_equal(
    normalize_expr(quote(a::b(4))), "b(NUM)"
  )

  expect_equal(
    normalize_expr(quote(rgt::`+`(1, 2))), "NUM"
  )
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
