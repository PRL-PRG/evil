test_that("expr_resolve captures only language expression", {
  withr::local_options(list(keep.source=TRUE, keep.parse.data=TRUE))

  f <- function(a, x=1, y=x, z=sin) {
    eval(substitute(a))
  }

  d <- do_trace_eval(f(x))
  expect_true(is.na(d$expr_resolved))
  expect_equal(d$expr_resolved_type, 14)

  d <- do_trace_eval(f(y))
  expect_true(is.na(d$expr_resolved))
  expect_equal(d$expr_resolved_type, 14)

  d <- do_trace_eval(f(z))
  expect_true(is.na(d$expr_resolved))
  expect_equal(d$expr_resolved_type, 8)

  d <- do_trace_eval(f(x+y))
  expect_equal(d$expr_resolved, "x + y")
  expect_equal(d$expr_resolved_type, 6)

  r <- trace_eval(f(non_existing))
  browser()
  expect_false(r$data$successful)
  expect_true(is.na(r$data$expr_resolved))
  expect_true(is.na(r$data$expr_resolved_type))
})

test_that("resolve parse", {
  withr::local_options(list(keep.source=TRUE, keep.parse.data=TRUE))

  f <- function(g, x) {
    eval(g(x))
  }

  g1 <- function(y) {
    parse(text=paste0("print(", y, ")"))
  }
  g2 <- function(y) {
    str2expression(paste0("print(", y, ")"))
  }
  g3 <- function(y) {
    str2lang(paste0("print(", y, ")"))
  }

  d <- do_trace_eval(f(g1, 1))
  expect_equal(d$expr_resolved, "print(1)")

  d <- do_trace_eval(f(g2, 2))
  expect_equal(d$expr_resolved, "print(2)")

  d <- do_trace_eval(f(g3, 3))
  expect_equal(d$expr_resolved, "print(3)")

  1
})

test_that("basic eval", {
  withr::local_options(list(keep.source=TRUE, keep.parse.data=TRUE))

  f <- function(x) {
    if (FALSE) {
      eval(call("str", x))
    } else {
      eval(x)
    }
  }

  g <- function(y) {
    f(call("identity", y))
  }

  a <- 1:10
  b <- a
  e1 <- quote(sin(a))
  e2 <- quote(cos(b))
  expr <- call("list", e1, e2)

  d <- trace_eval(g(expr))

  browser()

  1

})

test_that("capture eval.parent", {
  withr::local_options(list(keep.source=TRUE, keep.parse.data=TRUE))

  var <- 1

  f <- function(x) {
    eval.parent(g(x))
  }

  g <- function(y) {
    parse(text=paste0("print(", y, ")"))
  }

  d <- trace_eval(f("var"))

  browser()

  1
})
