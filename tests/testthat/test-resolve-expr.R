test_that("a smoke test for a base function calling eval", {
  f <- function(x=c("A")) {
    match.arg(x)
  }

  d <- do_trace_eval(f())
  expect_equal(d$call_package, "base")
  expect_equal(d$call_function, "eval")
  expect_equal(d$call_expression, "eval(formal.args[[as.character(substitute(arg))]], envir = sys.frame(sysP))")

  expect_equal(d$caller_expression, "match.arg(x)")
  expect_equal(d$caller_function, "match.arg")
  expect_equal(d$caller_package, "base")

  expect_equal(d$expr_expression, "formal.args[[as.character(substitute(arg))]]")
  expect_equal(d$expr_expression_type, 6)
  expect_equal(d$expr_resolved, "c(\"A\")")
  expect_equal(d$expr_resolved_type, 6)

  expect_equal(d$envir_expression, "sys.frame(sysP)")
  expect_equal(d$envir_type, 4)

  expect_equal(d$enclos_expression, NA)
  expect_equal(d$enclos_type, 4)
})

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
    parse(text=paste0("identity(", y, ")"))
  }
  g2 <- function(y) {
    str2expression(paste0("identity(", y, ")"))
  }
  g3 <- function(y) {
    str2lang(paste0("identity(", y, ")"))
  }

  d <- do_trace_eval(f(g1, 1))
  expect_equal(d$expr_resolved, "identity(1)")
  expect_equal(d$expr_parsed, "parse")

  d <- do_trace_eval(f(g2, 2))
  expect_equal(d$expr_resolved, "identity(2)")
  expect_equal(d$expr_parsed, "str2expression")

  d <- do_trace_eval(f(g3, 3))
  expect_equal(d$expr_resolved, "identity(3)")
  expect_equal(d$expr_parsed, "str2lang")

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

  # TODO: assertions
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

  # TODO: assertions
})
