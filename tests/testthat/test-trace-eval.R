test_that("example", {
   r <- trace_eval({
      library(ggplot2)
      benchplot(ggplot(mtcars, aes(mpg, wt)) + geom_point())
   })
   library(dplyr)
   d <- tibble::as_tibble(r$data)
   expect_true("loop" %in% d$environment_class)
})

test_that("eval from a thunk", {
  d <- do_trace_eval({
    f <- function(thunk) force(thunk)
    f(eval(1+1))
  })

  expect_equal(d$eval_call_expression, "eval(1 + 1)")
  expect_equal(d$caller_expression, "eval(code, test_env)")

  # the following is wrong
  expect_equal(d$caller_package, "base")
  expect_equal(d$caller_function, "eval")
  expect_equal(d$caller_expression, "eval(code, test_env)")

  expect_equal(d$caller_stack_expression, "eval(1 + 1)\nforce(thunk)\nf(eval(1 + 1))")
  expect_equal(d$caller_stack_expression_raw, "function(x) x\nfunction(thunk) force(thunk)\nf(eval(1+1))")
})

test_that("function and function arity", {
  d <- do_trace_eval(eval(base::quote(TRUE)))
  expect_equal(d$expr_expression_function, "base::quote")
  expect_equal(d$expr_expression_args_num, 1)

  d <- do_trace_eval(eval(.Primitive("sin")(1)))
  expect_equal(d$expr_expression_function, ".Primitive(\"sin\")")
  expect_equal(d$expr_expression_args_num, 1)

  d <- do_trace_eval(eval(base:::.Primitive("sin")(1)))
  expect_equal(d$expr_expression_function, "base:::.Primitive(\"sin\")")
  expect_equal(d$expr_expression_args_num, 1)

  d <- do_trace_eval(eval({sin(1) + 1; sin(1) + 2}))
  expect_equal(d$expr_expression_function, "{")
  expect_equal(d$expr_expression_args_num, 2)
})

test_that("a smoke test for a base function calling eval", {
  f <- function(x=c("A")) {
    match.arg(x)
  }

  d <- do_trace_eval(f())
  expect_equal(d$eval_function, "eval")
  expect_equal(d$eval_call_expression, "eval(formal.args[[as.character(substitute(arg))]], envir = sys.frame(sysP))")
  expect_equal(d$caller_expression, "match.arg(x)")
  expect_equal(d$caller_function, "match.arg")
  expect_equal(d$caller_package, "base")

  expect_equal(d$expr_expression, "formal.args[[as.character(substitute(arg))]]")
  expect_equal(d$expr_expression_type, 6)
  expect_equal(d$expr_expression_function, "[[")
  expect_equal(d$expr_expression_args_num, 2)

  expect_equal(d$expr_resolved, "c(\"A\")")
  expect_equal(d$expr_resolved_type, 6)
  expect_equal(d$expr_resolved_function, "c")
  expect_equal(d$expr_resolved_args_num, 1)

  expect_equal(d$envir_expression, "sys.frame(sysP)")
  expect_equal(d$envir_type, 4)

  expect_equal(d$enclos_expression, NA)
  expect_equal(d$enclos_type, 4)
})

test_that("long expression is cut off", {
  f <- function(x) g(x)
  g <- function(x) eval(x)

  d <- do_trace_eval({
    f(quote({
      x <- 1
      y <- 2
      z <- 3
      (function(expr, width.cutoff = 60L,
                backtick = mode(expr) %in% c("call", "expression", "(", "function"),
                control = c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                nlines = -1L)
        .Internal(deparse(expr, width.cutoff, backtick,
                          .deparseOpts(control), nlines)))(quote(x+y+z))
    }))
  })
  expect_equal(d$caller_stack_expression, "eval(x)\ng(x)\nf(quote({⏎    x <- 1⏎    y <- 2⏎    z <- 3⏎    (function(expr, width.cutoff = 60⋯")
  expect_true(startsWith(d$expr_resolved, "{\n    x <- 1\n    y <- 2\n    z <- 3\n"))
  expect_equal(d$expr_resolved_length, 345)
})

test_that("expr_resolve captures only language expression", {
  withr::local_options(list(keep.source=TRUE, keep.parse.data=TRUE))

  f <- function(a, x=1, y=x, z=sin) {
    eval(substitute(a))
  }

  ## d <- do_trace_eval(f(x))
  ## expect_true(is.na(d$expr_resolved))
  ## expect_equal(d$expr_resolved_type, 14)

  d1 <- do_trace_eval(f(x))
  d2 <- do_trace_eval(f(1))
  expect_equal(d1$expr_resolved_hash, d2$expr_resolved_hash)

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
  expect_true(startsWith(d$expr_parsed_expression, "parse(text"))

  d <- do_trace_eval(f(g2, 2))
  expect_equal(d$expr_resolved, "identity(2)")
  expect_true(startsWith(d$expr_parsed_expression, "str2expression("))

  d <- do_trace_eval(f(g3, 3))
  expect_equal(d$expr_resolved, "identity(3)")
  expect_true(startsWith(d$expr_parsed_expression, "str2lang("))

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
    parse(text=paste0(y, "+", y))
  }

  d <- trace_eval(f("var"))

  # TODO: assertions
})
