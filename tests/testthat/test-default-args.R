test_that("default envir", {
  f <- function() eval(1)

  calls <- do_trace_eval(f())

  expect_equal(calls$envir_expression, "NA")
})

test_that("same as default", {
  f <- function() {
    env <- environment()
    eval(1, envir=env)
  }

  calls <- do_trace_eval(f())

  expect_equal(calls$envir_expression, "env")
})

test_that("parent.frame() envir", {
  f <- function() eval(1, envir=parent.frame())

  calls <- do_trace_eval(f())

  expect_equal(calls$envir_expression, "parent.frame()")
})

test_that("other env envir", {
  f <- function() eval(1, envir=new.env())

  calls <- do_trace_eval(f())

  expect_equal(calls$envir_expression, "new.env()")
})
