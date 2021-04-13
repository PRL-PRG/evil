test_that("match.call is well detected", {
  calls <- do_trace_eval({
    g <- function(a1, a2, a3) {
      a1 + a2 * a3
    }

    f <- function(a1, a2, a3) {
      mf <- match.call()
      mf[[1]] <- quote(g)
      eval(mf, parent.frame())
    }

    f(2, 4, 89)
  })

  expect_false(is.na(calls$expr_match_call))
  expect_equal(calls$expr_match_call, "match.call")
})


test_that("match.call and symbols, scalars", {
  g <- function(a1, a2) {
      mf <- match.call()
      eval(mf[[2]], parent.frame()) 
  }

  f <- function() {
    x <- 1
    g(x, 3)
  }

  calls <- do_trace_eval(f())

  expect_equal(calls$expr_match_call, "match.call")

  f <- function() {
    g(1, 3)
  }

  calls <- do_trace_eval(f())

  expect_equal(calls$expr_match_call, "match.call")

  f <- function() {
    g("hello", 3)
  }

  calls <- do_trace_eval(f())

  expect_equal(calls$expr_match_call, "match.call")
})