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
