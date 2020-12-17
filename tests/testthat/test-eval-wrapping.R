test_that("wrap_eval works", {
  expr <- quote({ f(eval(1)) })

  r <- wrap_evals(expr, "id")

  expect_equal(attr(r[[2]][[2]], "csid"), "id1")
  expect_equal(r, expr)
})

test_that("wrapping works", {
  test <- function() {
    f <- function() {
      withr::defer(format(1))
      withr::deferred_run()
    }

    r <- evil::trace_code(f(), evals_to_trace=c("withr::execute_handlers"))
    print(r$tables$calls$eval_call_srcref)
  }

  r <- callr::r(test)
  expect_equal(r, "::withr::execute_handlers::1")
})

test_that("tracing code gets wrapped as well", {
  r <- trace_code(eval(1))
  expect_equal(r$tables$calls$eval_call_srcref, "::global::main::1")
})

test_that("evals to trace accepts packages", {
  r <- parse_evals_to_trace(c("pkg1", "pkg2"))
  expect_equal(r$package, c("pkg1", "pkg2"))
  expect_true(all(is.na(r$fun)))
})

test_that("evals to trace accepts function names", {
  r <- parse_evals_to_trace(c("pkg1::f", "pkg2::g", "pkg2::h"))
  expect_equal(r$package, c("pkg1", "pkg2", "pkg2"))
  expect_equal(r$fun, c("f", "g", "h"))
})

test_that("evals to trace accepts mix of packages and function names", {
  r <- parse_evals_to_trace(c("pkg1::f", "pkg2", "pkg2::h"))
  expect_equal(r$package, c("pkg1", "pkg2", "pkg2"))
  expect_equal(r$fun, c("f", NA, "h"))
})
