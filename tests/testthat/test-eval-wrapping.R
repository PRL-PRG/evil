test_that("wrap_eval works", {
  expr <- quote(f(eval(1), g(eval(2)), eval(3)))

  r <- wrap_evals(expr, "id")

  # it is DFS visit of the AST
  expect_equal(attr(r[[2]], "csid"), "id1")
  expect_equal(attr(r[[3]][[2]], "csid"), "id2")
  expect_equal(attr(r[[4]], "csid"), "id3")
  # should not alter the expression itself
  expect_equal(r, expr)
})

test_that("wrapping works", {
  test <- function() {
    f <- function() {
      withr::defer(format(1))
      withr::deferred_run()
    }

    evil::trace_code(f(), evals_to_trace=c("withr::execute_handlers"))
  }

  r <- callr::r(test)
  expect_equal(r$tables$calls$eval_call_srcref, "::withr::execute_handlers::1")
})

test_that("tracing code gets wrapped as well", {
  # TODO: add support for global, base
  # TODO: test with NULL - tracing all
  r <- trace_code(eval(1), evals_to_trace="global")
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

test_that("see_if", {
  calls <- do_trace_eval({
    assertthat::see_if(is.character("A"))
  })

  expect_equal(nrow(calls), 2)
  expect_true(
    all(
      calls$eval_call_srcref %in%
        c(
          "::assertthat::see_if::1",
          "::assertthat::see_if::2"
        )
    )
  )
})
