do_trace_eval <- function(expr) {
  r <- trace_eval(expr)

  if(is_error(r$result)) stop(r$result$error$message)

  r$data
}

expect_starts_with <- function(s1, s2) {
  expect_true(startsWith(s1, s2))
}
