do_trace_eval <- function(expr) {
  r <- trace_eval(expr)

  if(is_error(r$result)) stop(r$result$error$message)

  r$data$calls
}

expect_starts_with <- function(s1, s2) {
  
  as1 <- quasi_label(rlang::enquo(s1), label = "s1")
  as2 <- quasi_label(rlang::enquo(s2), label = "s2")
  
  expect(
    startsWith(as1$val, as2$val),
    sprintf("%s does not start with:\n===== \n%s\n=====\nbut with: \n=====\n%s", as1$lab, as2$val, as1$val)
  )
  
  invisible(as1$val)
}
