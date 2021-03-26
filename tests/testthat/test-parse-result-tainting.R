## TODO this is a known bug
## we cannot attach an attribute to a symbol
## test_that("parse variable", {
##   do_trace_eval({
##     p <- str2lang("sin")
##   })
##   expect_equal(attr(p, "._evil_parsed_expression"), 'str2lang(s = "sin")')
## })

test_that("parse taints the result", {
   do_trace_eval({
     p <- parse(text="1+1")
     q <- str2expression(text="2+2")
     r <- str2lang(s="3+3")
   })

   pa <- attributes(p)
   qa <- attributes(q)
   ra <- attributes(r)

   attributes(p) <- NULL
   attributes(q) <- NULL
   attributes(r) <- NULL

   expect_equal(p, as.expression(quote(1+1)))
   expect_equal(q, as.expression(quote(2+2)))
   expect_equal(r, quote(3+3))

   expect_equal(pa$`._evil_parsed_expression`, 'parse(text = "1+1")')
   expect_equal(qa$`._evil_parsed_expression`, 'str2expression(text = "2+2")')
   expect_equal(ra$`._evil_parsed_expression`, 'str2lang(s = "3+3")')
})

test_that("resolve parse from variable", {
  f <- function() {
    x <- parse(text="1+1")
    eval(x)
  }

  calls <- do_trace_eval(f())

  expect_equal(nrow(calls), 1)
  expect_equal(calls$eval_call_expression, "eval(x)")
  expect_equal(calls$expr_parsed_expression, 'parse(text = "1+1")')
})

test_that("resolve parse directly", {
  f <- function() {
    eval(parse(text = "1+1"))
  }

  calls <- do_trace_eval(f())

  expect_equal(nrow(calls), 1)
  expect_equal(calls$eval_call_expression, 'eval(parse(text = "1+1"))')
  expect_equal(calls$expr_parsed_expression, 'parse(text = "1+1")')
})
