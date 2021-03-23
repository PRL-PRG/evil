test_that("Environment passed to the caller function  and to envir are well detected", {
  calls <- do_trace_eval({
    f <- function(en) {
      eval(as.name("x"), envir = en)
    }

    e <- new.env()
    e$x <- 3
    f(e)
  })

  expect_equal(calls$envir_from_arg, 0)

  calls <- do_trace_eval({
    f <- function() {
      eval(as.name("x"), envir = parent.frame())
    }

    x <- 3
    f()
  })

  expect_true(is.na(calls$envir_from_arg))
})

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
  expect_equal(calls$expr_match_call, "g")
})


# test_that("side-effecting evals are captured", {
#   calls <- do_trace_eval({
#     x <- 32
#     evalq(x <<- 332)
#   })
#
#   expect_equal(calls$direct_writes, 1)
#
#
#   calls <- do_trace_eval({
#     evalq(x <- 34)
#   })
#
#   expect_equal(calls$direct_writes, 0)
#
#
#   calls <- do_trace_eval({
#     me <- 34
#     f <- function(x) {
#       me <<- x
#     }
#     evalq(f(32))
#   })
#
#   expect_equal(calls$direct_writes, 0)
#
#
#   calls <- do_trace_eval({
#     me <- 34
#     f <- function(x) {
#       me <<- x
#     }
#     evalq(evalq(f(32)), new.env())
#   })
#
#   expect_equal(calls$direct_writes, c(1, 0))
#   expect_equal(calls$indirect_writes, c(0, 1))
# })


test_that("eval from a thunk", {
  d <- do_trace_eval({
    f <- function(thunk) force(thunk)
    f(eval(1 + 1))
  })

  expect_equal(d$eval_call_expression, "eval(1 + 1)")
  expect_equal(d$caller_expression, "eval(code, test_env)")

  expect_equal(d$expr_expression_nodes, 3)
  expect_equal(d$expr_resolved_nodes, 1)

  # the following is wrong
  expect_equal(d$caller_package, "base")
  expect_equal(d$caller_function, "eval")
  expect_equal(d$caller_expression, "eval(code, test_env)")
})

test_that("eval return value", {
  d <- do_trace_eval({
    r <- eval(1 +1)
  })

  expect_equal(d$expr_return, NA_character_)# because we convert values to NA
  expect_equal(d$expr_return_type, "double")
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

  d <- do_trace_eval(eval({
    sin(1) + 1
    sin(1) + 2
  }))
  expect_equal(d$expr_expression_function, "{")
  expect_equal(d$expr_expression_args_num, 2)
})

# test_that("a smoke test for a base function calling eval", {
#   f <- function(x = c("A")) {
#     match.arg(x)
#   }
#
#   d <- do_trace_eval(f())
#   expect_equal(d$eval_function, "eval")
#   expect_equal(d$eval_call_expression, "eval(formal.args[[as.character(substitute(arg))]], envir = sys.frame(sysP))")
#   expect_equal(d$caller_expression, "match.arg(x)")
#   expect_equal(d$caller_function, "match.arg")
#   expect_equal(d$caller_package, "base")
#
#   expect_equal(d$expr_expression, "formal.args[[as.character(substitute(arg))]]")
#   expect_equal(d$expr_expression_type, 6)
#   expect_equal(d$expr_expression_function, "[[")
#   expect_equal(d$expr_expression_args_num, 2)
#
#   expect_equal(d$expr_resolved, "c(\"A\")")
#   expect_equal(d$expr_resolved_type, 6)
#   expect_equal(d$expr_resolved_function, "c")
#   expect_equal(d$expr_resolved_args_num, 1)
#
#   expect_equal(d$envir_expression, "sys.frame(sysP)")
#   expect_equal(d$envir_type, 4)
#
#   expect_equal(d$enclos_expression, NA)
#   expect_equal(d$enclos_type, 4)
# })

# test_that("long expression is cut off", {
#   f <- function(x) g(x)
#   g <- function(x) eval(x)
#
#   d <- do_trace_eval({
#     f(quote({
#       x <- 1
#       y <- 2
#       z <- 3
#       (function(expr, width.cutoff = 60L,
#                 backtick = mode(expr) %in% c("call", "expression", "(", "function"),
#                 control = c("keepNA", "keepInteger", "niceNames", "showAttributes"),
#                 nlines = -1L) {
#         .Internal(deparse(
#           expr, width.cutoff, backtick,
#           .deparseOpts(control), nlines
#         ))
#       })(quote(x + y + z))
#     }))
#   })
#   expect_starts_with(d$expr_resolved, "{\n    x <- 1\n    y <- 2\n    z <- 3\n")
#   expect_equal(d$expr_resolved_length, 345)
# })

# test_that("expr_resolve captures only language expression", {
#   withr::local_options(list(keep.source = TRUE, keep.parse.data = TRUE))
#
#   f <- function(a, x = 1, y = x, z = sin) {
#     eval(substitute(a))
#   }
#
#   d <- do_trace_eval(f(x))
#   expect_true(is.na(d$expr_resolved))
#   expect_equal(d$expr_resolved_type, 14)
#
#   d1 <- do_trace_eval(f(x))
#   d2 <- do_trace_eval(f(1))
#   expect_equal(d1$expr_resolved_hash, d2$expr_resolved_hash)
#
#   d <- do_trace_eval(f(y))
#   expect_true(is.na(d$expr_resolved))
#   expect_equal(d$expr_resolved_type, 14)
#
#   d <- do_trace_eval(f(z))
#   expect_true(is.na(d$expr_resolved))
#   expect_equal(d$expr_resolved_type, 8)
#
#   d <- do_trace_eval(f(x + y))
#   expect_equal(d$expr_resolved, "x + y")
#   expect_equal(d$expr_resolved_type, 6)
#
#   r <- trace_eval(f(non_existing))
#   expect_false(r$data$calls$successful)
#   expect_true(is.na(r$data$calls$expr_resolved))
#   expect_true(is.na(r$data$calls$expr_resolved_type))
# })

test_that("basic eval", {
  withr::local_options(list(keep.source = TRUE, keep.parse.data = TRUE))

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

  d <- do_trace_eval(g(expr))

  expect_equal(d$expr_resolved, "identity(list(sin(a), cos(b)))")

  #TODO: add other assertions!
})

test_that("capture eval.parent", {
  withr::local_options(list(keep.source = TRUE, keep.parse.data = TRUE))

  var <- 1

  f <- function(x) {
    eval.parent(g(x))
  }

  g <- function(y) {
    parse(text = paste0(y, "+", y))
  }

  d <- do_trace_eval(f("var"))

  expect_equal(nrow(d), 2) #There is eval and eval.parent
  expect_equal(d[2, "expr_resolved"], "var + var")
  expect_true(dplyr::all_equal(d["eval_function"], dplyr::tribble(~eval_function, "eval", "eval.parent")))
})
