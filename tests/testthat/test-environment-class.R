test_that("default argument classes", {
    f <- function() {
        eval(1)
    }

    calls <- do_trace_eval(f())

    expect_equal(calls$environment_class, "caller-1-")
})

test_that("default argument classes up", {
    f <- function() {
        eval(1)
    }

    g <- function() {
        eval(quote(f()), envir = parent.frame(2))
    }

    h <- function() {
        g()
    }

    i <- function() { g() }

    calls <- do_trace_eval(i())

    expect_equal(calls$environment_class[order(calls$eval_call_id)], c("caller-3-", "caller-1-"))
})

test_that("new env", {
    f <- function() {
        e <- new.env()
        eval(1, e)

        e <- new.env(parent = emptyenv())
        eval(1, e)
    }

    calls <- do_trace_eval(f())

    expect_equal(calls$environment_class[order(calls$eval_call_id)], c("new+caller-1-", "new+caller-15-empty"))
})

test_that("global env", {
    f <- function() {
        eval(1, globalenv())
        eval(2, parent.frame())
    }

    calls <- do_trace_eval(f())

    expect_equal(calls$environment_class[order(calls$eval_call_id)], c("caller-15-global", "caller-2-"))
})

test_that("base env", {
    f <- function() {
        eval(1, baseenv())
    }

    calls <- do_trace_eval(f())

    expect_equal(calls$environment_class, c("caller-15-base"))

    f <- function() {
        eval(1, .BaseNamespaceEnv)
    }

    calls <- do_trace_eval(f())

    expect_equal(calls$environment_class, c("caller-15-base"))
})

test_that("sys frame env", {
    g <- function() {
        eval(1, sys.frame(-1))
    }

    f <- function() {
        g()
    }

    calls <- do_trace_eval(f())

    expect_equal(calls$environment_class, "caller-2-")

    calls <- do_trace_eval(g())

    # would be ... caller-1 ... byt testthat creates 14 additional frames (and from base, not global)
    expect_equal(calls$environment_class, "new+new+new+caller-15-base")
})

test_that("list2env", {
    f <- function() {
        e <-list2env(list(x = 3, y = 5))

        eval(quote(x), e)
    }

    calls <- do_trace_eval(f())

    expect_equal(calls$environment_class, "new-caller-1-")

    f <- function() {
        e <-list2env(list(x = 3, y = 5), envir = environment())

        eval(quote(x), e)
    }

    calls <- do_trace_eval(f())

    expect_equal(calls$environment_class, "caller-1-")

})
