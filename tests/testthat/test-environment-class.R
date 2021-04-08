test_that("default argument classes", {
    f <- function() {
        eval(1)
    }

    calls <- do_trace_eval(f())

    expect_equal(calls$environment_class, "caller-0-")

    g <- function() {
        eval(1, parent.frame())
    }

    f <- function() { g() }
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

    expect_equal(calls$environment_class[order(calls$eval_call_id)], c("caller-2-", "caller-0-"))
})

test_that("new env", {
    f <- function() {
        e <- new.env()
        eval(1, e)

        e <- new.env(parent = emptyenv())
        eval(1, e)
    }

    calls <- do_trace_eval(f())

    expect_equal(calls$environment_class[order(calls$eval_call_id)], c("new+caller-0-", "new+empty"))
})

test_that("global env", {
    f <- function() {
        eval(1, globalenv())
        eval(2, parent.frame())
    }

    calls <- do_trace_eval(f())

    expect_equal(calls$environment_class[order(calls$eval_call_id)], c("global", "caller-1-"))
})

test_that("base env", {
    f <- function() {
        eval(1, baseenv())
    }

    calls <- do_trace_eval(f())

    expect_equal(calls$environment_class, "base")

    f <- function() {
        eval(1, .BaseNamespaceEnv)
    }

    calls <- do_trace_eval(f())

    expect_equal(calls$environment_class, "base")
})

test_that("sys frame env", {
    g <- function() {
        eval(1, sys.frame(-1))
    }

    f <- function() {
        g()
    }

    calls <- do_trace_eval(f())

    expect_equal(calls$environment_class, "caller-1-")

    calls <- do_trace_eval(g())

    expect_equal(calls$environment_class, "new+new+new+base")
})

test_that("list2env", {
    f <- function() {
        e <-list2env(list(x = 3, y = 5))

        eval(quote(x), e)
    }

    calls <- do_trace_eval(f())

    expect_equal(calls$environment_class, "new+caller-0-")

    f <- function() {
        e <-list2env(list(x = 3, y = 5), envir = environment())

        eval(quote(x), e)
    }

    calls <- do_trace_eval(f())

    expect_equal(calls$environment_class, "caller-0-")

})
