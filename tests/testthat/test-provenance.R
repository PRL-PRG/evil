test_that("parse is detected", {
    f <- function() {
        x <- parse(text = "1 + 1")
        eval(x)
    }

    provs <- do_trace_provenances(f())

    expect_equal(provs$provenance, "parse")
    expect_equal(provs$provenance_args, "parse(text = \"1 + 1\"); ")
    expect_equal(provs$nb_provenances, 1)
})

test_that("substitute is detected", {
    f <- function() {
        x <- substitute(1)
        eval(x)
    }

    provs <- do_trace_provenances(f())

    expect_equal(provs$provenance, "substitute")
    expect_equal(provs$provenance_args, "substitute(1); ")
    expect_equal(provs$nb_provenances, 1)
})

test_that("match.call and symbols", {
    g <- function(a1, a2) {
      mf <- match.call()
      eval(mf[[2]], parent.frame()) 
    }

    f <- function() {
        x <- 1
        g(x, 3)
    }

    provs <- do_trace_provenances(f())

    expect_equal(provs$provenance, "match.call")
    expect_equal(provs$provenance_args, "match.call(); ")
    expect_equal(provs$nb_provenances, 1)

    f <- function() {
        g(1, 3)
    }

    provs <- do_trace_provenances(f())

    expect_equal(provs$provenance, "match.call")
    expect_equal(provs$provenance_args, "match.call(); ")
    expect_equal(provs$nb_provenances, 1)

    f <- function() {
        g("hello", 3)
    }

    provs <- do_trace_provenances(f())

    expect_equal(provs$provenance, "match.call")
    expect_equal(provs$provenance_args, "match.call(); ")
    expect_equal(provs$nb_provenances, 1)
})

test_that("expression", {
    f <- function() {
        x <- expression(1)
        eval(x)
    }

    provs <- do_trace_provenances(f())

    expect_equal(provs$provenance, "expression")
    expect_equal(provs$provenance_args, "expression(1); ")
    expect_equal(provs$nb_provenances, 1)
})


test_that("Multiple provenances", {
    f <- function() {
        x <- parse(text = "1 ; 2")
        x[[2]] <- quote(4)
        eval(x)
    }

    provs <- do_trace_provenances(f())
    expect_true(provs$provenance %in% c("parse", "quote")) # not deterministic...
    expect_equal(provs$provenance_args, "quote(4); parse(text = \"1 ; 2\"); ")
    expect_equal(provs$nb_provenances, 2)
})

test_that("inside eval", {
    f <- function() {
        eval(parse(text = "1"))
    }

    provs <- do_trace_provenances(f())
    expect_equal(provs$provenance, "parse")
    expect_equal(provs$provenance_args, "parse(text = \"1\"); ")
    expect_equal(provs$nb_provenances, 1)

    f <- function() {
        g <- function() parse(text = "1")
        eval(g())
    }

    provs <- do_trace_provenances(f())
    expect_equal(provs$provenance, "parse")
    expect_equal(provs$provenance_args, "parse(text = \"1\"); ")
    expect_equal(provs$nb_provenances, 1)
})

test_that("match.call and multiple provenances", {
    provs <- do_trace_provenances({
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

    expect_equal(provs$provenance, "match.call")
    expect_equal(provs$provenance_args, "quote(g); match.call(); ")
    expect_equal(provs$nb_provenances, 2)
})

