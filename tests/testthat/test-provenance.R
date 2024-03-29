test_that("parse is detected", {
    f <- function() {
        x <- parse(text = "1 + 1")
        eval(x)
    }

    provs <- do_trace_provenances(f())

    expect_equal(provs$provenance, "parse")
    expect_equal(provs$provenance_args, "parse(text = \"1 + 1\")")
    expect_equal(provs$nb_provenances, 1)
})

test_that("substitute is detected", {
    f <- function() {
        x <- substitute(1)
        eval(x)
    }

    provs <- do_trace_provenances(f())

    expect_equal(provs$provenance, "substitute")
    expect_equal(provs$provenance_args, "substitute(1)")
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
    expect_equal(provs$provenance_args, "match.call()")
    expect_equal(provs$nb_provenances, 1)

    f <- function() {
        g(1, 3)
    }

    provs <- do_trace_provenances(f())

    expect_equal(provs$provenance, "match.call")
    expect_equal(provs$provenance_args, "match.call()")
    expect_equal(provs$nb_provenances, 1)

    f <- function() {
        g("hello", 3)
    }

    provs <- do_trace_provenances(f())

    expect_equal(provs$provenance, "match.call")
    expect_equal(provs$provenance_args, "match.call()")
    expect_equal(provs$nb_provenances, 1)
})

test_that("expression", {
    f <- function() {
        x <- expression(1)
        eval(x)
    }

    provs <- do_trace_provenances(f())

    expect_equal(provs$provenance, "expression")
    expect_equal(provs$provenance_args, "expression(1)")
    expect_equal(provs$nb_provenances, 1)
})


test_that("Multiple provenances", {
    f <- function() {
        x <- parse(text = "1 ; 2")
        x[[2]] <- quote(4)
        eval(x)
    }

    provs <- do_trace_provenances(f())
    expect_equal(provs$provenance, "parse")
    expect_equal(provs$provenance_args, "parse(text = \"1 ; 2\")")
    expect_equal(provs$nb_provenances, 2)
})

test_that("inside eval", {
    f <- function() {
        eval(parse(text = "1"))
    }

    provs <- do_trace_provenances(f())
    expect_equal(provs$provenance, "parse")
    expect_equal(provs$provenance_args, "parse(text = \"1\")")
    expect_equal(provs$nb_provenances, 1)

    f <- function() {
        g <- function() parse(text = "1")
        eval(g())
    }

    provs <- do_trace_provenances(f())
    expect_equal(provs$provenance, "parse")
    expect_equal(provs$provenance_args, "parse(text = \"1\")")
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
    expect_equal(provs$provenance_args, "match.call()")
    expect_equal(provs$nb_provenances, 2)
})

test_that("call", {
    provs <- do_trace_provenances({
        g <- call("+", 1, 1)
        eval(g)
    })

    expect_equal(provs$provenance, "call")
    expect_equal(provs$provenance_args, "call(\"+\", 1, 1)")
    expect_equal(provs$nb_provenances, 1)
})

test_that("as.name and as.symbol", {
    provs <- do_trace_provenances({
        x <- as.name("y")
        y <- 1
        eval(x)
    })

    expect_equal(provs$provenance, "as.name")
    expect_equal(provs$provenance_args, "as.name(\"y\")")
    expect_equal(provs$nb_provenances, 1)

    provs <- do_trace_provenances({
        x <- as.symbol("y")
        y <- 1
        eval(x)
    })

    expect_equal(provs$provenance, "as.symbol")
    expect_equal(provs$provenance_args, "as.symbol(\"y\")")
    expect_equal(provs$nb_provenances, 1)
})

test_that("tilde ~", {
    provs <- do_trace_provenances({
        e <- x ~ y 
        eval(e)
    })

    expect_equal(provs$provenance, "~")
    expect_equal(provs$provenance_args, "x ~ y")
    expect_equal(provs$nb_provenances, 1) 
})

test_that("no intermediate variable", {
    provs <- do_trace_provenances({
        a <- 1
        eval(parse(text = "a")[[1]])
    })

    expect_equal(provs$provenance, "parse")
    expect_equal(provs$provenance_args, "parse(text = \"a\")")
    expect_equal(provs$nb_provenances, 1) 
})

test_that("as.call", {
    provs <- do_trace_provenances({
        cl <- as.call(list(quote(`+`), 1, 1))
        eval(cl)
    })

    expect_equal(provs$provenance, "as.call")
    expect_equal(provs$provenance_args, "as.call(list(quote(`+`), 1, 1))")
    expect_equal(provs$nb_provenances, 1) 
})

test_that("language in a list", {
    provs <- do_trace_provenances({
        l <- list(quote(a), 3)
        a <- -7
        eval(l[[1]])
    })

    expect_equal(provs$provenance, "quote")
    expect_equal(provs$provenance_args, "quote(a)")
    expect_equal(provs$nb_provenances, 1) 
})

test_that("attributes", {
    provs <- do_trace_provenances({
        a <- -8
        x <- "test"
        attr(x, "p") <- parse(text = "1 ; a")
        attr(x, "p") <- attr(x, "p")[[2]]
        eval(attr(x, "p"))
    })

    expect_equal(provs$provenance, "parse")
    expect_equal(provs$provenance_args, "parse(text = \"1 ; a\")")
    expect_equal(provs$nb_provenances, 1) 
})


test_that("expression in a list", {
    g <- function() {
        convergence <- expression({
            a + 1
        })

        list(p = 45, convergence = convergence)
    }

    provs <- do_trace_provenances({
        a <- 3

        l <- g()

        eval(l$convergence)
    })

    expect_equal(provs$provenance, "expression")
    expect_equal(provs$provenance_args, "expression({\\n     a + 1\\n })")
    expect_equal(provs$nb_provenances, 1) 
})

test_that("expression in default argument", {
    g <- function(b = expression(a+1)) {
        list(p = 45, ex = b)
    }

    provs <- do_trace_provenances({
        a <- 3

        l <- g()

        eval(l$ex)
    })

    expect_equal(provs$provenance, "expression")
    expect_equal(provs$provenance_args, "expression(a + 1)")
    expect_equal(provs$nb_provenances, 1) 
})

test_that("structure", {
    provs <- do_trace_provenances({
        a <- 3

        l <- structure(list(conver = quote(a+1)), plop = "test")

        eval(l$conver)
    })

    expect_equal(provs$provenance, "quote")
    expect_equal(provs$provenance_args, "quote(a + 1)")
    expect_equal(provs$nb_provenances, 1) 
})

test_that("slot", {
    provs <- do_trace_provenances({
        setClass("langObj", slots = c(x="expression", y="numeric"))
        myLang <- new("langObj", x = expression(a + 1), y = exp(-4:4))
        a <- -9
        eval(slot(myLang, "x"))
    })

    provs <- provs[3,]

    expect_equal(provs$provenance, "expression")
    expect_equal(provs$provenance_args, "expression(a + 1)")
    expect_equal(provs$nb_provenances, 1) 
})