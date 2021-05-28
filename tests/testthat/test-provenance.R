test_that("parse is detected", {
    f <- function() {
        x <- parse(text = "1 + 1")
        eval(x)
    }

    provs <- do_trace_provenances(f())

    expect_equal(provs$provenance, "parse")
    expect_equal(provs$provenance_args, "parse(text = \"1 + 1\") ;")
    expect_equal(provs$nb_provenances, 1)
})