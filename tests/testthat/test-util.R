test_that("sexp_typeof works for NULL", {
  expect_equal(sexp_typeof(NULL), 0)
})

test_that("get_ast_size", {
    expect_equal(get_ast_size(NULL), 1)

    expect_equal(get_ast_size(1), 1)

    expect_equal(get_ast_size(1L), 1)

    expect_equal(get_ast_size(quote(f(1,2,3))), 4)

    expect_equal(get_ast_size(list(1,2,3)), 1)

    x <- 1:1000000
    expect_equal(get_ast_size(x), 1)

    expect_equal(get_ast_size(function(x) x + 1), 1)

    ## NOTE: function asts contain a hidden 4th element that is being counted here
    expect_equal(get_ast_size(quote(function(x) x + 1)), 6)

    ## TODO: function asts contain a hidden 4th element that is being counted here
    expect_equal(get_ast_size(expression(x = 1 + 2, y = 8 + 9)), 7)
})
