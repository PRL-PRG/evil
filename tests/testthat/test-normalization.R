test_that("Constant folding", {
  expect_equal(normalize_expr(expression(1 + 1)), "0")
  expect_equal(normalize_expr(quote(1 + 1)), "0")
  expect_equal(normalize_expr(quote(TRUE && FALSE)), "0")
  expect_equal(normalize_expr(quote(paste0("Hello, ", "World"))), "S")
  expect_equal(normalize_expr(quote(x)), "X")
  expect_equal(normalize_expr(quote(x + y)), "X")
  expect_equal(normalize_expr(quote(x + 1)), "X")
  expect_equal(normalize_expr(quote(mean(1))), "0")
})

test_that("List simplification", {
  expect_equal(normalize_expr(quote(f(g(1),g(1)))), "f(g(0))")
  expect_equal(normalize_expr(quote((g()+h()-y()))), "OP(OP(g(), h()), y())")
  expect_equal(normalize_expr(quote(x$y)), "X$")
  expect_equal(normalize_expr(quote(x<-f(y))), "X <- f(X)")
  expect_equal(normalize_expr(quote(x$y$z)), "X$$X") # Yuck!!!!
  expect_equal(normalize_expr(quote(c(1, 2, 3, 4))), "0")
  expect_equal(normalize_expr(quote(list(1, 2, 3, 4))), "0")
  expect_equal(normalize_expr(quote(c(TRUE, FALSE, FALSE, TRUE))), "0")
  expect_equal(normalize_expr(quote(c("a", "e", "i", "o"))), "S")
  expect_equal(normalize_expr(quote(list(1, "hello"))), "S")
  expect_equal(normalize_expr(quote(c(1, 2, x, 4))), "X")
})

test_that("Constructors", {
  expect_equal(normalize_expr(quote(integer(0))), "0")
  expect_equal(normalize_expr(quote(numeric(0))), "0")
  expect_equal(normalize_expr(quote(double(0))), "0")
  expect_equal(normalize_expr(quote(character(0))), "S")
})


test_that("Stress test", {
  # Stress test: more than 100 characters to allocate
  # 6 x pattern 1,  "test", TRUE, "test2",
  expect_equal(
    normalize_expr(quote(list(
      1,  "test", TRUE, "test2",
      1,  "test", TRUE, "test2",
      1,  "test", TRUE, "test2",
      1,  "test", TRUE, "test2",
      1,  "test", TRUE, "test2",
      1,  "test", TRUE, "test2"
    ))),
    "S"
  )
})

test_that("Anonymous functions", {
  expect_equal(normalize_expr(quote((function() 1)(2))), "function(0)(0)")
  expect_equal(normalize_expr(quote((function(a) a + 1)(2))), "function((ARGS), X)(0)")
  expect_equal(normalize_expr(quote((function(a, b) a + b)(2))), "function((ARGS), X)(0)")
})

test_that("Superfluous parenthesis", {
  expect_equal(normalize_expr(quote((1))), "0")
  expect_equal(normalize_expr(quote((1) + 2)), "0")
  expect_equal(normalize_expr(quote(c(1, (2)))), "0")
})

test_that("NA coercion", {
  expect_equal(normalize_expr(quote(c(1, NA))), "0")
  expect_equal(normalize_expr(quote(c(1, NA, NA))), "0")
  expect_equal(normalize_expr(quote(c(NA, 1))), "0")
  expect_equal(normalize_expr(quote(1 + NA)), "0")
  expect_equal(normalize_expr(quote(NA + 1)), "0")
  expect_equal(normalize_expr(quote(NA + NA + 1)), "0")
  expect_equal(normalize_expr(quote(c(NA))), "NA")
  expect_equal(normalize_expr(quote(NA)), "NA")
})

test_that("model.frame", {
  expect_equal(normalize_expr(quote(model.frame(x ~ y))), "model.frame(~(X))")
  expect_equal(normalize_expr(quote(model.frame(x ~ y, NULL))), "model.frame(~(X), NULL)")
  expect_equal(normalize_expr(quote(model.frame(x ~ y, subset = t))), "model.frame(~(X), X)")
})

test_that("Blocks", {
  expect_equal(normalize_expr(quote({})), "{()")
  expect_equal(normalize_expr(quote({sin(1 + 1)})), "0")
  expect_equal(normalize_expr(quote({1+1; x * 2})), "X") # We still subsumes in blocks
  expect_equal(normalize_expr(quote({f(1); x * 2})), "{MANY()")
})


test_that("Various normalization", {
  # Crushing consecutive same types in c and list1
  expect_equal(
    normalize_expr(quote(c(1, 2, 3, "hi", "test", 4, "true"))),
    "S"
  )

  # Namespace name is ignored
  expect_equal(
    normalize_expr(quote(a::b(4))), "b(0)"
  )

  expect_equal(
    normalize_expr(quote(rgt::`+`(1, 2))), "0"
  )

  expect_equal(
    normalize_expr(quote(plop::test(1, 2))), "test(0)"
  )
})

test_that("Structure is  simplified", {
  expect_equal(normalize_expr(quote(structure(x, tag = "plop"))), "X")
  expect_equal(normalize_expr(quote(structure(f(1 + 1), tag = "plop"))), "structure(f(0), S)")
  expect_equal(normalize_expr(quote(structure(123, tag = r))), "X")
})

