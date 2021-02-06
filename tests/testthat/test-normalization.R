test_that("Constant folding", {
  expect_equal(normalize_expr(expression(1 + 1)), "0")
  expect_equal(normalize_expr(quote(1 + 1)), "0")
  expect_equal(normalize_expr(quote(TRUE && FALSE)), "0")
  expect_equal(normalize_expr(quote(paste0("Hello, ", "World"))), "\"C\"")
  expect_equal(normalize_expr(quote(x)), "X")
  expect_equal(normalize_expr(quote(x + y)), "X")
  expect_equal(normalize_expr(quote(x + 1)), "X")
})

test_that("List simplification", {
  expect_equal(normalize_expr(quote(c(1, 2, 3, 4))), "0")
  expect_equal(normalize_expr(quote(list(1, 2, 3, 4))), "0")
  expect_equal(normalize_expr(quote(c(TRUE, FALSE, FALSE, TRUE))), "0")
  expect_equal(normalize_expr(quote(c("a", "e", "i", "o"))), "\"C\"")
  expect_equal(normalize_expr(quote(list(1, "hello"))), "\"C\"")
  expect_equal(normalize_expr(quote(c(1, 2, x, 4))), "X")
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
    "\"C\""
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
  # That can be tricky!
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
  # Special treatment!
  expect_equal(normalize_expr(quote(model.frame(x ~ y))), "model.frame(~(X))")
  expect_equal(normalize_expr(quote(model.frame(x ~ y, NULL))), "model.frame(~(X), NULL)")
  expect_equal(normalize_expr(quote(model.frame(x ~ y, subset = t))), "model.frame(~(X), X)")
})

test_that("Various normalization", {
  # Crushing consecutive same types in c and list1
  expect_equal(
    normalize_expr(quote(c(1, 2, 3, "hi", "test", 4, "true"))),
    "\"C\""
  )

  # Namespace name is ignored
  expect_equal(
    normalize_expr(quote(a::b(4))), "b(0)"
  )

  expect_equal(
    normalize_expr(quote(rgt::`+`(1, 2))), "0"
  )
})
