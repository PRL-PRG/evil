test_that("Constant folding", {
  expect_equal(normalize_expr(expression(1 + 1)), "NUM")
  expect_equal(normalize_expr(quote(1 + 1)), "NUM")
  expect_equal(normalize_expr(quote(TRUE && FALSE)), "BOOL")
  expect_equal(normalize_expr(quote(paste0("Hello, ", "World"))), "STR")
  expect_equal(normalize_expr(quote(x)), "VAR")
  expect_equal(normalize_expr(quote(x + y)), "OP(VAR)")
  expect_equal(normalize_expr(quote(x + 1)), "OP(VAR)")
})

test_that("List simplification", {
  expect_equal(normalize_expr(quote(c(1, 2, 3, 4))), "c(NUM)")
  expect_equal(normalize_expr(quote(list(1, 2, 3, 4))), "list(NUM)")
  expect_equal(normalize_expr(quote(c(TRUE, FALSE, FALSE, TRUE))), "c(BOOL)")
  expect_equal(normalize_expr(quote(c("a", "e", "i", "o"))), "c(STR)")
  expect_equal(normalize_expr(quote(list(1, "hello"))), "list(NUM, STR)")
  expect_equal(normalize_expr(quote(c(1, 2, x, 4))), "c(VAR)")
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
    "list(NUM, STR, BOOL, STR, NUM, STR, BOOL, STR, NUM, STR, BOOL, STR, NUM, STR, BOOL, STR, NUM, STR, BOOL, STR, NUM, STR, BOOL, STR)"
  )
})

test_that("Anonymous functions", {
  expect_equal(normalize_expr(quote((function() 1)(2))), "function(NULL, NUM, NUM)(NUM)")
  expect_equal(normalize_expr(quote((function(a) a + 1)(2))), "function((a), OP(VAR), NUM)(NUM)")
  expect_equal(normalize_expr(quote((function(a, b) a + b)(2))), "function((a, b), OP(VAR), NUM)(NUM)")
})

test_that("Superfluous parenthesis", {
  expect_equal(normalize_expr(quote((1))), "NUM")
  expect_equal(normalize_expr(quote((1) + 2)), "NUM")
  expect_equal(normalize_expr(quote(c(1, (2)))), "c(NUM)")
})

test_that("NA coercion", {
  # That can be tricky!
  expect_equal(normalize_expr(quote(c(1, NA))), "c(NUM)")
  expect_equal(normalize_expr(quote(c(1, NA, NA))), "c(NUM)")
  expect_equal(normalize_expr(quote(c(NA, 1))), "c(NUM)")
  expect_equal(normalize_expr(quote(1 + NA)), "NUM")
  expect_equal(normalize_expr(quote(NA + 1)), "NUM")
  expect_equal(normalize_expr(quote(NA + NA + 1)), "NUM")
  expect_equal(normalize_expr(quote(c(NA))), "c(BOOL)")
  expect_equal(normalize_expr(quote(NA)), "BOOL")
})

test_that("model.frame", {
  # Special treatment!
  expect_equal(normalize_expr(quote(model.frame(x ~ y))), "model.frame(~(VAR, VAR), NULL, subset = NULL)")
  expect_equal(normalize_expr(quote(model.frame(x ~ y, NULL))), "model.frame(~(VAR, VAR), NULL, subset = NULL)")
  expect_equal(normalize_expr(quote(model.frame(x ~ y, subset = t))), "model.frame(~(VAR, VAR), NULL, subset = VAR)")
})

test_that("Various normalization", {
  # Crushing consecutive same types in c and list1
  expect_equal(
    normalize_expr(quote(c(1, 2, 3, "hi", "test", 4, "true"))),
    "c(NUM, STR, NUM, STR)"
  )

  # Namespace name is ignored
  expect_equal(
    normalize_expr(quote(a::b(4))), "b(NUM)"
  )

  expect_equal(
    normalize_expr(quote(rgt::`+`(1, 2))), "NUM"
  )
})
