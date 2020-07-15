sexp_typeof <- function(x)
  .Call(C_sexp_typeof, x)

mark_parsed_expression <- function(x, parse_fun_name)
  .Call(C_mark_parsed_expression, x, parse_fun_name)
