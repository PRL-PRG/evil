#include "normalization.h"
#include "r_init.h"

/* Is the target string in the array of strings? */
int in(const char* target, const char** array, int array_length) {
    for (int i = 0; i < array_length; i++) {
        if (strcmp(array[i], target) == 0) {
            return 1;
        }
    }
    return 0;
}

const char* from_normalized_type(normalized_type ntype) {
    static const char* ntypes[] = {"NUM",
                                   "BOOL",
                                   "STR",
                                   "OP",
                                   "LOGI",
                                   "COMP",
                                   "VAR",
                                   "ENV",
                                   "WREF",
                                   "PTR",
                                   "NULL",
                                   "STROP",
                                   "LISTVEC",
                                   "MODEL.FRAME",
                                   "::",
                                   "FUNCTION",
                                   "PAREN",
                                   "NA",
                                   "OTHER"};
    return ntypes[ntype];
}


/* Hold a sequence of characters. */
class CharBuff  {
  char* buf; // character buffer, no 0 terminated
  int   len; // number of valid characters
  int   size; // number of allocated chars
public:

  CharBuff(int sz) {
    len = 0;
    size = sz;
    buf = (char*) malloc(size * sizeof(char));
  }

  ~CharBuff() { free(buf); }

  /* Check if the buffer can take increment new elements, realloc if needed. */
  void growIfNeeded(int increment) {
    int target = len + increment;
    if (target < size) return;
    size = size * 2 + increment;
    buf = (char*) realloc(buf, size * sizeof(char));
    if (!buf) error("Alloc failed.");
  }

  /* Add a string to the end of the buffer, grow if needed. */
  void write(const char* str) {
    int l = 0;
    while (str[l] != '\0') l++;
    growIfNeeded(l);
    for(int i=0; i < l; i++) buf[len++] = str[i];
  }

  /* Go back to an old state. */
  void rollback(int old) {  len = old ;  }

  /* Returns the number of char in the buffer */
  int pos() { return len; }

  /* Returns a zero terminated char buffer. Do not write after calling get. */
  char* get() {
    buf[len] = '\0';
    return buf;
  }
};


#define NB_ARITH_OP 15
#define NB_STR_OP 3
#define NB_COMP_OP 6
#define NB_BOOL_OP 5
#define NB_LISTVEC 2

normalized_type normalize_expr(SEXP ast,
                               CharBuff* buffer,
                               normalized_type previous_ntype,
                               int merge) {
    static const char* arith_op[NB_ARITH_OP] = {"/",
                                                "-",
                                                "*",
                                                "+",
                                                "^",
                                                "log",
                                                "sqrt",
                                                "exp",
                                                "max",
                                                "min",
                                                "cos",
                                                "sin",
                                                "abs",
                                                "atan",
                                                ":"};

    static const char* str_op[NB_STR_OP] = {"paste", "paste0", "str_c"};

    static const char* cmp_op[NB_COMP_OP] = {"<", ">", "<=", ">=", "==", "!="};

    static const char* bool_op[NB_BOOL_OP] = {"&", "&&", "|", "||", "!"};

    static const char* listvec[NB_LISTVEC] = {"list", "c"};

    switch (TYPEOF(ast)) {
    case NILSXP: {
      if (!merge || (previous_ntype != N_Null && previous_ntype != N_NA)) 
	buffer->write("NULL");
      return N_Null;
    }

    case INTSXP:
    case REALSXP:
    case CPLXSXP: {
      if (!merge || (previous_ntype != N_Num && previous_ntype != N_NA)) 
	buffer->write("NUM");
      return N_Num;
    }

    case SYMSXP: {
        if (previous_ntype == N_Function) { // This is a symbol from a function call
            if (in(CHAR(PRINTNAME(ast)), arith_op, NB_ARITH_OP)) {
	      buffer->write("OP");
                return N_Op;
            } else if (in(CHAR(PRINTNAME(ast)), bool_op, NB_BOOL_OP)) {
		  buffer->write("LOGI");
                return N_Logi;
            } else if (in(CHAR(PRINTNAME(ast)), cmp_op, NB_COMP_OP)) {
                buffer->write("COMP");
                return N_Comp;
            } else if (in(CHAR(PRINTNAME(ast)), str_op, NB_STR_OP)) {
                buffer->write(CHAR(PRINTNAME(ast)));
                return N_StrOp;
            } else if (in(CHAR(PRINTNAME(ast)), listvec, NB_LISTVEC)) {
                buffer->write(CHAR(PRINTNAME(ast)));
                return N_ListVec;
            } else if (strcmp(CHAR(PRINTNAME(ast)), "model.frame") == 0) {
                buffer->write("model.frame");
                return N_ModelFrame;
            } else if (ast == R_DoubleColonSymbol) {
                // We want to get rid of the namespaces
                return N_Namespace;
            } else if (strcmp(CHAR(PRINTNAME(ast)), "(") == 0) {
                // We want to get rid of this parenthesis operator. (a) => a
                return N_Paren;
            } else {
                buffer->write(CHAR(PRINTNAME(ast)));
                return N_Other;
            }
        } else {
            if (!merge || previous_ntype != N_Var) {
                buffer->write("VAR");
            }
            return N_Var;
        }
    }

    case LGLSXP: {
        // NA by default is boolean, but people write if for other types.
        // So we coerce it to the previous ntype
        // TODO: handle the case when NA is the first element
        // Probably just emit a new nytpe N_NA
        if (asLogical(ast) == NA_LOGICAL &&
            (previous_ntype == N_Num || previous_ntype == N_String)) {
            return previous_ntype;
        } else if (!merge ||
                   (previous_ntype != N_Boolean && previous_ntype != N_NA)) {
            buffer->write("BOOL");
        }
        if (asLogical(ast) == NA_LOGICAL) {
            return N_NA; // Will be coerced to what is needed with the next
                         // element
        }
        return N_Boolean;
    }

    case STRSXP: {
        // This comes from the parser and so there is always only one CHARSXP in
        // the character vector
        const char* s = CHAR(STRING_ELT(ast, 0));

        if (strcmp(s, "<ENVIRONMENT>") == 0) {
            if (previous_ntype != N_Env) {
                buffer->write("ENV");
            }
            return N_Env;
        } else if (strcmp(s, "<WEAK REFERENCE>") == 0) {
            if (previous_ntype != N_WRef) {
                buffer->write("WREF");
            }
            return N_WRef;
        } else if (strcmp(s, "<POINTER>") == 0) {
            if (previous_ntype != N_Ptr) {
                buffer->write("PTR");
            }
            return N_Ptr;
        }

        if (!merge || (previous_ntype != N_String && previous_ntype != N_NA)) {
            buffer->write("STR");
        }
        return N_String;
    }

    case LISTSXP: {
        // Function call arguments!
        SEXP ptr = ast;
        buffer->write("(");
        while (ptr != R_NilValue) {
	  const char* argument_name =
	    isNull(TAG(ptr)) ? "NULL" : CHAR(PRINTNAME(TAG(ptr)));
	  
            buffer->write(argument_name);
            ptr = CDR(ptr);
            if (ptr != R_NilValue) {
                buffer->write(", ");
            }
        }
        buffer->write(")");
        return N_Other;
    }

    case LANGSXP: {
      SEXP ptr = ast;
      int old_write_pos = buffer->pos();   // Save pos in case we need to roll back
      // Function name (or anonymous function)
      normalized_type ntype_function = normalize_expr(CAR(ptr), buffer, N_Function, 0);
      
      if (ntype_function == N_Namespace) { // We skip the namespace name!
	ptr = CDDR(ptr);
	return normalize_expr(CAR(ptr), buffer, N_Function, 0);
      }
      if (ntype_function == N_Paren) {     // Ignore the superfluous parenthesis
	ptr = CDR(ptr);
	return normalize_expr(CAR(ptr), buffer, previous_ntype, 0);
      }
      if (ntype_function == N_ModelFrame) {
	// We only look at the 1st two arguments and at whether there
	// is the subset argument
	ptr = CDR(ptr);
	int i = 0;
	int isSubset = 0;
	buffer->write("(");
	while (ptr != R_NilValue) {
	  int old_write_pos = buffer->pos();
	  const char* argument_name =
	    isNull(TAG(ptr)) ? "NULL" : CHAR(PRINTNAME(TAG(ptr)));
	  isSubset = strcmp(argument_name, "subset") == 0;
	  if (i < 2 || isSubset) {
	    if (isSubset) {
	      if (i < 2) {
		// One of the two first argument is missing (both is
		// not possible)
		buffer->write("NULL, ");
	      }
	      buffer->write("subset = ");
	    }
	    normalize_expr(CAR(ptr), buffer, N_Other, 0);
	  }
	  ptr = CDR(ptr);
	  i++;
	  if (!isSubset || (ptr != R_NilValue && old_write_pos != buffer->pos())) {
	    buffer->write(", ");
	  }
	}
	// It means that subset was not part of the arguments
	if (!isSubset) {
	  if (i < 2) {
	    // One of the two first argument is missing (both is
	    // not possible)
	    buffer->write("NULL, ");
	  }
	  buffer->write("subset = NULL");
	}
	buffer->write(")");
	return N_Other;
      }
      // Will be used for constant folding
      // This is the type we expect to see according to the following
      // operators
      normalized_type ntype = N_NA;
      if (ntype_function == N_Comp || ntype_function == N_Op) {
	ntype = N_Num;
      } else if (ntype_function == N_Logi) {
	ntype = N_Boolean;
      } else if (ntype_function == N_StrOp) {
	ntype = N_String;
      }
      // Rprintf("NType function: %s\n",
      // from_normalized_type(ntype_function));
      
      buffer->write("(");
      
      // Arguments
      ptr = CDR(ptr);
      int isVar = 0; // is there at least one Var?
      int old_write_pos2 = buffer->pos();
      normalized_type ntype_arg = N_Other;
      while (ptr != R_NilValue) {
	int old_write_pos3 = buffer->pos();
	ntype_arg = normalize_expr(CAR(ptr),
				   buffer,
				   ntype_arg,
				   ntype_function == N_ListVec);

	// Rprintf("NType argument: %s\n", from_normalized_type(ntype_arg));
	
	// To merge all similar elements in a list or vector, or do
	// VAR absorption. We are at the beginning of the list or the
	// first elements were NAs
	if (ntype == N_NA && ntype_function == N_ListVec) {
	  ntype = ntype_arg;
	}
	  
	// Different ntypes, excluding VAR so no Constant Folding won't be
	// done
	if (ntype_arg != ntype && ntype_arg != N_Var && ntype_arg != N_NA) {
	  ntype = N_Other;
	}
	if (ntype_arg == N_Var) {
	  // We cannot break out of the loop
	  // VAR will absorb only if all other arguments are scalar types
	  // or VAR (Or constant folded to scalar types) But we want to
	  // keep more elaborated sub-AST
	  isVar = 1;
	}
	ptr = CDR(ptr);
	  // A bit ugly..., to not write that comma at the end
            // Another solution would be to rollback the buffer by 2 characters
            // after the loop But is it efficient? And it also feels hacky...
            // And we put a comma only if we wrote something after the previous
            // comma...
            if (ptr != R_NilValue && old_write_pos3 != buffer->pos()) {
                buffer->write(", ");
            }
        }

        // Constant folded and VAR absorption
        // VAR absorption only for the special operators
        if (ntype != N_Other &&
            (ntype_function == N_Op || ntype_function == N_Logi ||
             ntype_function == N_Comp || ntype_function == N_StrOp ||
             ntype_function == N_ListVec)) {
	  if (isVar) {
	    buffer->rollback(old_write_pos2);
	    buffer->write("VAR");
	  } else if (ntype_function == N_ListVec) {
	    buffer->rollback(old_write_pos2);
	    // If NA has not been coerced yet, it is by default a boolean
	    ntype = ntype == N_NA ? N_Boolean : ntype;
	    buffer->write(from_normalized_type(ntype));
	  } else {
	    // All the same type so we roll back to previous write position!
	    buffer->rollback(old_write_pos);
	    if (ntype_function == N_Comp) {
	      buffer->write("BOOL");
	      return N_Boolean;
	    } else { // TODO: check if it is a type that makes sense
	      buffer->write( from_normalized_type(ntype));
	      return ntype;
	    }
	  }
        }
        buffer->write(")");
        return N_Other;
    }

    case EXPRSXP: {
        int size = Rf_length(ast);
        for (int i = 0; i < size - 1; i++) {
            normalize_expr(VECTOR_ELT(ast, i), buffer, N_Other, 0);
            buffer->write("; ");
        }
        normalize_expr(VECTOR_ELT(ast, size - 1), buffer, N_Other, 0);

        return N_Other;
    }

    case DOTSXP: {
        buffer->write("...");
        return N_Other;
    }

    // The following cases should not happen ars they are not deparsable
    // The yare handled by magic values in STRSXP
    case BCODESXP: {
        buffer->write("BYTECODE");
        return N_Other;
    }

    case RAWSXP: {
        buffer->write("RAWVEC");
        return N_Other;
    }

    case EXTPTRSXP: {
        buffer->write("PRT");
        return N_Other;
    }

    case WEAKREFSXP: {
        buffer->write("WREF");
        return N_Other;
    }

    case ENVSXP: {
        buffer->write("ENV");
        return N_Other;
    }

    default:
        warning("Seeing Unexpected SEXP: %s!\n", type2char(TYPEOF(ast)));
        return N_Other;
    }

    error("Not supported");
    return N_Other;
}

#define BUF_INIT_SIZE 100
SEXP r_normalize_expr(SEXP ast) {
  CharBuff* buffer = new CharBuff(BUF_INIT_SIZE);

  // The address pointing to buffer will be changed by realloc!
  // So the initial buffer will be invalidated and we need to keep track of it
  normalize_expr(ast, buffer, N_Other, 0);
  SEXP r_value = PROTECT(mkString(buffer->get()));
  UNPROTECT(1);
  delete buffer; // mkString copies
  return r_value;
}
