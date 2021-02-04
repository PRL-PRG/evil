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

const char* ntypes[] = {"NUM", "BOOL", "STR", "OP", "LOGI", "COMP",
  "VAR", "ENV", "WREF", "PTR", "NULL", "STROP", "LISTVEC", "MODEL.FRAME",
  "::", "FUNCTION", "PAREN", "NA", "OTHER"};

#define NB_ARITH_OP 15
#define NB_STR_OP 3
#define NB_COMP_OP 6
#define NB_BOOL_OP 5
#define NB_LISTVEC 2

const char* arith_op[NB_ARITH_OP] = {"/",  "-",  "*", "+", "^",
   "log", "sqrt", "exp", "max", "min", "cos", "sin", "abs", "atan", ":"};
const char* str_op[NB_STR_OP] = {"paste", "paste0", "str_c"};
const char* cmp_op[NB_COMP_OP] = {"<", ">", "<=", ">=", "==", "!="};
const char* bool_op[NB_BOOL_OP] = {"&", "&&", "|", "||", "!"};
const char* listvec[NB_LISTVEC] = {"list", "c"};


const char* from_normalized_type(NTYPE ntype) {
    return ntypes[ntype];
}

/* Hold a sequence of characters. */
class CharBuff  {
  char* buf; // character buffer, no 0 terminated
  int   len; // number of valid characters
  int   size; // number of allocated chars
public:

  CharBuff() {
    len = 0;
    size = 100;
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

class Normalizer {
  CharBuff buffer;

public:
  NTYPE normalize(SEXP ast, NTYPE previous, bool merge) {
    switch (TYPEOF(ast)) {
    case NILSXP:  return doNILSXP(ast, previous, merge);
    case INTSXP:
    case REALSXP:
    case CPLXSXP: return doNUMSXP(ast, previous, merge);
    case SYMSXP:  return doSYMSXP(ast, previous, merge);
    case LGLSXP:  return doLGLSXP(ast, previous, merge);
    case STRSXP:  return doSTRSXP(ast, previous, merge);
    case LISTSXP: return doLISTSXP(ast, previous, merge);
    case LANGSXP: return doLANGSXP(ast, previous, merge);
    case EXPRSXP: return doEXPRSXP(ast, previous, merge);
    case DOTSXP:  return doDEFAULT("...");
    // The following should not happen, they are not deparsable
    case BCODESXP:   return doDEFAULT("BYTECODE");
    case RAWSXP:     return doDEFAULT("RAW");
    case EXTPTRSXP:  return doDEFAULT("PTR");
    case WEAKREFSXP: return doDEFAULT("WREF");
    case ENVSXP:     return doDEFAULT("ENV");
    default:
      warning("Unexpected SEXP: %s!\n", type2char(TYPEOF(ast)));
      return N_Other;
    }
  }

  NTYPE doNILSXP(SEXP ast, NTYPE previous, bool merge) {
    if (!merge || (previous != N_Null && previous != N_NA))
      buffer.write("NULL");
    return N_Null;
  }

  NTYPE doNUMSXP(SEXP ast, NTYPE previous, bool merge) {
    if (!merge || (previous != N_Num && previous != N_NA))
      buffer.write("NUM");
    return N_Num;
  }

  NTYPE doSYMSXP(SEXP ast, NTYPE previous, bool merge) {
    if (previous == N_Function) { // This is a symbol from a function call
      const char* str = CHAR(PRINTNAME(ast));
      if (in(str, arith_op, NB_ARITH_OP)) {
        buffer.write("OP");
        return N_Op;
      } else if (in(str, bool_op, NB_BOOL_OP)) {
        buffer.write("LOGI");
        return N_Logi;
      } else if (in(str, cmp_op, NB_COMP_OP)) {
        buffer.write("COMP");
        return N_Comp;
      } else if (in(str, str_op, NB_STR_OP)) {
        buffer.write(str);
        return N_StrOp;
      } else if (in(str, listvec, NB_LISTVEC)) {
        buffer.write(str);
        return N_ListVec;
      } else if (strcmp(str, "model.frame") == 0) {
        buffer.write("model.frame");
        return N_ModelFrame;
      } else if (ast == R_DoubleColonSymbol) {  // Get rid of the namespaces
        return N_Namespace;
      } else if (strcmp(str, "(") == 0) {  // Get rid of parentheses. (a) => a
        return N_Paren;
      } else {
        buffer.write(str);
        return N_Other;
      }
    }
    if (!merge || previous != N_Var) buffer.write("VAR");
    return N_Var;
  }

  /* The default type of NA is Logial, but it is coerced to any other NA type,
     as needed. */
  NTYPE doLGLSXP(SEXP ast, NTYPE previous, bool merge) {
    int logic = asLogical(ast) == NA_LOGICAL;
    if (logic && (previous == N_Num || previous == N_String))
      return previous;
    if (!merge || (previous != N_Boolean && previous != N_NA))
      buffer.write("BOOL");
    return  (logic) ? N_NA : N_Boolean;
  }

  NTYPE doSTRSXP(SEXP ast, NTYPE previous, bool merge) {
    // This comes from the parser and so there is always only one CHARSXP in
    // the character vector
    const char* s = CHAR(STRING_ELT(ast, 0));

    if (strcmp(s, "<ENVIRONMENT>") == 0) {
      if (previous != N_Env) buffer.write("ENV");
      return N_Env;
    } else if (strcmp(s, "<WEAK REFERENCE>") == 0) {
      if (previous != N_WRef) buffer.write("WREF");
      return N_WRef;
    } else if (strcmp(s, "<POINTER>") == 0) {
      if (previous != N_Ptr) buffer.write("PTR");
      return N_Ptr;
    }
    if (!merge || (previous != N_String && previous != N_NA))
      buffer.write("STR");
    return N_String;
  }

  NTYPE doLISTSXP(SEXP ast, NTYPE previous, bool merge) {
    SEXP ptr = ast;     // Function call arguments!
    buffer.write("(");
    while (ptr != R_NilValue) {
      const char* arg_name = isNull(TAG(ptr)) ? "NULL" : CHAR(PRINTNAME(TAG(ptr)));
      buffer.write(arg_name);
      ptr = CDR(ptr);
      if (ptr != R_NilValue) buffer.write(", ");
    }
    buffer.write(")");
    return N_Other;
  }

  NTYPE doLANGSXP(SEXP ast, NTYPE previous, bool merge) {
    SEXP ptr = ast;
    int old_pos = buffer.pos(); // Save pos to roll back
    NTYPE fun_name = normalize(CAR(ptr), N_Function, 0);

    if (fun_name == N_Namespace) { // Skip the namespace
      ptr = CDDR(ptr);
      return normalize(CAR(ptr), N_Function, 0);
    } else if (fun_name == N_Paren) {  // Ignore superfluous parenthesis
      ptr = CDR(ptr);
      return normalize(CAR(ptr), previous, 0);
    } else if (fun_name == N_ModelFrame) { // Keep 2 args and subdset
      ptr = CDR(ptr);
      int i = 0;
      int isSubset = 0;
      buffer.write("(");
      while (ptr != R_NilValue) {
        int old_pos = buffer.pos();
        const char* arg_name =
          isNull(TAG(ptr)) ? "NULL" : CHAR(PRINTNAME(TAG(ptr)));
        isSubset = strcmp(arg_name, "subset") == 0;
        if (i < 2 || isSubset) {
          if (isSubset) {
            if (i < 2) // One of the two first argument is missing (both can't)
              buffer.write("NULL, ");
            buffer.write("subset = ");
          }
          normalize(CAR(ptr), N_Other, 0);
        }
        ptr = CDR(ptr);
        i++;
        if (!isSubset || (ptr != R_NilValue && old_pos != buffer.pos()))
          buffer.write(", ");
      }
      // It means that subset was not part of the arguments
      if (!isSubset) {
        if (i < 2)  // One of the two first argument is missing (both can't)
          buffer.write("NULL, ");
        buffer.write("subset = NULL");
      }
      buffer.write(")");
      return N_Other;
    }
    // Will be used for constant folding. This is the type we expect to see
    // according to the following operators
    NTYPE ntype = N_NA;
    if (fun_name == N_Comp || fun_name == N_Op) {
      ntype = N_Num;
    } else if (fun_name == N_Logi) {
      ntype = N_Boolean;
    } else if (fun_name == N_StrOp) {
      ntype = N_String;
    }
    buffer.write("(");
    // Arguments
    ptr = CDR(ptr);
    int isVar = 0; // is there at least one Var?
    int old_pos2 = buffer.pos();
    NTYPE ntype_arg = N_Other;
    while (ptr != R_NilValue) {
      int old_pos3 = buffer.pos();
      ntype_arg = normalize(CAR(ptr), ntype_arg, fun_name == N_ListVec);
      // To merge all similar elements in a list or vector, or do
      // VAR absorption. We are at the beginning of the list or the
      // first elements were NAs
      if (ntype == N_NA && fun_name == N_ListVec) {
        ntype = ntype_arg;
      }
      // Different ntypes, excluding VAR so no Constant Folding won't be done
      if (ntype_arg != ntype && ntype_arg != N_Var && ntype_arg != N_NA) {
        ntype = N_Other;
      }
      if (ntype_arg == N_Var) {
        // We cannot break out of the loop. VAR will absorb only if all other
        // arguments are scalar types or VAR (Or constant folded to scalar
        // types) But we want to keep more elaborated sub-AST
        isVar = 1;
      }
      ptr = CDR(ptr);
      // A bit ugly..., to not write that comma at the end
      // Another solution would be to rollback the buffer by 2 characters
      // after the loop But is it efficient? And it also feels hacky...
      // And we put a comma only if we wrote something after the previous
      // comma...
      if (ptr != R_NilValue && old_pos3 != buffer.pos()) {
        buffer.write(", ");
      }
    }

    // Constant folded and VAR absorption. VAR absorption only for special ops.
    if (ntype != N_Other &&
        (fun_name == N_Op || fun_name == N_Logi || fun_name == N_Comp ||
         fun_name == N_StrOp || fun_name == N_ListVec)) {
      if (isVar) {
        buffer.rollback(old_pos2);
        buffer.write("VAR");
      } else if (fun_name == N_ListVec) {
        buffer.rollback(old_pos2);
        // If NA has not been coerced yet, it is by default a boolean
        ntype = ntype == N_NA ? N_Boolean : ntype;
        buffer.write(from_normalized_type(ntype));
      } else {
        // All the same type so we roll back to previous write position!
        buffer.rollback(old_pos);
        if (fun_name == N_Comp) {
          buffer.write("BOOL");
          return N_Boolean;
        } else { // TODO: check if it is a type that makes sense
          buffer.write( from_normalized_type(ntype));
          return ntype;
        }
      }
    }
    buffer.write(")");
    return N_Other;
  }

  NTYPE doEXPRSXP(SEXP ast, int previous, bool merge) {
    int size = Rf_length(ast);
    for (int i = 0; i < size - 1; i++) {
      normalize(VECTOR_ELT(ast, i), N_Other, 0);
      buffer.write("; ");
    }
    normalize(VECTOR_ELT(ast, size - 1),  N_Other, 0);
    return N_Other;
  }

  NTYPE doDEFAULT(const char* val) {
    buffer.write(val);
    return N_Other;
  }

  char* get() { return buffer.get(); }
};


SEXP r_normalize_expr(SEXP ast) {
  Normalizer norm;
  norm.normalize(ast, N_Other, 0);
  SEXP r_value = PROTECT(mkString(norm.get()));
  UNPROTECT(1);
  return r_value;
}
