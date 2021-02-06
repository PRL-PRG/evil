#include <vector>
#include <iostream>
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

const char* from_normalized_type(NTYPE ntype) { return ntypes[ntype]; }

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

  // ~CharBuff() { free(buf); }

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
    // This comes from the parser, there is one CHARSXP in the vector
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
      int i = 0, isSubset = 0;
      buffer.write("(");
      while (ptr != R_NilValue) {
        int old_pos = buffer.pos();
        const char* arg_name = isNull(TAG(ptr)) ? "NULL" : CHAR(PRINTNAME(TAG(ptr)));
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
      if (!isSubset) { // Subset was not part of the arguments
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
      if (ntype == N_NA && fun_name == N_ListVec)
        ntype = ntype_arg;
      // Different ntypes, excluding VAR so no Constant Folding won't be done
      if (ntype_arg != ntype && ntype_arg != N_Var && ntype_arg != N_NA)
        ntype = N_Other;
      isVar |= (ntype_arg == N_Var);
      // We cannot break out of the loop. VAR will absorb only if all other
      // arguments are scalar types or VAR (Or constant folded to scalar
       // types) But we want to keep more elaborated sub-AST
      ptr = CDR(ptr);
      // To avoid writing a  comma at the end.
      if (ptr != R_NilValue && old_pos3 != buffer.pos())
        buffer.write(", ");
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


const char* copy(const char* str) {
  int len = strlen(str) + 1;
  char* cpstr = (char*) malloc(len * sizeof(char));
  strcpy(cpstr, str);
  return cpstr;
}

/////////////////// Tree /////////////////////
class Tree {
public:
  virtual const char* print() { return "";  }
  virtual bool is_sym() { return false; }
  virtual bool is_call() { return false; }
  virtual bool is_null() { return false; }
  virtual bool is_na() { return false; }
  virtual bool is_statements() { return false; }
  virtual bool is_num() { return false; }
  virtual bool is_str() { return false; }
  virtual bool is_other() { return false; }
  virtual bool subsumes(Tree* t) { return false; }
  void write(CharBuff* buf) { buf->write(print()); }
  Tree* dup(Tree* t);
};

using Vec = std::vector<Tree*>;

////////////////  Null ///////////////////////
class Null : public Tree {

public:
  const char* print() { return "NULL"; }

  bool is_null() { return true; }

  bool subsumes(Tree* t) {
    return t->is_null() || t->is_na();
  }
};

//////////// Num ///////////////////////////
class Num : public Tree {

public:
  const char* print() { return "0"; }

  bool is_num() { return true; }

  bool subsumes(Tree* t) {
    return t->is_null() || t->is_na() || t->is_num();
  }
};

/////////////// Sym ////////////////////////
class Sym : public Tree {
  const char* name;

public:
  Sym(const char* nm) : name(copy(nm)) { }

  Sym(Sym* t) : name(copy(t->name)) { }

  ~Sym() { free((char*)name); }

  bool is_sym() { return true; }

  /* All symbols are normalized to 'X'. */
  const char* print() { return "X"; }

  /* This is used while printing function names. */
  const char* get_name() { return name; }

  bool subsumes(Tree* t) {
    return t->is_sym() || t->is_num() || t->is_na() || t->is_null() || t->is_str();
  }

};

//////////////// NA ////////////////////////
class NA : public Tree {

public:
  const char* print() { return "NA"; }

  bool subsumes(Tree* t) { return t->is_na(); }

  bool is_na() { return true; }
};

/////////////// Call ////////////////////////
class Call : public Tree{
  Sym* name; // name without namespace, could be null
  Tree* anon; // Expression giving a function (for anon)
  int opkind; // 0=Unknown, 1=model.frame, 2=arith, 3=logic, 4=named, 5=c list
  Vec args;
  const char* string_rep = nullptr;

public:
  Call(Sym* nm, Tree* anon) : name(nm), anon(anon) {
    if (!name) opkind = 0;
    else {
      const char* str = name->get_name(); // real name
      if (in(str, arith_op, NB_ARITH_OP))    opkind = 2;
      else if (in(str, bool_op, NB_BOOL_OP)) opkind = 3;
      else if (in(str, cmp_op, NB_COMP_OP))  opkind = 3;
      else if (in(str, str_op, NB_STR_OP))   opkind = 3;
      else if (in(str, listvec, NB_LISTVEC)) opkind = 5;
      else if (!strcmp(str, "model.frame"))  opkind = 1;
      else                                   opkind = 4;
    }
  }

  Call(Sym* nm, Tree* anon, Vec nargs) : Call(nm, anon) {
    add_args(nargs);
  }

  Call(Call* x) {
    name = (x->name) ? x->name : nullptr;
    anon = dup(x->anon);
  }

  Call(Call* x, Vec newargs) : Call(x) {
    add_args(newargs);
  }

  ~Call() {
    delete anon; // should delete name, since we got name off it in build()
    if(string_rep) free((char*)string_rep);
  }

  void add_args(Vec newargs) {
    for(Tree* t : newargs) args.push_back(t);
  }

  int args_len() { return args.size(); }

  Vec get_args() { return args; }

  int kind() { return opkind; }

  bool is_call() { return true; }

  Tree* get_anon() { return anon; }

  bool is_name(const char* nm) {
    if (!name) return false;
    return strcmp(nm, name->get_name()) == 0;
  }

  const char* get_name() {
    if (name) return name->get_name();
    else return anon->print();
  }

  const char* print() {
    if (!string_rep) {
      CharBuff buf;
      if(name)
	buf.write(name->get_name());
      else
	anon->write(&buf);
      buf.write("(");
      int pos = -1;
      for(Tree* x : args) {
	buf.write(x->print());
	pos = buf.pos();
	buf.write(", ");
      }
      if (pos!=-1) buf.rollback(pos);
      buf.write(")");
      string_rep = buf.get();
    }
    return string_rep;
  }

  void add_arg(Tree* a) { args.push_back(a); }

  Tree* get_arg(int x) { return args[x]; }
};

///////////// Str ////////////////////////////
class Str : public Tree{

public:
  const char* print() { return "\"C\""; }

  bool is_str() { return true; }

  bool subsumes(Tree* t) {
    return t->is_str() || t->is_na() || t->is_null() || t->is_num();
  }

};

///////////// Statements /////////////////////
class Statements : public Tree {
  Vec elems;
  const char* string_rep = nullptr;

public:
  Statements() {}

  Statements(Vec v) { for(Tree* x : v) add(x); }

  Statements(Statements* x) : Statements(x->elems){  }

  ~Statements() {
    for(Tree* x : elems) delete x;
    if (string_rep) free((char*)string_rep);
  }

  Vec get_elems() { return elems; }

  void add(Tree* x) { elems.push_back(x); }

  bool is_statements() { return true; }

  bool subsumes(Tree* t) { return false;  }

  const char* print() {
    if (!string_rep) {
      CharBuff buf;
      buf.write("{ ");
      int pos = -1;
      for(Tree* x : elems) {
	buf.write(x->print());
	pos = buf.pos();
	buf.write("; ");
      }
      if (pos!=-1) buf.rollback(pos);
      buf.write(" }");
      string_rep = buf.get();
    }
    return string_rep;
  }
};

////////////// Other /////////////////////
class Other : public Tree {
  const char* name;

public:
  Other(const char* nm) : name(nm) { }

  Other(Other* x) : name(copy(x->name)) { }

  ~Other() { /*Do not delete name*/ }

  bool is_other() { return true; }

  const char* print() { return name; }
};


//////////////////////////////////////////

Tree* Tree::dup(Tree* t) {
  if (t->is_sym()) return new Sym(dynamic_cast<Sym*>(t));
  else if (t->is_call()) return new Call(dynamic_cast<Call*>(t));
  else if (t->is_null()) return new Null();
  else if (t->is_na()) return new NA();
  else if (t->is_statements()) return new Statements(dynamic_cast<Statements*>(t));
  else if (t->is_num()) return new Num();
  else if (t->is_str()) return new Str();
  else if (t->is_other()) return new Other(dynamic_cast<Other*>(t));
}


////////////// Builder /////////////////////
class Builder {

public:
  Tree* build(SEXP ast) {
    switch (TYPEOF(ast)) {
    case NILSXP:  return new Null();
    case INTSXP:
    case REALSXP:
    case CPLXSXP: return new Num();
    case SYMSXP:  return  new Sym(CHAR(PRINTNAME(ast)));
    case LGLSXP:  return asLogical(ast)==NA_LOGICAL ?
	((Tree*) new NA()) : ((Tree*) new Num());
    case STRSXP:  return doSTRSXP(ast);
    case LISTSXP: return doLISTSXP(ast);
    case LANGSXP: return doLANGSXP(ast);
    case EXPRSXP: return doEXPRSXP(ast);
    case DOTSXP:     return doDEFAULT("...");
    case BCODESXP:   return doDEFAULT("BYTECODE");
    case RAWSXP:     return doDEFAULT("RAW");
    case EXTPTRSXP:  return doDEFAULT("PTR");
    case WEAKREFSXP: return doDEFAULT("WREF");
    case ENVSXP:     return doDEFAULT("ENV");
    default:
      error("Unexpected SEXP: %s!\n", type2char(TYPEOF(ast)));
    }
  }

  Tree* doSTRSXP(SEXP ast) {
    const char* s = CHAR(STRING_ELT(ast, 0));
    if (!strcmp(s, "<ENVIRONMENT>") || !strcmp(s, "<WEAK REFERENCE>") ||
        !strcmp(s, "<POINTER>"))
      return new Other(s);
    else
      return new Str();
  }

  Tree* doLISTSXP(SEXP ast) { return new Other("(ARGS)"); }

  Tree* doLANGSXP(SEXP ast) {
    Tree* fun = build(CAR(ast)); // Function 'name' can be (1) a
    Sym* fun_name = nullptr;   // symbol, (2) namespace (langsxp),
    // (3) anonymous function (langsxp). For (1) and (2) we keep
    // function names, for (3) we retain the whole expression.
    if (fun->is_sym()) fun_name = dynamic_cast<Sym*>(fun);
    else if (fun->is_call()) {
      Call* name_call = dynamic_cast<Call*>(fun);
      if (name_call->is_name("::")) {
	fun_name = dynamic_cast<Sym*>(name_call->get_arg(1));
      }
    }

    Call* call = new Call(fun_name, fun);
    // Process the arguments
    for (SEXP ptr = CDR(ast); ptr != R_NilValue; ptr = CDR(ptr)) {
      call->add_arg(build(CAR(ptr)));
    }
    return call;
  }

  Tree* doEXPRSXP(SEXP ast) {
    Statements* stmts = new Statements();
    int size = Rf_length(ast);
    for (int i = 0; i < size; i++)
      stmts->add(build(VECTOR_ELT(ast, i)));
    return stmts;
  }

  Tree* doDEFAULT(const char* val) { return new Other(val);  }

};

///////////////////// Simplifier /////////////////////////
class Simplifier {

public:
  Tree* simplify(Tree* t) {
    if (t->is_sym()) return new Sym(dynamic_cast<Sym*>(t));
    else if (t->is_call()) return doCall(dynamic_cast<Call*>(t));
    else if (t->is_null()) return new Null();
    else if (t->is_na()) return new NA();
    else if (t->is_statements()) return doStatements(dynamic_cast<Statements*>(t));
    else if (t->is_num()) return new Num();
    else if (t->is_str()) return new Str();
    else if (t->is_other()) return new Other(dynamic_cast<Other*>(t));
    error("Not reached.");
  }

  Tree* doCall(Call* x) {
    Vec args;
    for(Tree* t : x->get_args()) args.push_back(simplify(t));
    args = subsume(args);

    if (x->kind() == 0) { // anon function
      Tree* anon = simplify(x->get_anon());
      return new Call(nullptr, anon, args);
    } else if (x->kind() == 1) { // model.frame
      return new Call(x, args);
    } else if  (x->kind() == 2||x->kind() == 3) { // arith | logic
      if (args.size() == 1) return args[0];
      Sym* op = new Sym("OP");
      return new Call(op, op, args);
    } else if  (x->kind() == 4) { // named function
      if (x->is_name("(") && args.size() == 1) return args[0];
      return new Call(x, args);
    } else if  (x->kind() == 5) { // c() or list()
      if (args.size() == 1) return args[0];
      else return new Call(x, args);
    }
  }

  std::vector<Tree*> subsume(std::vector<Tree*> v) {
    std::vector<Tree*> r;
    int len = v.size();
    for (int i=0; i<len; i++) {
      Tree* t = v[i];
      bool added = false;
      for (int j=0; j<r.size(); j++) {
        bool tr = t->subsumes(r[j]);
        bool rt = r[j]->subsumes(t);
        added = (tr || rt);
        if (tr) r[j] = t;
        if (added) break;
      }
      if (!added) r.push_back(t);
    }
    return r;
  }

  Tree* doStatements(Statements* x) {
    Vec elems;
    for(Tree* t: x->get_elems())
      elems.push_back(simplify(t));
    elems = subsume(elems);
    if (elems.size() == 1) return elems[0];
    return new Statements(elems);
  }
};


///////////////////////////////////////////////////////////
SEXP r_normalize_expr(SEXP ast) {
  Normalizer norm;
  norm.normalize(ast, N_Other, 0);

  Builder builder;
  Tree* t = builder.build(ast);
  CharBuff buf;
  t->write(&buf);
  std::cout << buf.get() << std::endl;

  Simplifier s;
  Tree* t2 = s.simplify(t);
  CharBuff buf2;
  t2->write(&buf2);
  std::cout << buf2.get() << std::endl;

  SEXP r_value = PROTECT(mkString(norm.get()));
  UNPROTECT(1);
  return r_value;
}
