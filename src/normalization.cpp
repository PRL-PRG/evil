#include <vector>
#include <iostream>
#include <cassert>
#include <array>
#include "normalization.h"
#include "r_init.h"

const char* copy(const char* str) {
  int len = strlen(str) + 1;
  char* cpstr = (char*) malloc(len * sizeof(char));
  strcpy(cpstr, str);
  return cpstr;
}

bool eq(const char* str, const char* str2) {
  if (str == str2) return true;
  if (str == nullptr || str2 == nullptr) return false;
  return strcmp(str, str2) == 0;
}

/* Is the target string in the array of strings? */
bool in(const char* target, const char** array, int array_length) {
    for (int i = 0; i < array_length; i++)
        if (eq(array[i], target)) return true;
    return false;
}

#define NB_ARITH_OP 35
#define NB_STR_OP 5
#define NB_COMP_OP 6
#define NB_BOOL_OP 5
#define NB_LISTVEC 2

static const char* arith_op[NB_ARITH_OP] = {"/",  "-",  "*", "+", "^", "log", "sqrt",
  "exp", "max", "min", "cos", "sin", "abs", "atan", ":", "mean", "atanh", "sd",
  "round", "ceiling", "floor", "trunc," "median", "pmin", "pmax", "log10", "log1p",
  "log2", "tan", "asin", "cosh", "sinh", "acos", "sign", "atan2", "sum"};
static const char* str_op[NB_STR_OP] = {"paste", "paste0", "str_c",  "toupper",
  "tolower"};
static const char* cmp_op[NB_COMP_OP] = {"<", ">", "<=", ">=", "==", "!="};
static const char* bool_op[NB_BOOL_OP] = {"&", "&&", "|", "||", "!"};
static const char* listvec[NB_LISTVEC] = {"list", "c"};


/* Hold a sequence of characters. */
class CharBuff  {
  char* buf;        // character buffer, not 0 terminated
  int   len = 0;    // number of valid characters
  int   size = 100; // number of allocated chars
public:

  CharBuff() { buf = (char*) malloc(size * sizeof(char)); }

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
  char* get() { buf[len] = '\0'; return buf; }
};

class Call;           // forward declarations
class Statements;
class Other;

/////////////////// Exp /////////////////////
class Exp {
public:
  /* If the tree needs to allocate a string representation, it is cached here
   * and will be freed when tree is freed. */
  const char* string_rep = nullptr;
  inline static bool unify_values = false;

  Exp() {}
  /* Copy the string to make sure we own it. */
  Exp(const char* n) : string_rep(copy(n)) {}

  ~Exp() { if(string_rep) free((char*)string_rep); }

  virtual bool is_sym() { return false; }
  virtual bool is_call() { return false; }
  virtual bool is_null() { return false; }
  virtual bool is_na() { return false; }
  virtual bool is_statements() { return false; }
  virtual bool is_num() { return false; }
  virtual bool is_str() { return false; }
  virtual bool is_other() { return false; }

  virtual bool eq_call(Call* c) { return false;  }
  virtual bool eq_statements(Statements* s) { return false;  }
  virtual bool eq_other(Other* o) { return false;  }

  /* Is this more general than that? For example, Num subsumes NA. */
  virtual bool subsumes(Exp* that) { return false; }

  /* Write tree to buffer. */
  void write(CharBuff* buf, bool _unify_values=false) {
      unify_values = _unify_values;
    buf->write(print());
    }

  /* Return tree as string. */
  virtual const char* print() { return "";  }

  /* Deep copy of the tree. */
  Exp* dup(Exp* t);

  /* Two exps are equal */
  bool equals(Exp* t);
};

using Vec = std::vector<Exp*>;

////////////////  Null ///////////////////////
class Null : public Exp {

public:
  const char* print() { return Exp::unify_values? "V" : "NULL"; }

  bool is_null() { return true; }

  // Null << { Null,  Na }
  bool subsumes(Exp* t) { return t->is_null() || t->is_na(); }
};

//////////// Num ///////////////////////////
/* Num represents Int, Double, Cmplx, and Logical */
class Num : public Exp {

public:
  const char* print() { return Exp::unify_values? "V" : "0"; }

  bool is_num() { return true; }

  // Num << {Null, NA, Num}
  bool subsumes(Exp* t) { return t->is_null() || t->is_na() || t->is_num(); }
};

/////////////// Sym ////////////////////////
class Sym : public Exp {

public:
  Sym(const char* nm) : Exp(nm) { }

  Sym(Sym* t) : Exp(t->string_rep) { }

  bool is_sym() { return true; }

  /* All symbols are normalized to 'X'. */
  const char* print() { return "X"; }

  /* This is used while printing function names. */
  const char* get_name() { return string_rep; }

  // Sym << {Sym, Num, NA, Null, Str}
  // Rationale, a Sym could hold any of those
  bool subsumes(Exp* t) {
    return t->is_sym() || t->is_num() || t->is_na() || t->is_null() || t->is_str();
  }

};

//////////////// NA ////////////////////////
class NA : public Exp {

public:
  const char* print() { return Exp::unify_values? "V" : "NA"; }

  // NA << NA
  bool subsumes(Exp* t) { return t->is_na(); }

  bool is_na() { return true; }
};

/////////////// Call ////////////////////////
class Call : public Exp{
  Sym* name; // name without namespace, could be null
  Exp* anon; // Expression giving a function (for anon)
  OpKind opkind; // 0=Unknown, 1=model.frame, 2=arith, 3=logic, 4=named, 5=c list
  Vec args;

public:
  /*  A function may not have a nm, but always has an anon, which is the
   *  expression that yields a function value. If it has a name we try to assign
   *  it a kind. */
  Call(Sym* nm, Exp* anon) : name(nm), anon(anon) {
    if (!name) opkind = UnknownOp;
    else {
      const char* str = name->get_name(); // real name
      if (in(str, arith_op, NB_ARITH_OP))    opkind = ArithOp;
      else if (in(str, bool_op, NB_BOOL_OP)) opkind = LogicOp;
      else if (in(str, cmp_op, NB_COMP_OP))  opkind = LogicOp;
      else if (in(str, str_op, NB_STR_OP))   opkind = LogicOp;
      else if (in(str, listvec, NB_LISTVEC)) opkind = ListVecOp;
      else if (eq(str, "model.frame"))       opkind = ModelFrameOp; // model.matrix?
      else                                   opkind = NamedOp;
    }
  }

  Call(Sym* nm, Exp* anon, Vec nargs) : Call(nm, anon) { add_args(nargs); }

  /* Copy constructor, deep copies that */
  Call(Call* that) {
    name = (that->name) ? dynamic_cast<Sym*>(dup(that->name)) : nullptr;
    anon = dup(that->anon);
    add_args(that->args);
  }

  Call(Call* x, Vec newargs) : Call(x->name, x->anon, newargs) { }

  ~Call() { delete anon; } // should delete name, since we got name off it in build()

  void add_args(Vec newargs) { for(Exp* t : newargs) args.push_back(t); }

  const Vec& get_args() const { return args; }
  Vec& get_args() { return args; }

  OpKind kind() { return opkind; }

  bool is_call() { return true; }

  bool eq_call(Call* c) {
    if (name && c->name) {
      if (!eq(name->get_name(),c->name->get_name())) return false;
    } else if (!name && !c->name) {
      //ok
    } else return false;

    if (!anon->equals(c->anon)) return false;

    int len = args.size();
    int len2 = c->args.size();
    if (len != len2) return false;

    for(int i=0; i<len; i++)
      if (!args[i]->equals(c->args[i])) return false;
    return true;
  }

  // A Call susbsumes structurally equal calls.
  bool subsumes(Exp* t) {
    if (t->is_call()) return equals(t);
    else return false;
  }

  Exp* get_anon() { return anon; }

  bool eq_name(const char* nm) { return name? eq(nm, name->get_name()) : false; }

  const char* get_name() { return name? name->get_name() : "anon"; }

  const char* print() {
    if (string_rep) return string_rep;
    int len = args.size();
    CharBuff buf;
    const char* op = nullptr;
    if (eq_name("<-")) {
      buf.write(args[0]->print());
      buf.write(" <- ");
      if (len == 2) buf.write(args[1]->print());
    } else if (eq_name("$")) {
      buf.write(args[0]->print());
      buf.write("$");
      if (len == 2) buf.write(args[1]->print());
    } else if (eq_name("[")) {
      buf.write(args[0]->print());
      buf.write("[$");
      int pos = -1;
      for(int i=1; i<len; i++) {
        buf.write(args[i]->print());
        pos = buf.pos();
        buf.write(", ");
      }
      if (pos!=-1) buf.rollback(pos);
      buf.write("]");
    } else {
      if(name) buf.write(name->get_name());
      else anon->write(&buf);
      buf.write("(");
      int pos = -1;
      for(Exp* x : args) {
        buf.write(x->print());
        pos = buf.pos();
        buf.write(", ");
      }
      if (pos!=-1) buf.rollback(pos);
      buf.write(")");
    }
    return string_rep = copy(buf.get()); // buf deletes its string, so must copy
  }

  void add_arg(Exp* a) { args.push_back(a); }

  Exp* get_arg(int x) { return args[x]; }
};

///////////// Str ////////////////////////////
class Str : public Exp{

public:
  const char* print() { return Exp::unify_values? "V" : "S"; }

  bool is_str() { return true; }

  // Str << Str, NA, Null, Num
  bool subsumes(Exp* t) {
    return t->is_str() || t->is_na() || t->is_null() || t->is_num();
  }

};

///////////// Statements /////////////////////
class Statements : public Exp {
  Vec elems;

public:
  Statements() {}

  Statements(Vec v) { for(Exp* x : v) add(x); }

  Statements(Statements* x) : Statements(x->elems){  }

  ~Statements() { for(Exp* x : elems) delete x;  }

  Vec get_elems() { return elems; }

  void add(Exp* x) { elems.push_back(x); }

  bool is_statements() { return true; }

  bool eq_statements(Statements* s) {
    int len = elems.size();
    int len2 = s->elems.size();
    if (len != len2) return false;
    for (int i=0; i<len; i++)
      if (!elems[i]->equals(s->elems[i]))
        return false;
    return true;
  }

  // Statements do not subsume
  bool subsumes(Exp* t) { return false;  }

  const char* print() {
    if (string_rep) return string_rep;
    CharBuff buf;
    buf.write("{ ");
    int pos = -1;
    for(Exp* x : elems) {
      buf.write(x->print());
      pos = buf.pos();
      buf.write("; ");
    }
    if (pos!=-1) buf.rollback(pos);
    buf.write(" }");
    return string_rep = copy(buf.get());
  }
};

////////////// Other /////////////////////
class Other : public Exp {

public:
  Other(const char* nm) : Exp(nm) { }

  Other(Other* x) : Exp(x->string_rep) { }

  bool is_other() { return true; }

  const char* print() { return string_rep; }

  bool eq_other(Other* o) {
    if ( string_rep && o->string_rep)
      return eq(string_rep, o->string_rep);
    else return false;
  }
};
//////////////////////////////////////////

Exp* Exp::dup(Exp* t) {
  if (t->is_sym()) return new Sym(dynamic_cast<Sym*>(t));
  else if (t->is_call()) return new Call(dynamic_cast<Call*>(t));
  else if (t->is_null()) return new Null();
  else if (t->is_na()) return new NA();
  else if (t->is_statements()) return new Statements(dynamic_cast<Statements*>(t));
  else if (t->is_num()) return new Num();
  else if (t->is_str()) return new Str();
  else if (t->is_other()) return new Other(dynamic_cast<Other*>(t));
}


bool Exp::equals(Exp* t) {
  if (t->is_sym()) return is_sym();
  else if (t->is_call()) eq_call(dynamic_cast<Call*>(t));
  else if (t->is_null()) return is_null();
  else if (t->is_na()) return is_na();
  else if (t->is_statements()) eq_statements(dynamic_cast<Statements*>(t));
  else if (t->is_num()) return is_num();
  else if (t->is_str()) return is_str();
  else if (t->is_other()) return eq_other(dynamic_cast<Other*>(t));
}

////////////// Builder /////////////////////
/* A builder takes an R ast represented by a SEXP and builds the corresponding
*  Exp. In the process it simplifies things a bit as our Exp is simpler than R's
*  SEXP. */
class Builder {

public:
  Exp* build(SEXP ast) {
    switch (TYPEOF(ast)) {
    case NILSXP:  return new Null();
    case INTSXP:
    case REALSXP:
    case CPLXSXP: return new Num();
    case SYMSXP:  return new Sym(CHAR(PRINTNAME(ast)));
    case LGLSXP:  return doLGLSXP(ast);
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

  /* Logicals are represented by Nums except for NA, this is because NA starts
  *  as LGL NA and is then converted to which ever type of NA the environment
  *  needs. */
  Exp* doLGLSXP(SEXP ast) {
    return asLogical(ast)==NA_LOGICAL ? ((Exp*) new NA()) : ((Exp*) new Num());
  }

  /* We forget the value of strings, except a few special cases. */
  Exp* doSTRSXP(SEXP ast) {
    const char* s = CHAR(STRING_ELT(ast, 0));
    if (eq(s, "<ENVIRONMENT>") || eq(s, "<WEAK REFERENCE>") || eq(s, "<POINTER>"))
      return new Other(s);
    else
      return new Str();
  }

  /* The only place we should encounter this type is when processing the
     arguments of a function definition. Since we don't really care about those
     right now, we can simplify. */
  Exp* doLISTSXP(SEXP ast) { return new Other("(ARGS)"); }

  /* Function calls can be place to named functions or anonymous ones. */
  Exp* doLANGSXP(SEXP ast) {
    Exp* fun = build(CAR(ast)); // Function 'name' can be (1) a
    Sym* fun_name = nullptr;   // symbol, (2) namespace (langsxp),
    // (3) anonymous function (langsxp). For (1) and (2) we keep
    // function names, for (3) we retain the whole expression.
    if (fun->is_sym()) fun_name = dynamic_cast<Sym*>(fun);
    else if (fun->is_call()) {
      Call* name_call = dynamic_cast<Call*>(fun);
      if (name_call->eq_name("::") || name_call->eq_name(":::"))
	    fun_name = dynamic_cast<Sym*>(name_call->get_arg(1));
    }
    Call* call = new Call(fun_name, fun);
    // Process the arguments
    for (SEXP ptr = CDR(ast); ptr != R_NilValue; ptr = CDR(ptr))
      call->add_arg(build(CAR(ptr)));

    // Some function have 3 arguments. The last one is a srcref. We get rid of it
    if(call->eq_name("function") && call->get_args().size() == 3)
        call->get_args().pop_back();
    return call;
  }

  /* Unclear what this is... */
  Exp* doEXPRSXP(SEXP ast) {
    Statements* stmts = new Statements();
    int size = Rf_length(ast);
    for (int i = 0; i < size; i++)
      stmts->add(build(VECTOR_ELT(ast, i)));
    return stmts;
  }

  /* Other cases that we don't really handle */
  Exp* doDEFAULT(const char* val) { return new Other(val);  }

};

///////////////////// Simplifier /////////////////////////
/* The simplifier takes an Exp and returns a simpler or equal Exp. */
class Simplifier {

public:
  Exp* simplify(Exp* t) {
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

  /* This is the heart of simplification as all intersting things are calls. */
  Exp* doCall(Call* x) {
    Vec args;
    args.reserve(x-> get_args().size());
    for(Exp* t : x-> get_args()) args.push_back(simplify(t));

    args = subsume(args);

    if (x->kind() == UnknownOp) { // anon function
      Exp* anon = simplify(x->get_anon());
      return new Call(nullptr, anon, args);
    } else if (x->kind() == ModelFrameOp) {
      return new Call(x, args);
    } else if  (x->kind() == ArithOp ||x->kind() == LogicOp) {
      if (args.size() == 1) return args[0];
      Sym* op = new Sym("OP");
      return new Call(op, op, args);
    } else if  (x->kind() == NamedOp) { // named function
      if (x->eq_name("(") && args.size() == 1) return args[0];
      else if((x->eq_name("integer") || x->eq_name("double") ||
	       x->eq_name("numeric")) && args.size() == 1 && args[0]->is_num() )
	return new Num();
      else if(x->eq_name("character")  && args.size() == 1 &&
	      args[0]->is_num() )
	return new Str();
      else if(x->eq_name("structure") && args.size() == 1 &&
         !(args[0]->is_other() || args[0]->is_statements()))
	return args[0];
      else if(x->eq_name("{")) {
	if(args.size() == 1) return args[0]; // Elide block with only 1 statement
	else if(args.size() == 0) return new Call(x, args);
	Vec empty_args;
	return new Call(new Sym("{MANY"), x->get_anon(), empty_args);
      }
      return new Call(x, args);
    } else if  (x->kind() == ListVecOp)  // c() or list()
      return (args.size() == 1)?  args[0] :  new Call(x, args);
  }

  /* Subsume takes a vector of simplified Exp and removes all entries that are
  *  subsumed by other entries. */
Vec subsume(const std::vector<Exp*>& v) {
    std::vector<Exp*> r;
    int len = v.size();
    for (int i=0; i<len; i++) {
      Exp* t = v[i];
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

  Exp* doStatements(Statements* x) {
    Vec elems;
    elems.reserve(x->get_elems().size());
    for(Exp* t: x->get_elems()) elems.push_back(simplify(t));
    elems = subsume(elems);
    if (elems.size() == 1) return elems[0];
    return new Statements(elems);
  }
};



///////////////// COUNTER ////////////////////////////////
class Counter {
  bool top = true;  // are we at the root? Internal use.

public:
  const char* topcall = nullptr; // what is the top call if any?
  bool is_model = false; // is the top a call to model.frame?
  bool has_fundef = false;  // is there a function definition
  int has_calls = 0;// how many function calls?
  int has_assigns = 0; // how many assignments?
  bool has_var = false; // is there a var access?
  bool has_bracket = false; // is there indexing?
  bool is_assign = false; // is it assignment?
  bool is_value = false; // is it  a value?
  bool is_ignore = false; // should we ignore this?
  bool has_dollar = false;
  bool has_user_call = false;
  bool has_block = false;

  bool boring() {
    return is_ignore || is_value || has_calls <= 1;
  }


  void count(Exp* t) {
    if (t->is_sym())        { has_var = true; }
    else if (t->is_call())  { doCall(dynamic_cast<Call*>(t)); }
    else if (t->is_null())  { if (top) is_value = true; }
    else if (t->is_na())    { if (top) is_value = true; }
    else if (t->is_statements()) { doStatements(dynamic_cast<Statements*>(t)); }
    else if (t->is_num())   { if (top) is_value = true;}
    else if (t->is_str())   { if (top) is_value = true; }
    else if (t->is_other()) { if (top) is_ignore = true;}
  }
  void doCall(Call* x) {
    if (top) topcall = x->get_name();

    if(x->eq_name("<-") || x->eq_name("assign") || x->eq_name("<<-")) {
      if (top) is_assign = true;
      has_assigns++;
    } else if (x->eq_name("$")) {
      has_dollar = true;
    } else if (x->eq_name("{")) {
      has_block = true;
    } else if (x->eq_name("[[") || x->eq_name("[")) {
      has_bracket = true;
    } else if (x->kind() == NamedOp || x->kind() == UnknownOp) {
      has_calls++;
      has_user_call = true;
    }

    if (top) is_model = (x->kind() == ModelFrameOp);

    if (x->eq_name("function")) { has_fundef = true; return; }
    top = false;
    for(auto arg : x->get_args()) count(arg);
  }

  void doStatements(Statements* x) { for(Exp* t: x->get_elems()) count(t); }

};

bool first = true;

///////////////////////////////////////////////////////////
SEXP r_normalize(SEXP hash, SEXP ast, SEXP trimmed_str) {
  Builder builder;
  Exp* t = builder.build(ast);
  Simplifier s;
  Exp* t2 = s.simplify(t);
  delete t;
  CharBuff buf;
  t2->write(&buf, true);
  Counter c;
  c.count(t2);
  char* str = buf.get();
  if (first) {
    std::cout << "minimized, "
	     <<  "topcall, "
	     <<  "is_model, "
      	     <<  "has_fundef, "
      	     <<  "has_calls, "
      	     <<  "has_assigns, "
      	     <<  "has_var, "
      	     <<  "has_bracket, "
	     <<  "is_assign,"
             <<  "is_value, "
	     <<  "is_ignore, "
	     <<  "has_dollar, "
	     <<  "has_user_call, "
	     <<  "has_block, "
	     <<  "is_value, "
             <<  "normalized, "
             << "trimmed,"
             <<  "hash"
             << std::endl;
    first = false;
  }
  if (c.is_ignore)                        std::cout << "Ignore" ;
  else if (c.is_value)                    std::cout << "V" ;
  else if (c.is_model)                    std::cout << "model.frame" ;
  else if (eq(c.topcall,"{MANY"))         std::cout << "{BLOCK}" ;
  else if (eq(c.topcall,"function"))      std::cout << "FUN ";
  else if (c.is_assign && c.boring())     std::cout << "<-";
  else if (c.has_dollar && c.boring())    std::cout << "$" ;
  else if (c.has_var && c.has_calls == 0) std::cout << "X";
  else if (c.has_calls == 1) {
    if (!c.has_user_call) {
      std::cout << "HUH " << str;
    } else {
      if (c.has_dollar)                   std::cout << "F($)" ;
      else if (c.has_var)                 std::cout << "F(X)" ;
      else                                std::cout << "F()" ;
    }
  } else if (!c.has_user_call) {
    if (c.has_dollar) {
      if (c.has_assigns)                  std::cout << "$<-";
      else                                std::cout << "$";
    } else if (c.has_assigns)             std::cout << "<-";
    else if (c.has_var)                   std::cout << "X";
    else {
      std::cout << "ERROR " << str
		<< " has_var=" << c.has_var
		<< " has_assigns=" << c.has_assigns
		<< " has_dollar=" << c.has_dollar
		<< " has_calls" << c.has_calls;
    }
  } else {
    if (c.has_user_call)                  std::cout << "F(F(";
    if (c.has_assigns)                    std::cout << "<- ";
    if (c.has_block)                      std::cout << "{BLOCK} ";
    if (c.has_fundef)                     std::cout << "FUN";
    if (c.has_user_call)                  std::cout << "))";
  }
  std::cout << ", " << (c.topcall? c.topcall : "")
	    << ", " << c.is_model
    	    << ", " << c.has_fundef
    	    << ", " << c.has_calls
    	    << ", " << c.has_assigns
	    << ", " << c.has_var
	    << ", " << c.has_bracket
	    << ", " << c.is_assign
    	    << ", " << c.is_value
    	    << ", " << c.is_ignore
       	    << ", " << c.has_dollar
       	    << ", " << c.has_user_call
	    << ", " << c.has_block
            << ", \"" << str << "\""
            << ", \"" << CHAR(STRING_ELT(trimmed_str, 0)) <<  "\""
            << ", " << CHAR(STRING_ELT(hash,0))
	    << std::endl;
  delete t2;

  return Rf_ScalarLogical(NA_LOGICAL); // Just return something to prevent warnings
}


SEXP r_normalize_expr(SEXP ast) {
  Builder builder;
  Exp* t = builder.build(ast);
  Simplifier s;
  Exp* t2 = s.simplify(t);
  delete t;
  CharBuff buf;
  t2->write(&buf, false);
  delete t2;
  SEXP r_value = PROTECT(mkString(buf.get()));
  UNPROTECT(1);
  return r_value;
}
