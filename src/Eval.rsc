module Eval

import Syntax;
import String;
import ParseTree;
import IO;

/*
 * Big-step semantics for QL
 */
 
// NB: Eval assumes the form is type- and name-correct.

// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment, mapping question names to values.
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input = user(str question, Value \value);
  

Value type2default((Type)`integer`) = vint(0);
Value type2default((Type)`string`) = vstr("");
Value type2default((Type)`boolean`) = vbool(false);


// produce an environment which for each question has a default value
// using the function type2default function defined above.
// observe how visit traverses the form and match on normal questions and computed questions.
VEnv initialEnv(start[Form] f) = initialEnv(f.top);

VEnv initialEnv(Form f) {
  VEnv venv = ();
  visit(f){
    case (Question)`<Str _> <Id id> : <Type t>` :  venv["<id>"] =type2default(t);  
    case (Question)`<Str _> <Id id> : <Type t> = <Expr _>`: venv["<id>"]=type2default(t);
  }
  return eval(f, user("", vint(0)), venv);
}

// Expression evaluation (complete for all expressions) 

Value eval((Expr)`<Id x>`, VEnv venv) = venv["<x>"];


Value eval((Expr)`<Int n>`, VEnv venv) = vint(toInt("<n>"));
Value eval((Expr)`<Str s>`, VEnv venv) = vstr("<s>");
Value eval((Expr)`<Bool b>`, VEnv venv) = vbool("<b>" == "true");
Value eval((Expr)`<Expr e>`,VEnv venv)=eval(e, venv);

Value eval((Expr)`<Expr lhs> + <Expr rhs>`,VEnv venv)=vint(n+m)
  when vint(int n) := eval(lhs, venv), vint(int m) := eval(rhs, venv);

Value eval((Expr)`<Expr lhs> - <Expr rhs>`,VEnv venv)=vint(n-m)
  when vint(int n) := eval(lhs, venv), vint(int m) := eval(rhs, venv);


Value eval((Expr)`<Expr lhs> * <Expr rhs>`,VEnv venv)=vint(n*m)
  when vint(int n) := eval(lhs, venv), vint(int m) := eval(rhs, venv);

  Value eval((Expr)`<Expr lhs> / <Expr rhs>`,VEnv venv)=(m!=0)?vint(n/m):vint(0)
    when vint(int n) := eval(lhs, venv), vint(int m) := eval(rhs, venv);

Value eval((Expr)`<Expr lhs> && <Expr rhs>`,VEnv venv)=vbool(b && c)
  when vbool(bool b) := eval(lhs, venv), vbool(bool c) := eval(rhs, venv);

Value eval((Expr)`<Expr lhs> || <Expr rhs>`,VEnv venv)=vbool(b || c)
  when vbool(bool b) := eval(lhs, venv), vbool(bool c) := eval(rhs, venv);

Value eval((Expr)`<Expr lhs> \> <Expr rhs>`,VEnv venv)=vbool(n > m)
  when vint(int n) := eval(lhs, venv), vint(int m) := eval(rhs, venv);

Value eval((Expr)`<Expr lhs> \< <Expr rhs>`,VEnv venv)=vbool(n < m)
  when vint(int n) := eval(lhs, venv), vint(int m) := eval(rhs, venv);

Value eval((Expr)`<Expr lhs> == <Expr rhs>`,VEnv venv)=vbool(eval(lhs,venv) == eval(rhs,venv));


Value eval((Expr)`<Expr lhs> != <Expr rhs>`,VEnv venv)=vbool(eval(lhs,venv) != eval(rhs,venv));
  

Value eval((Expr)`<Expr lhs> \<= <Expr rhs>`,VEnv venv)=vbool(n <= m)
  when vint(int n) := eval(lhs, venv), vint(int m) := eval(rhs, venv);


Value eval((Expr)`<Expr lhs> \>= <Expr rhs>`,VEnv venv)=vbool(n >= m)
  when vint(int n) := eval(lhs, venv), vint(int m) := eval(rhs, venv);

Value eval((Expr)`!<Expr e>`,VEnv venv)=vbool(!b)
  when vbool(bool b) := eval(e, venv);

Value eval((Expr)`(<Expr e>)`,VEnv venv)=eval(e, venv);



VEnv eval(start[Form] f, Input inp, VEnv venv) = eval(f.top, inp, venv);

// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(Form f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

// evaluate the questionnaire in one round 
VEnv evalOnce(Form f, Input inp, VEnv venv)
  = ( venv | eval(q, inp, it) | Question q <- f.questions );


VEnv eval(Question q, Input inp, VEnv venv) {
  switch(q) {
    case (Question)`<Str label> <Id id> : <Type t> = <Expr v>`:
      return venv + ("<id>": eval(v, venv));
    case (Question)`if (<Expr cond>) <Question then>`:{
      if (eval(cond, venv) == vbool(true)) {
        return eval(then, inp, venv);
      }
      else {
        return venv;
      }
    }
    case (Question)`if (<Expr cond>) <Question then> else <Question e>`:{
      if (eval(cond, venv) == vbool(true)) {
        return eval(then, inp, venv);
      }
      else {
        return eval(e, inp, venv);
      }
    }
    case (Question)`{<Question*  qs>}`:{
      for (Question q <- qs) {
        venv = eval(q, inp, venv);
      }
      return venv;
    }
    case (Question)`<Str _> <Id id> : <Type t>`:{
      if(inp.question == "<id>") {
        return venv + (inp.question: inp.\value);
      }

    }

      
  
  
  }    
  return venv;
}

/*
 * Rendering UIs: use questions as widgets
 */

list[Question] render(start[Form] form, VEnv venv) = render(form.top, venv);

list[Question] render(Form form, VEnv venv) = [*render(q, venv) | Question q <- form.questions];

list[Question] render(q:(Question)`<Str label> <Id id> : <Type t>`, VEnv venv)=[q];

list [Question] render((Question)`if (<Expr cond>) <Question then>`, VEnv venv) =[*render(then, venv)|eval(cond,venv)==vbool(true)];

list [Question] render((Question)`if (<Expr cond>) <Question then> else <Question e>`, VEnv venv) =[*render(then, venv)|eval(cond,venv)==vbool(true)]+[*render(e, venv)|eval(cond,venv)==vbool(false)];

list [Question] render((Question)`{<Question*  qs>}`, VEnv venv) =[*render(q, venv)|Question q <- qs];

list [Question] render((Question)`<Str label> <Id id> : <Type t>=<Expr v>`, VEnv venv) =[(Question)`<Str label> <Id id> : <Type t>=<Expr val>`] 
  when Expr val := value2expr(eval(v, venv));





Expr value2expr(vbool(bool b)) = [Expr]"<b>";
Expr value2expr(vstr(str s)) = [Expr]"\"<s>\"";
Expr value2expr(vint(int i)) = [Expr]"<i>";

void printUI(list[Question] ui) {
  for (Question q <- ui) {
    println(q);
  }
}


void evalSnippets() {
  start[Form] pt = parse(#start[Form], |project://sle-master-course/examples/tax.myql|);

  env = initialEnv(pt);
  env2 = eval(pt, user("hasSoldHouse", vbool(true)), env);
  env3 = eval(pt, user("sellingPrice", vint(1000)), env2);
  env4 = eval(pt, user("privateDebt", vint(500)), env3);

  for (Input u <- [user("hasSoldHouse", vbool(true)), user("sellingPrice", vint(1000)), user("privateDebt", vint(500))]) {
    env = eval(pt, u, env);
    println(env);
  }
}