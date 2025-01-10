module PartialEvaluation

import Syntax;
import Eval;
import Flatten;
import String;
import IO;
import ParseTree;

 /*
  * Partial evaluation:
  *  evaluate a questionnaire to another questionnaire with partial knowledge 
  *  as represented by the VEnv argument.
  *   - if conditions evaluate to true or false eliminate if-constructs
  *   - if variables are in VEnv evaluate them to values
  *   - expressions with value operands should be evaluated 
  *  Use eval where needed. 
  */


start[Form] peval(start[Form] form, VEnv venv) = unflatten(peval(flatten(form), venv), form);

list[Question] peval(list[Question] qs, VEnv venv) {
    return [*peval(q, venv)|Question q <- qs];
} 


// NB: this function returns a list of questions
// so that if conditionals disappear you can return the empty list.
list[Question] peval(Question q, VEnv venv) {
    list [Question] final = [];
    if(q has cond){
        if((Expr)`true`:=peval(q.cond,venv) ){
            final += peval(q.then, venv);
        } else if ((Expr)`false`:=peval(q.cond,venv))  {
            ;
        } else {
            //println(q);
            q.then=peval(q.then,venv)[0];
            final+=[q];
        }
        
    
    }else {
        if(q has \value){
            q.\value=peval(q.\value,venv);
            
        }
        final+=[q];

    }
    return final;
}

Expr peval(e:(Expr)`true`, VEnv env) = e;
Expr peval(e:(Expr)`false`, VEnv env) = e;
Expr peval(e:(Expr)`<Int n>`, VEnv env) = e;
Expr peval(e:(Expr)`<Str s>`, VEnv env) = e;
Expr peval(e:(Expr)`<Id x>`, VEnv env) = value2expr(env["<x>"])
    when "<x>" in env;

default Expr peval(e:(Expr)`<Id x>`, VEnv env) = e;


Expr peval((Expr)`<Expr lhs> + <Expr rhs>`, VEnv env) = iexp
    when (Expr)`<Int i1>` := peval(lhs, env),
         (Expr)`<Int i2>` := peval(rhs, env),
         int i := toInt("<i1>") + toInt("<i2>"),
         Expr iexp := [Expr]"<i>";


default Expr peval((Expr)`<Expr lhs> + <Expr rhs>`, VEnv env) 
    = (Expr)`<Expr e1> + <Expr e2>`
    when Expr e1 := peval(lhs, env),
         Expr e2 := peval(rhs, env);

Expr peval((Expr)`<Expr lhs> - <Expr rhs>`, VEnv env) = iexp
    when (Expr)`<Int i1>` := peval(lhs, env),
         (Expr)`<Int i2>` := peval(rhs, env),
         int i := toInt("<i1>") - toInt("<i2>"),
         Expr iexp := [Expr]"<i>";

default Expr peval((Expr)`<Expr lhs> - <Expr rhs>`, VEnv env)
    = (Expr)`<Expr e1> - <Expr e2>`
    when Expr e1 := peval(lhs, env),
         Expr e2 := peval(rhs, env);

Expr peval((Expr)`<Expr lhs> * <Expr rhs>`, VEnv env) = iexp
    when (Expr)`<Int i1>` := peval(lhs, env),
         (Expr)`<Int i2>` := peval(rhs, env),
         int i := toInt("<i1>") * toInt("<i2>"),
         Expr iexp := [Expr]"<i>";

default Expr peval((Expr)`<Expr lhs> * <Expr rhs>`, VEnv env)
    = (Expr)`<Expr e1> * <Expr e2>`
    when Expr e1 := peval(lhs, env),
         Expr e2 := peval(rhs, env);

Expr peval((Expr)`<Expr lhs> / <Expr rhs>`, VEnv env) = iexp
    when (Expr)`<Int i1>` := peval(lhs, env),
         (Expr)`<Int i2>` := peval(rhs, env),
         int i := toInt("<i1>") / toInt("<i2>"),
         Expr iexp := [Expr]"<i>";

default Expr peval((Expr)`<Expr lhs> / <Expr rhs>`, VEnv env)
    = (Expr)`<Expr e1> / <Expr e2>`
    when Expr e1 := peval(lhs, env),
         Expr e2 := peval(rhs, env);

Expr peval((Expr)`<Expr lhs> && <Expr rhs>`, VEnv env) = bexp
    when (Expr)`<Bool b1>` := peval(lhs, env),
         (Expr)`<Bool b2>` := peval(rhs, env),
         bool b := (("<b1>" == "true") && ("<b2>" == "true")),
         Expr bexp := [Expr]"<b>";

default Expr peval((Expr)`<Expr lhs> && <Expr rhs>`, VEnv env)
    = (Expr)`<Expr e1> && <Expr e2>`
    when Expr e1 := peval(lhs, env),
         Expr e2 := peval(rhs, env);

Expr peval((Expr)`<Expr lhs> || <Expr rhs>`, VEnv env) = bexp
    when (Expr)`<Bool b1>` := peval(lhs, env),
         (Expr)`<Bool b2>` := peval(rhs, env),
         bool b := (("<b1>" == "true") || ("<b2>" == "true")),
         Expr bexp := [Expr]"<b>";

default Expr peval((Expr)`<Expr lhs> || <Expr rhs>`, VEnv env)
    = (Expr)`<Expr e1> || <Expr e2>`
    when Expr e1 := peval(lhs, env),
         Expr e2 := peval(rhs, env);

Expr peval((Expr)`!<Expr e>`, VEnv env) = bexp
    when (Expr)`<Bool bo>` := peval(e, env),
         bool b := "<bo>" == "false",
         Expr bexp := [Expr]"<b>";

default Expr peval((Expr)`!<Expr e1>`, VEnv env)
    = (Expr)`!<Expr e>`
    when Expr e := peval(e1, env);




