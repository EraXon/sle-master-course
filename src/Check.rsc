module Check

import Message;
import IO;
import ParseTree;
import List;
import vis::Text;

extend Syntax;

// internal type to represent unknown 
syntax Type = "*unknown*";

// type environment maps question names to types
// (NB: it's not a map, because the form can contain errors!)
alias TEnv = lrel[str, Type];

// build a Type Environment (TEnv) for a questionnaire.
TEnv collect(Form f) 

{   
    TEnv env = [];

  visit(f){
    case (Question)`<Str _> <Id id> : <Type t>` :  env += [<"<id>",t>];  
    case (Question)`<Str _> <Id id> : <Type t> = <Expr _>`: env += [<"<id>", t>];

  }
  return env;

}


/*
 * typeOf: compute the type of expressions
 */

// the fall back type is *unknown*
default Type typeOf(Expr _, TEnv env) = (Type)`*unknown*`;

// a reference has the type of its declaration
Type typeOf((Expr)`<Id x>`, TEnv env) = t
    when <"<x>", Type t> <- env;

Type typeOf((Expr)`<Int _>`, TEnv env) = (Type) `integer`;
Type typeOf((Expr)`<Str _>`, TEnv env) = (Type) `string`;
Type typeOf((Expr)`<Bool _>`, TEnv env) = (Type) `boolean`;

Type typeOf((Expr)`<Expr lhs> + <Expr rh>`, TEnv env) = (Type) `integer`;
Type typeOf((Expr)`<Expr lhs> - <Expr rh>`, TEnv env) = (Type) `integer`;
Type typeOf((Expr)`<Expr lhs> * <Expr rh>`, TEnv env) = (Type) `integer`;
Type typeOf((Expr)`<Expr lhs> / <Expr rh>`, TEnv env) = (Type) `integer`;
Type typeOf((Expr)`<Expr lhs> && <Expr rh>`, TEnv env) = (Type) `boolean`;
Type typeOf((Expr)`<Expr lhs> || <Expr rh>`, TEnv env) = (Type) `boolean`;
Type typeOf((Expr)`<Expr lhs> \> <Expr rh>`, TEnv env) = (Type) `boolean`;
Type typeOf((Expr)`<Expr lhs> \< <Expr rh>`, TEnv env) = (Type) `boolean`;
Type typeOf((Expr)`<Expr lhs> == <Expr rh>`, TEnv env) = (Type) `boolean`;
Type typeOf((Expr)`<Expr lhs> != <Expr rh>`, TEnv env) = (Type) `boolean`;
Type typeOf((Expr)`<Expr lhs> \<= <Expr rh>`, TEnv env) = (Type) `boolean`;
Type typeOf((Expr)`<Expr lhs> \>= <Expr rh>`, TEnv env) = (Type) `boolean`;
Type typeOf((Expr)`(<Expr e>)`, TEnv env) = typeOf(e, env);
Type typeOf((Expr)`!<Expr e>`, TEnv env) =(Type) `boolean`;


/*
 * Checking forms
 */

set[Message] check(start[Form] form) = check(form.top);

set[Message] check(Form form) 
  = { *check(q, env) | Question q <- form.questions }
  + checkDuplicates(form)
  + checkCycles(form)
  when TEnv env := collect(form);

set[Message] checkCycles(Form form) {

    set[Message] errors = {};
    rel[str decl,str depends,loc l] dependsData = {};
    rel[str decl,str depend,loc l] dependsControl={};

    visit (form){
        case q:(Question)`<Str _> <Id id> : <Type _> = <Expr e>`:{
            dependsData += {<"<id>", "<x>", q.src>| /Id x:=e};
        }

    }

    visit (form){        
        
        case (Question)`if ( <Expr cond> ) <Question then>`:{
            dependsControl += {<"<x>", "<q.id>", q.src>| /Id x:=cond, /Question q:=then, q has id};
            
        }
        case (Question)`if (<Expr cond>) <Question then> else <Question else1>`:{
            dependsControl += {<"<x>", "<q.id>", q.src>| /Id x:=cond, /Question q:=then, q has id};
            dependsControl += {<"<x>", "<q.id>", q.src>| /Id x:=cond, /Question q:=else1, q has id};
        }

    }



 
    for (<str x, x> <- dependsData<decl,depends>+) 
        for( <_, x, loc a> <- dependsData)
            errors += {error("cyclic control dependency", a)};
    
    for (<str x, x> <- dependsControl<decl,depend>+) 
        for( <_, x, loc a> <- dependsControl)
            errors += {error("cyclic data dependency", a)};

    println(dependsControl);
    return errors;
}

set[Message] checkDuplicates(Form form) {
    
    set [Question] seenQuestions = {};
    set[Message] errors = {};
    visit(form){
        case x:(Question)`<Str label> <Id id> : <Type t>`:{
                for (Question y <- seenQuestions) {
                    if("<y.id>" == "<id>"){
                        if ("<y.label>" != "<label>") 
                            errors += { warning("redeclared with some id but differnt label", x.src) };
                        if ("<y.\type>" != "<t>") {
                            errors += { error("redeclared with some id but differnt type", x.src) };
                        }

                        
                    }
                    else if ("<y.label>" == "<label>") {
                        errors += { warning("redeclared with same label but differnt id", x.src) };
                    }

                }

                seenQuestions += {x};
                errors += { warning("empty prompt", x.src) | (Str)`""` := label };
        }


        case f:(Question)`<Str label> <Id id> : <Type t> = <Expr _>`:{
                for (Question y <- seenQuestions) {
                    if("<y.id>" == "<id>"){
                        if ("<y.label>" != "<label>") 
                            errors += { warning("redeclared with some id but differnt label", f.src) };
                        if ("<y.\type>" != "<t>") {
                            errors += { error("redeclared with some id but differnt type", f.src) };
                        }

                        
                    }
                    else if ("<y.label>" == "<label>") {
                        errors += { warning("redeclared with same label but differnt id", f.src) };
                    }

                }

                seenQuestions += {f};
                errors += { warning("empty prompt", f.src) | (Str)`""` := label };
        }

        

    }
    return errors;
}

/*
 * Checking questions
 */

// by default, there are no errors or warnings
default set[Message] check(Question _, TEnv _) = {};



set[Message] check(x:(Question)`<Str label> <Id id> : <Type t> = <Expr v>`, TEnv env) = 
    {error("invalid expression type", v.src)|  t!:= typeOf(v,env) }+
    check(v, env);

set[Message] check(x:(Question)`if ( <Expr cond>) <Question then>`, TEnv env) =
    {error("invalid condition type", cond.src)|  (Type)`boolean`!:= typeOf(cond,env) }+
    check(then, env)+check(cond, env)+
    {warning("Useless condition",cond.src)| (Expr)`true`:=cond}+
    {warning("Empty if branch",then.src)| (Question)`{}`:=then}+
    {warning("Dead then branch",then.src)| (Expr)`false`:=cond};

set[Message] check(x:(Question)`if ( <Expr cond> ) <Question then> else <Question else1>`, TEnv env) =
    {error("invalid condition type", cond.src)|  (Type)`boolean`!:= typeOf(cond,env) }+
    check(then, env)+check(else1, env)+check(cond, env)+
    {warning("Dead else branch",cond.src)| (Expr)`true`:=cond}+
    {warning("Empty if branch",then.src)| (Question)`{}`:=then}+
    {warning("Empty else branch",else1.src)| (Question)`{}`:=else1}+
    {warning("Dead then branch",then.src)| (Expr)`false`:=cond};



set[Message] check(x:(Question)`{ <Question* questions >}`, TEnv env) =
    { *check(q, env) | Question q <- questions };

/*
 * Checking expressions
 */


// when the other cases fail, there are no errors
default set[Message] check(Expr _, TEnv env) = {};

set[Message] check(e:(Expr)`<Id x>`, TEnv env) = {error("undefined question", x.src)}
    when "<x>" notin env<0>;

set[Message] check(x:(Expr)`<Expr lhs> + <Expr rhs>`, TEnv env) = arthimethicErrors(x,lhs,rhs,env);


set[Message] check(x:(Expr)`<Expr lhs> * <Expr rhs>`, TEnv env) = arthimethicErrors(x,lhs,rhs,env);

set[Message] check(x:(Expr)`<Expr lhs> / <Expr rhs>`, TEnv env) = arthimethicErrors(x,lhs,rhs,env);

set[Message] check(x:(Expr)`<Expr lhs> - <Expr rhs>`, TEnv env) = arthimethicErrors(x,lhs,rhs,env);


set[Message] arthimethicErrors(Expr e,Expr lhs, Expr rhs, TEnv env) = 
    {error("invalid operand", lhs.src)|  (Type)`integer`!:= typeOf(lhs, env) } + {error("invalid operand", rhs.src)|(Type)`integer`!:= typeOf(rhs, env)}+
    check(lhs, env) + check(rhs, env);






set[Message] check(x:(Expr)`<Expr lhs> && <Expr rhs>`, TEnv env) = logicErrors(x,lhs,rhs,env);
    

set[Message] check(x:(Expr)`<Expr lhs> || <Expr rhs>`, TEnv env) = logicErrors(x,lhs,rhs,env);


set[Message] logicErrors(Expr e,Expr lhs, Expr rhs, TEnv env) = 
    {error("invalid operand", lhs.src)|  (Type)`boolean`!:= typeOf(lhs, env) } + {error("invalid operand", rhs.src)|(Type)`boolean`!:= typeOf(rhs, env)}+
    check(lhs, env) + check(rhs, env);  


set[Message] check(x:(Expr)`!<Expr e>`, TEnv env) = 
    {error("invalid operand", x.src)|  (Type)`boolean`!:= typeOf(e, env)}+
    check(e, env) ;

set[Message] check(x:(Expr)`<Expr lhs> \< <Expr rhs>`, TEnv env) = arthimethicErrors(x,lhs,rhs,env);


set[Message] check(x:(Expr)`<Expr lhs> \> <Expr rhs>`, TEnv env) = arthimethicErrors(x,lhs,rhs,env);

set[Message] check(x:(Expr)`<Expr lhs> == <Expr rhs>`, TEnv env) = {error("types do not match", x.src)|  (Type)t:= typeOf(lhs, env) , t!:=typeOf(rhs,env)} + 
    check(lhs, env) + check(rhs, env);

set[Message] check(x:(Expr)`<Expr lhs> != <Expr rhs>`, TEnv env) = {error("types do not match", x.src)|  (Type)t:= typeOf(lhs, env) , t!:=typeOf(rhs,env)} + 
    check(lhs, env) + check(rhs, env);
set[Message] check(x:(Expr)`<Expr lhs> \<= <Expr rhs>`, TEnv env) = arthimethicErrors(x,lhs,rhs,env);

set[Message] check(x:(Expr)`<Expr lhs> \>= <Expr rhs>`, TEnv env) = arthimethicErrors(x,lhs,rhs,env);




set[Message] check((Expr)`(<Expr e>)`, TEnv env) = check(e, env);





void printTEnv(TEnv tenv) {
    for (<str x, Type t> <- tenv) {
        println("<x>: <t>");
    }
}
 
