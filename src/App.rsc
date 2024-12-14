module App

import salix::HTML;
import salix::App;
import salix::Core;
import salix::Index;

import Eval;
import Syntax;

import String;

// The salix application model is a tuple
// containing the questionnaire and its current run-time state (env).
alias Model = tuple[start[Form] form, VEnv env];

App[Model] runQL(start[Form] ql) = webApp(qlApp(ql), |project://sle-master-course/src|);

SalixApp[Model] qlApp(start[Form] ql, str id="root") 
  = makeApp(id, 
        Model() { return <ql, initialEnv(ql)>; }, 
        withIndex("<ql.top.title>"[1..-1], id, view, css=["https://cdn.simplecss.org/simple.min.css"]), 
        update);


// The salix Msg type defines the application events.
data Msg
  = updateInt(str name, str n)
  | updateBool(str name, bool b)
  | updateStr(str name, str s)
  ;

// We map messages to Input values 
// to be able to reuse the interpreter defined in Eval.
Input msg2input(updateInt(str q, str n)) = user(q, vint(toInt(n)));
Input msg2input(updateBool(str q, bool b)) = user(q, vbool(b));
Input msg2input(updateStr(str q, str s)) = user(q, vstr(s));

// The Salix model update function simply evaluates the user input
// to obtain the new state. 
Model update(Msg msg, Model model) = model[env=eval(model.form, msg2input(msg), model.env)];

// Salix view rendering works by "drawing" on an implicit HTML canvas.
// Look at the Salix demo folder to learn how html elements are drawn, and how element nesting is achieved with
// nesting of void-closures.
void view(Model model) {
    h3("<model.form.top.title>"[1..-1]);
    form((){
      for(Question question<-model.form.top.questions){
        viewQuestion(question, model);
      }
    });
}

// fill in: question rendering, but only if they are enabled.
void viewQuestion(Question q, Model model) {
  switch(q){
    case (Question)`if ( <Expr cond>) <Question then>`:{
      if(eval(cond, model.env) == vbool(true)){
        viewQuestion(then, model);
      }
    }
    case (Question)`if ( <Expr cond>) <Question then> else <Question e>`:{
      if(eval(cond, model.env) == vbool(true)){
        viewQuestion(then, model);
      } else {
        viewQuestion(e, model);
      }
    }
    case (Question)`{ <Question* questions >}`:{
      for(Question question<-questions){
        viewQuestion(question, model);
      }
    }
    case (Question)`<Str label> <Id id> : <Type t>`:{
      p(() {
        text("<label>"[1..-1]);
        switch(t){
          
          case (Type)`integer`:{ 
               
                input(\type("number"), \value("<model.env["<id>"].n>"), onChange(partial(updateInt, "<id>"))); 
          }

          case (Type)`string`:{
                input(\type("text"), \value(model.env["<id>"].s), onChange(partial(updateStr, "<id>"))); 
          }
          case (Type)`boolean`:{
                input(\type("checkbox"), \checked(model.env["<id>"].b), onClick((updateBool( "<id>",!model.env["<id>"].b)))); 
          }
        }


      });
    }
    case (Question)`<Str label> <Id id> : <Type t> = <Expr v>`:
      {
      p((){
        text("<label>"[1..-1]);
        switch(t){
            case (Type)`integer`:{ 
               
                input(\type("number"), \value("<model.env["<id>"].n>"),disabled(true)); 
          }

          case (Type)`string`:{
                input(\type("text"), \value(model.env["<id>"].s), disabled(true)); 
          }
          case (Type)`boolean`:{
                input(\type("checkbox"), \checked(model.env["<id>"].b),disabled(true)); 
          }
        
        
        }
      });
      



    
  }
  }
}
