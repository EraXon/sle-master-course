module Syntax

extend lang::std::Layout;
extend lang::std::Id;

start syntax Form 
  = form: "form" Str title "{" Question* questions "}"; 

lexical Str = [\"]![\"]* [\"];

lexical Bool = "true" | "false";

lexical Int = [0-9]+ ; 

// boolean, integer, string
syntax Type = "boolean"|"integer"|"string";


// TODO: answerable question, computed question, block, if-then-else
syntax Question 
  = ifThen: "if" "(" Expr cond ")" Question then () !>> "else" 
  | blockQuestion: "{" Question* questions "}"
  | answerableQuestion: Str label Id id ":" Type type
  | computedQuestion: Str label Id id ":" Type type "=" Expr value
  | ifThenElse: "if" "(" Expr cond ")" Question then "else" Question else
  ;

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr
  = var: Id name \ "true" \"false"
  > left (Expr lhs "+" Expr rhs| sub: Expr lhs "-" Expr rhs)
  > left (Expr lhs "*" Expr rhs| div: Expr lhs "/" Expr rhs)
  > left (Expr lhs "&&" Expr rhs| Expr lhs "||" Expr rhs)
  > left (Expr lhs "\>" Expr rhs| Expr lhs "\<" Expr rhs| Expr lhs "\<=" Expr rhs| Expr lhs "\>=" Expr rhs)
  > left (Expr lhs "==" Expr rhs| Expr lhs "!=" Expr rhs)
  | "!" Expr |"(" Expr ")" 
  |Int|Bool|Str ;

