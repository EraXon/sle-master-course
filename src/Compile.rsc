module Compile

import Syntax;
import Eval;
import IO;
import ParseTree;


import lang::html::AST; // modeling HTML docs
import lang::html::IO; // reading/writing HTML


void compile(start[Form] form) {
  loc h = form.src[extension="html"];
  loc j = form.src[extension="js"].top;
  
  HTMLElement ht = compile2html(form);
  
  writeHTMLFile(h, ht, escapeMode=extendedMode());
  str js = compile2js(form);
  writeFile(j, js);
}

str compile2js(start[Form] form) {
  return code+readFile(|project://sle-master-course/src/RestofJsForCompile.js|);
}

HTMLElement compile2html(start[Form] f) {
HTMLElement page=html([
  
  lang::html::AST::head([
    title([text("<f.top.title>"[1..-1])]),
    script([],src=f.src[extension="js"].top.file)
  ]),
  body([
       h1([text("<f.top.title>"[1..-1])]),
      *[q2html(q,(Expr)`true`) | q <- f.top.questions]
    ])
  ]);
  return page;
}

str code="const map1 = new Map();\n const values = new Map();\n";

HTMLElement q2html(Question q, Expr isEnabled) {
  HTMLElement elem;

  switch(q){
    case q:(Question)`<Str l> <Id i> : <Type t>` :{
 
      elem=div([
        label([text("<l>"[1..-1])]),
        typeOfInput(t, q,true)
      ],id="div_<i>_<q.src.begin.line>");
      code+="map1.set(\"div_<i>_<q.src.begin.line>\",
       '{kind:\"answerable\",
       ' type:\"<t>\",
       ' variable:\"<i>\",
       ' isEnabled:\"<exprToStr(isEnabled)>\"
       '});
       '\n";
      code+="values.set(\"<i>\",\"<i>\");\n";
    }
    case (Question)`<Str l> <Id i> : <Type t> = <Expr e>`:{
      println("expresion: <e>");
      elem=div([
        label([text("<l>"[1..-1])]),
        typeOfInput(t, q,false)
      ],id="div_<i>_<q.src.begin.line>");
      code+="map1.set(\"div_<i>_<q.src.begin.line>\",
       '{kind:\"computable\",
       ' type:\"<t>\",
       ' variable:\"<i>\",
       ' isEnabled:\"<exprToStr(isEnabled)>\",
       'expr:\"<exprToStr(e)>\"
       '});
       '\n";
      code+="values.set(\"<i>\",\"<i>\");\n";
    }
    case (Question)`if (<Expr cond>) <Question then>`:{
      elem=div([
        q2html(then,(Expr)`<Expr isEnabled>&&<Expr cond>`)
      ],id="if_<i>_<q.src.begin.line>");
    }
    case (Question)`if (<Expr cond>) <Question then> else <Question e>`:{
      elem=div([
        q2html(then,(Expr)`<Expr isEnabled>&&<Expr cond>`),
        q2html(e,(Expr)`<Expr isEnabled>&&!(<Expr cond>)`)
      ],id="if_else_<q.src.begin.line>_<q.src.offset>");
    }
    case (Question)`{<Question*  qs>}`:{
      elem=div([
        *[q2html(qu,isEnabled) | qu <- qs]
      ]);
    }
  }
  return elem;
}

HTMLElement typeOfInput(Type t, Question q, bool enabled) {
  HTMLElement elem;
  switch(t){
    case (Type)`integer`:{ 
      if(enabled){
        elem= input(\type="number",id="input_div_<q.id>_<q.src.begin.line>");
      }
      else{
        elem= input(\type="number", disabled="true",id="input_div_<q.id>_<q.src.begin.line>");
      }
      
    }

    case (Type)`string`:{
      if(enabled){
        elem= input(\type="text",id="input_div_<q.id>_<q.src.begin.line>");
      }
      else{
        elem= input(\type="text", disabled="true",id="input_div_<q.id>_<q.src.begin.line>");
      }
    }
    case (Type)`boolean`:{
      if(enabled){
        elem= input(\type="checkbox",id="input_div_<q.id>_<q.src.begin.line>"); 
      }
      else{
        elem= input(\type="checkbox", disabled="true",id="input_div_<q.id>_<q.src.begin.line>");
      }
    }

  }

  return elem;
}


str exprToStr((Expr)`<Expr lhs>+ <Expr rhs>`) = "(<exprToStr(lhs)> + <exprToStr(rhs)>)";
str exprToStr((Expr)`<Expr lhs>* <Expr rhs>`) = "(<exprToStr(lhs)> * <exprToStr(rhs)>)";
str exprToStr((Expr)`<Expr lhs>/ <Expr rhs>`) = "(<exprToStr(lhs)> / <exprToStr(rhs)>)";
str exprToStr((Expr)`<Expr lhs>- <Expr rhs>`) = "(<exprToStr(lhs)> - <exprToStr(rhs)>)";
str exprToStr((Expr)`<Expr lhs> && <Expr rhs>`) = "(<exprToStr(lhs)> && <exprToStr(rhs)>)";
str exprToStr((Expr)`<Expr lhs> || <Expr rhs>`) = "(<exprToStr(lhs)> || <exprToStr(rhs)>)";
str exprToStr((Expr)`!<Expr e>`) = "(!<exprToStr(e)>)";
str exprToStr((Expr)`<Id x>`) = "(values.get(\\\"<x>\\\")) ";
str exprToStr((Expr)`<Int n>`) = "<n>";
str exprToStr((Expr)`<Str s>`)  {
  str aux="<s>"[1..-1];
  return "\\\"<aux>\\\"";
}
str exprToStr((Expr)`<Bool b>`) = "<b>";
str exprToStr((Expr)`(<Expr e>)`) = "(<exprToStr(e)>)";
str exprToStr((Expr)`<Expr lhs> \> <Expr rhs>`) = "(<exprToStr(lhs)> \> <exprToStr(rhs)>)";
str exprToStr((Expr)`<Expr lhs> \< <Expr rhs>`) = "(<exprToStr(lhs)> \< <exprToStr(rhs)>)";
str exprToStr((Expr)`<Expr lhs> == <Expr rhs>`) = "(<exprToStr(lhs)> == <exprToStr(rhs)>)";
str exprToStr((Expr)`<Expr lhs> != <Expr rhs>`) = "(<exprToStr(lhs)> != <exprToStr(rhs)>)";
str exprToStr((Expr)`<Expr lhs> \<= <Expr rhs>`) = "(<exprToStr(lhs)> \<= <exprToStr(rhs)>)";
str exprToStr((Expr)`<Expr lhs> \>= <Expr rhs>`) = "(<exprToStr(lhs)> \>= <exprToStr(rhs)>)";



