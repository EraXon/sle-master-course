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
  str js = compile2js(form);
  HTMLElement ht = compile2html(form);
  writeFile(j, js);
  writeHTMLFile(h, ht, escapeMode=extendedMode());
}

str compile2js(start[Form] form) {
  return "";
}

HTMLElement compile2html(start[Form] form) {
  return html([

    head([title([text("<form.top.title>"[1..-1])])])
  ]);
}

