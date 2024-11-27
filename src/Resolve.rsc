module Resolve

import Syntax;
import IO;
import Node;
import ParseTree;
/*
 * Name resolution for QL
 */ 


// modeling declaring occurrences of names
alias Def = rel[str name, loc def];

// modeling use occurrences of names
alias Use = rel[loc use, str name];

alias UseDef = rel[loc use, loc def];

// the reference graph
alias RefGraph = tuple[
  Use uses, 
  Def defs, 
  UseDef useDef
]; 

RefGraph resolve(start[Form] f) = <us, ds, us o ds>
  when Use us := uses(f), Def ds := defs(f);

Use uses(start[Form] f) {

  rel [loc, str] uses = { };
  visit(f){
    case var(Id id): uses += <id.src, "<id>">;
  }

  return uses; 
}

Def defs(start[Form] f) {

  rel [str, loc] defs = { };
  visit(f){
    case answerableQuestion(_,Id id,_):  defs += <"<id>", id.src>;  
    case computedQuestion(_,Id id,_,_): defs += <"<id>", id.src>;

  }
  return defs;
}

