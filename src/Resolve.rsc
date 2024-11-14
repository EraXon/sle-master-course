module Resolve

import Syntax;

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
  return {}; 
}

Def defs(start[Form] f) {
  return {}; 
}