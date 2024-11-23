module stm::AST

import stm::Syntax;

data AMachine
   = machine(str name, list[AState] states);

data AState
   = state(str name, list[ATrans] transitions);

data ATrans
  = trans(str event, str target);

AMachine implode(start[Machine] m) = implode(m.top);

AMachine implode((Machine)`machine <Id n> <State* ss>`)
  = machine("<n>", [ implode(s) | State s <- ss ]);

AState implode((State)`state <Id n> <Trans* ts> end`)
  = state("<n>", [ implode(t) | Trans t <- ts ]);

ATrans implode((Trans)`<Id e> =\> <Id t>`)
  = trans("<e>", "<t>");