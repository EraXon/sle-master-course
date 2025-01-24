module stm::Format

import util::SimpleBox;
import stm::Syntax;


Box format(start[Machine] m) = format(m.top);


Box format((Machine)`machine <Id m> <Var* vs> <State* ss>`)
  = V(H("machine", "<m>", hs=1),
     V([format(s) | State s <- ss ], vs=2), vs=2);

Box format((State)`state <Id s> <Action* as> <Trans* ts> end`)
  = V(
   H("state", "<s>", hs=1),
   I([ format(t) | Trans t <- ts ], is=2),
   "end"
  );

Box format((Trans)`<Id e> =\> <Id trg>`)
  = H("<e>", "=\>", "<trg>", hs=1);