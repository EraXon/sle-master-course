module stm::Check

import stm::Syntax;
import ParseTree;
import Message;
import IO;

/*

Errors
- Transition to undefined state
- Duplicate definition of state
- Unreachable state

Warnings
- Non-determinism
- Self transition

*/

set[Message] check(start[Machine] m, RefGraph refs) {
    set[Message] msgs = {};

    msgs += { error("undefined state", u) |
        <loc u, str x> <- refs.uses, !(<x, _> <- refs.defs) };

    msgs += { error("duplicate state", d) |
        str x <- refs.defs<name>, {_} !:= refs.defs[x],
            loc d <- refs.defs[x] };


    rel[loc, loc] reach = { <s.name.src, y> |
        State s <- m.top.states,
        Trans t <- s.transitions,
        <loc u, "<t.target>"> <- refs.uses,
        <u, loc y> <- refs.useDef };

    if (State initial <- m.top.states) {
        loc x = initial.name.src;
        msgs += { error("unreachable state", d) |
            <_, loc d> <- refs.defs, d notin (reach+)[x] };
    }    


   return msgs;
}
