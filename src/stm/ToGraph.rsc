module stm::ToGraph

import stm::Syntax;
import vis::Graphs;

alias StmGraph = lrel[str from, str event, str to];

StmGraph toGraph(start[Machine] m) = toGraph(m.top);

StmGraph toGraph(Machine m) {
    StmGraph g = [];

    for (State s <- m.states, Trans t <- s.transitions) {
        g += [<"<s.name>", "<t.event>", "<t.target>">];
    }

    return g;
}
