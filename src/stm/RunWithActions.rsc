module stm::RunWithActions


import stm::Syntax;
import stm::Run;

alias RuntimeStateWithActions 
  = tuple[str current, map[str, bool] vars];


str initialState(start[Machine] m) = "<s.name>"
  when State s <- m.top.states;

RuntimeStateWithActions init(start[Machine] m) 
  = <initialState(m), ( "<x>": (Expr)`true` := e | (Var)`var <Id x> = <Expr e>` <- m.top.vars )>;


RuntimeStateWithActions runWithActions(start[Machine] m, RuntimeStateWithActions state, str event) {
    state.current = run(m, state.current, event);
    for (State s <- m.top.states, "<s.name>" == state.current, Action a <- s.actions) {
        state.vars["<a.name>"] = (Expr)`true` := a.expr; // eval(a.expr, state.vars);
    }
    return state;
}


RuntimeStateWithActions runWithActions(start[Machine] m, RuntimeStateWithActions current, list[str] events) 
  = ( current | runWithActions(m, it, event) | str event <- events );
