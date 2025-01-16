module stm::Syntax

extend lang::std::Layout;
extend lang::std::Id;


start syntax Machine
  = "machine" Id name Var* vars State* states;

syntax Var
  = "var" Id name "=" Expr;

syntax State
  = "state" Id name Action* actions Trans* transitions "end";


syntax Action = Id name "=" Expr expr;

syntax Expr = Bool;

syntax Bool = "true" | "false";

syntax Trans
  = Id event "=\>" Id target;


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

RefGraph resolve(start[Machine] m) = resolve(m.top);

RefGraph resolve(Machine m) {
    RefGraph ref = <{}, {}, {}>;

    ref.defs = {<"<s.name>", s.name.src> | State s <- m.states };
    ref.uses = {<t.target.src, "<t.target>"> | State s <- m.states, Trans t <- s.transitions };

    ref.defs += { <"<v.name>", v.name.src> | Var v <- m.vars };
    ref.uses += { <a.name.src, "<a.name>"> | State s <- m.states, Action a <- s.actions };

    ref.useDef = ref.uses o ref.defs;

    return ref;
}