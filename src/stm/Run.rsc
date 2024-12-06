module stm::Run

import salix::App;
import salix::Core;
import salix::Index;
import salix::HTML;

import stm::Syntax;
import IO;

// the current state a machine is in
alias RuntimeState = str; 


RuntimeState run(start[Machine] m, RuntimeState current, str event) {

    if (State s <- m.top.states, "<s.name>" == current) {
        if (Trans t <- s.transitions, "<t.event>" == event) {
            return "<t.target>";   
        }
    }

    return current;
}

RuntimeState run(start[Machine] m, RuntimeState current, list[str] events) 
  = ( current | run(m, it, event) | str event <- events );


/*
 * Interactive evaluator with Salix
 */

alias Model = tuple[start[Machine] machine, RuntimeState current];

// Salix "application event" type
data Msg = fire(str event);

// Update takes a message and produces a new model.
Model update(Msg msg, Model m) {
    switch (msg) {
        case fire(str event): {
            m.current = run(m.machine, m.current, event);
        }
    }
    return m;
}


// the view function "draws" HTML on an implicit "canvas"
void view(Model m) {
    h2("State machine: <m.machine.top.name>");
    for (/Trans t := m.machine) {
        button(onClick(fire("<t.event>")), "<t.event>");
    }
    for (State s <- m.machine.top.states) {
        div(() {
            if (m.current == "<s.name>") {
                span("*");    
            }
            span("<s.name>");
            ul(() {
                for (Trans t <- s.transitions) {
                    li("<t>");
                }
            });
        });
    }
}







App[Model] runSTM(start[Machine] m) = webApp(stmApp(m), |project://sle-master-course/src/|);

SalixApp[Model] stmApp(start[Machine] m, str id="root") 
  = makeApp(id, 
        Model() { return <m, initialState(m)>; }, 
        withIndex("<m.top.name>", id, view, css=["https://cdn.simplecss.org/simple.min.css"]), 
        update);


str initialState(start[Machine] m) = [ "<s.name>" | State s <- m.top.states ][0];
