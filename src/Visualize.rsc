module Visualize

import vis::Graphs;
import Syntax;
import Content;
import ParseTree;
import IO;
// Identity of a node
alias DepId = tuple[Id name, loc location];

alias DepGraph = lrel[DepId from, str kind, DepId to];


Content visualizeDeps(start[Form] form) = 
    graph(form2deps(form.top), 
        \layout=defaultCoseLayout(), 
        nodeLabeler=str (DepId d) { return "<d.name>"; }, 
        nodeLinker=loc (DepId d) { return d.location; });



// extra control/data dependencies from a form
// use the kind field in DepGraph to indicate whether it's a data dependency or a control dependency.
DepGraph form2deps(Form f){
    DepGraph deps = [];

    visit (f){
        case q:(Question)`<Str _> <Id id> : <Type _> = <Expr e>`:{
            DepId from=<id, q.src>;
            deps+= [<from, "data", <x, x.src>> | /Id x:=e];
        }

    }

    visit (f){        

        case (Question)`if ( <Expr cond> ) <Question then>`:{

            deps+= [<<x,x.src>, "control", <q.id, q.src>> | /Id x:=cond, /Question q:=then, q has id];
        }
        case (Question)`if (<Expr cond>) <Question then> else <Question else1>`:{

            deps+= [<<x,x.src>, "control", <q.id, q.src>> | /Id x:=cond, /Question q:=then, q has id];
            deps+= [<<x,x.src>, "control", <q.id, q.src>> | /Id x:=cond, /Question q:=else1, q has id];
        }

    }
    for (dep<-deps){
        
        println(dep.from.name);
    }



    return deps;

}