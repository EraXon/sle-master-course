module Flatten

import Syntax;
import IO;
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
list[Question] flatten(start[Form] f) = flatten(f.top);

list[Question] flatten(Form f) = [*flatten((Expr)`true`,q)|Question q <- f.questions];

list[Question] flatten(Expr e, Question q) {
    list [Question] final = [];
    switch (q){
        case (Question)`<Str l> <Id i> : <Type t>` :{
            final += [(Question)`if (<Expr e>) <Question q>`];
        }
        case (Question)`<Str l> <Id i> : <Type t> = <Expr _>`:{
            final += [(Question)`if (<Expr e>) <Question q>`];
        }
        case (Question)`if (<Expr cond>) <Question then>`:{
            final += flatten((Expr)`<Expr e> && <Expr cond>`, then);
        }
        case (Question)`if (<Expr cond>) <Question then> else <Question els>`:{
            final+=flatten((Expr)`<Expr e> && <Expr cond>`, then);
            final+=flatten((Expr)`<Expr e> && !(<Expr cond>)`, els);
        }
        case (Question)`{<Question*  qs>}`:{
            for (Question q <- qs) {
                final += flatten(e, q);
            }
        }
    }

    return final;
}

// helper function to go back to a proper questionnaire term.
start[Form] unflatten(list[Question] qs, start[Form] org) {
    Str title = org.top.title;
    Form f = (Form)`form <Str title> {}`;
    for (Question q <- qs, (Form)`form <Str t> {<Question* qqs>}` := f) {
        f = (Form)`form <Str t> {
                  '  <Question* qqs>
                  '  <Question q>
                  '}`;
    }
    return org[top=f];
}
