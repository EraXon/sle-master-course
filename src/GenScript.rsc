module GenScript

import Syntax;
import Eval;
import util::Math;
import String;
import List;


/*
state S0
derive enabled questions
select one of them (random), fill in with a random value
  but not one you’ve already selected
  if such is not available, pick one earlier selected, but different value

update state
repeat

*/

lrel[Input, VEnv] genScript(start[Form] form, int length=10) {
    VEnv venv = initialEnv(form);
    set[str] seen = {};
    return for (int _ <- [0..length]) {
        list[Question] candidates = [ q | Question q <- render(form, venv), !(q has expr) ];
        list[Question] unseenCandidates = [ q | Question q <- candidates, "<q.name>" notin seen ];
        if (unseenCandidates == []) {
            // start over
            unseenCandidates = candidates;
            seen = {};
        }

        if (unseenCandidates != []) {
            int i = arbInt(size(unseenCandidates));
            Question focus = unseenCandidates[i];
            seen += {"<focus.name>"};
            Input inp = user("<focus.name>", arbValue(focus.\type));
            venv = eval(form, inp, venv);
            append <inp, venv>;
        }
    }
}

Value arbValue((Type)`integer`) = vint(arbInt(5000));
Value arbValue((Type)`boolean`) = vbool(arbInt(2) == 1);
Value arbValue((Type)`string`) = vstr(arbString(arbInt(100)));