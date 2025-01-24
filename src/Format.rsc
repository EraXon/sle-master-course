module Format

import util::SimpleBox;
import Syntax;

/*
 * Formatting: transforming QL forms to Box 
 */

str formatQL(start[Form] form) = format(form2box(form.top));

Box form2box((Form)`form <Str title> { <Question* questions> }`) = V(V(H("form", "<title>", hs=1),"{",
     I(V([form2box(q) | Question q <- questions ], vs=2), vs=2)),"}");

Box form2box((Question)`<Str label> <Id id> : <Type t>`) = I(H("<label>", "<id>", ":", "<t>", hs=1));

Box form2box((Question)`<Str label> <Id id> : <Type t> = <Expr e>`) = I(H("<label>", "<id>", ":", "<t>", "=", exprToStr(e), hs=1));

Box form2box((Question)`{ <Question* questions> }`) = V("{",I([form2box(q) | Question q <- questions ]),"}", vs=1);

Box form2box((Question)`if ( <Expr cond> ) <Question then>`) = H("if", "(", exprToStr(cond), ")", form2box(then), hs=1);

Box form2box((Question)`if ( <Expr cond> ) <Question then> else <Question else1>`) = V(H("if", "(", exprToStr(cond), ")"), H(form2box(then)), H("else",form2box(else1),hs=1), vs=2);




str exprToStr((Expr)`<Expr lhs>+ <Expr rhs>`) = "<exprToStr(lhs)> + <exprToStr(rhs)>";
str exprToStr((Expr)`<Expr lhs>* <Expr rhs>`) = "<exprToStr(lhs)> * <exprToStr(rhs)>";
str exprToStr((Expr)`<Expr lhs>/ <Expr rhs>`) = "<exprToStr(lhs)> / <exprToStr(rhs)>";
str exprToStr((Expr)`<Expr lhs>- <Expr rhs>`) = "<exprToStr(lhs)> - <exprToStr(rhs)>";
str exprToStr((Expr)`<Expr lhs> && <Expr rhs>`) = "<exprToStr(lhs)> && <exprToStr(rhs)>";
str exprToStr((Expr)`<Expr lhs> || <Expr rhs>`) = "<exprToStr(lhs)> || <exprToStr(rhs)>";
str exprToStr((Expr)`!<Expr e>`) = "!<exprToStr(e)>";
str exprToStr((Expr)`<Id x>`) = "<x>";
str exprToStr((Expr)`<Int n>`) = "<n>";
str exprToStr((Expr)`<Str s>`) = "<s>";
str exprToStr((Expr)`<Bool b>`) = "<b>";
str exprToStr((Expr)`(<Expr e>)`) = "<exprToStr(e)>";
str exprToStr((Expr)`<Expr lhs> \> <Expr rhs>`) = "<exprToStr(lhs)> \> <exprToStr(rhs)>";
str exprToStr((Expr)`<Expr lhs> \< <Expr rhs>`) = "<exprToStr(lhs)> \< <exprToStr(rhs)>";
str exprToStr((Expr)`<Expr lhs> == <Expr rhs>`) = "<exprToStr(lhs)> == <exprToStr(rhs)>";
str exprToStr((Expr)`<Expr lhs> != <Expr rhs>`) = "<exprToStr(lhs)> != <exprToStr(rhs)>";
str exprToStr((Expr)`<Expr lhs> \<= <Expr rhs>`) = "<exprToStr(lhs)> \<= <exprToStr(rhs)>";
str exprToStr((Expr)`<Expr lhs> \>= <Expr rhs>`) = "<exprToStr(lhs)> \>= <exprToStr(rhs)>";


