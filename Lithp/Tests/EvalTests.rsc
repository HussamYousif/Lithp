module EvalTests

import parseTree;

test bool parseTest1() {
    parse(eval(#Sexpr, "+ 2 2"));
    return true; 
}