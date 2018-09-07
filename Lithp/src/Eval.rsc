module Eval

import ParseTree;
import IO;
import String;
import Syntax;




public int eval((Sexpr) `<NUM a>`) {
	n = toInt("<a>"); 
	return n;
}



public int eval((Sexpr)`+<Sexpr a><Sexpr b>`) {
	n = eval("<a>"); 
	m = eval("<b>");
	return n+m;
}


public int eval((Sexpr)`*<Sexpr a> <Sexpr b>`) {
	n = eval("<a>"); 
	m = eval("<b>");
	return n*m;
}

public int eval((Sexpr)`-<Sexpr a> <Sexpr b>`) {
	n = eval("<a>"); 
	m = eval("<b>");
	return n-m;
}

// TODO MAY RETURN A FLOAT. 
public int eval((Sexpr)`/<Sexpr a> <Sexpr b>`) {
	n = eval("<a>"); 
	m = eval("<b>");
	return n/m;
}