module Syntax

// TODO expand with lists, functions and other interesting data. 
syntax Sexpr =
	 | NUM
	 | "+" NUM NUM
	 ;


lexical Atom = [a-zA-Z] !<< [a-zA-Z] !>> [a-zA-Z]; // TODO Add aditional symbols for the  	 

lexical NUM = [0-9] !<< [0-9] !>> [0-9];



