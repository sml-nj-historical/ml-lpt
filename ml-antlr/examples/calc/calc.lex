%name CalcLex;

%let digit = [0-9];
%let int = {digit}+;
%let alpha = [a-zA-Z];
%let id = {alpha}({alpha} | {digit})*;

%defs (
  open Tok
  type lex_result = token
);

let     => ( KW_let );
in      => ( KW_in );
{id}    => ( ID (yytext) );
{int}   => ( NUM (valOf (Int.fromString (yytext))) );
"="     => ( EQ );
"+"     => ( PLUS );
"-"     => ( MINUS );
"*"     => ( TIMES );
"("     => ( LP );
")"     => ( RP );
";"	=> ( SEMI );
" " | \n | \t
	=> ( continue() );
