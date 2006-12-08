%name CalcLex;

%let digit = [0-9];
%let int = {digit}+;
%let alpha = [a-zA-Z];
%let id = {alpha}({alpha} | {digit})*;

%defs (
  open CalcParseToks
  type lex_result = token

  fun eof() = EOF
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
