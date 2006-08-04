(* a subset of Pascal, a la the appendix to the Dragon book *)

%defs (
  val cat     = String.concat
  val catSp   = String.concatWith " "
  val catNl   = String.concatWith "\n"
  val catNlNl = String.concatWith "\n\n"
  val catCm   = String.concatWith ", "
  val catSemi = String.concatWith "; "
  val catSemiNl = String.concatWith ";\n"
);

%tokens
  : KW_program          ("program")
  | KW_var
  | KW_array
  | KW_of
  | KW_integer
  | KW_real
  | KW_function
  | KW_procedure
  | KW_begin
  | KW_end
  | KW_if
  | KW_then
  | KW_else
  | KW_while
  | KW_do
  | KW_not
  | ID of string
  | INT of IntInf.int
  | REAL of Real.real
  | ASSIGNOP	(* := *)
  | COMMA	(* , *)
  | COLON	(* : *)
  | SEMI	(* ; *)
  | DOT		(* . *)
  | LSB		(* [ *)
  | RSB		(* ] *)
  | LP		(* ( *)
  | RP		(* ) *)
  | MINUS
  | RELOP of string
  | ADDOP of string
  | MULOP of string
  ;

program
  : "program" ID LP id_list RP SEMI
      (declaration)*
      (subprogram_declaration)* 
      compound_statement
      DOT
      => ( catNl [ 
	 cat ["program ", ID, "(", id_list, ");"],
	 catNl   SR1, catNlNl SR2,
	 compound_statement ^ "."
      ])
  ;

id_list
  : ID (COMMA ID)* => ( catCm (ID::SR1) )
  ;

declaration
  : KW_var id_list_type SEMI => ( cat ["var ", id_list_type, ";"] )
  ;

compound_type
  : standard_type
  | KW_array LSB INT DOT DOT INT RSB KW_of standard_type
      => ( cat ["array [", IntInf.toString INT1, "..", IntInf.toString INT2, "] of ", standard_type] )
  ;

standard_type
  : KW_integer	=> ( "integer" )
  | KW_real	=> ( "real" )
  ;

id_list_type
  : id_list COLON compound_type => ( catSp [id_list, ":", compound_type] )
  ;

subprogram_declaration
  : subprogram_head (declaration)* compound_statement SEMI
      => ( catNl [subprogram_head, catNl SR1, compound_statement ^ ";"] )
  ;

subprogram_head
  : KW_function ID arguments COLON standard_type SEMI
      => ( cat ["function ", ID, arguments, " : ", standard_type, ";"] )
  | KW_procedure ID arguments  SEMI
      => ( cat ["procedure ", ID, arguments, ";"] )
  ;

arguments
  : LP parameter_list RP	=> ( "(" ^ parameter_list ^ ")" )
  |				=> ( "" )
  ;

parameter_list
  : id_list_type (SEMI id_list_type)* => ( catSemi (id_list_type::SR1) )
  ;

compound_statement
  : KW_begin (statement (SEMI statement)* => ( catSemiNl (statement::SR1) ))? KW_end
    => ( catNl ["begin", getOpt(SR1, ""), "end"] )
  ;

statement
  : variable ASSIGNOP exp	
      => ( cat [variable, " := ", exp] )
  | procedure_statement
  | compound_statement
  | KW_if exp KW_then statement KW_else statement
      => ( cat ["if ", exp, " then ", statement1, " else ", statement2] )
  | KW_while exp KW_do statement
      => ( cat ["while ", exp, " do ", statement] )
  ;

variable
  : ID
  | ID LSB exp RSB => ( cat [ID, "[", exp, "]"] )
  ;

procedure_statement
  : ID
  | ID LP exp (COMMA exp)* RP => ( cat [ID, "(", catCm (exp::SR1), ")" ] )
  ;

exp
  : simple_exp (RELOP simple_exp => ( RELOP ^ " " ^ simple_exp ))? 
      => ( catSp [simple_exp, getOpt(SR1, "")] )
  ;

simple_exp
  : signed_term (ADDOP signed_term => ( ADDOP ^ " " ^signed_term ))* 
      => ( catSp (signed_term::SR1) )
  ;

signed_term
  : MINUS term => ( "-" ^ term )
  | term
  ;
  
term
  : factor (MULOP factor => ( MULOP ^ " " ^ factor ))* 
      => ( catSp (factor::SR1) )
  ;

factor
  : variable
  | ID LP exp (COMMA exp)* RP => ( cat [ID, "(", catCm (exp::SR1), ")" ] )
  | INT			=> ( IntInf.toString INT )
  | REAL		=> ( Real.toString REAL )
  | LP exp RP		=> ( "(" ^ exp ^ ")" )
  | KW_not factor	=> ( "not " ^ factor )
  ;
