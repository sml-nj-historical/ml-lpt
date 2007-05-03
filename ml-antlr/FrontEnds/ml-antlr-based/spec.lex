(* spec.lex
 *
 * COPYRIGHT (c) 2006 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * (With some code borrowed from ml-yacc)
 *)

%defs (

structure Tok = SpecTokens

val comLvl : int ref = ref 0		(* nesting depth of comments *)
val comStart : int ref = ref 0		(* start line of current comment *)

type lex_result = Tok.token

fun err _ = ()

fun eof () = (
      if (!comLvl > 0)
        then ()
(* err(~1, "unclosed comment starting at line " ^ Int.toString(!comStart)) *)
        else ();
      Tok.EOF)

val text : string list ref = ref []
fun addText s = (text := s::(!text))
fun clrText () = (text := [])
fun getText () = concat (rev (!text))

val pcount = ref 0
fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)

);

%let eol=("\n"|"\013\n"|"\013");
%let ws=("\009"|"\011"|"\012"|" "|{eol});
%let lc=[a-z];
%let uc=[A-Z];
%let alpha=({lc}|{uc});
%let digit=[0-9];
%let int=digit*;
%let idchars=({alpha}|{digit}|"_"|"'");
%let id={alpha}{idchars}*;
%let qualid ={id}".";
%let tyvar="'"{idchars}*;

%states STRING COM CODE CONSTR;

%name SpecLex;

<INITIAL>"of"	=> (YYBEGIN CONSTR; Tok.OF);

<INITIAL>{ws}+	=> (skip());
<INITIAL>{id}		=> (Tok.ID yytext);

<INITIAL>"%token"("s")?	=> (YYBEGIN CONSTR; Tok.KW_tokens);
<INITIAL>"%defs"	=> (YYBEGIN CODE; clrText(); Tok.KW_defs);
<INITIAL>"%keyword"("s")?	=> (Tok.KW_keywords);
<INITIAL>"%nonterm"("s")?	=> (Tok.KW_nonterms);
<INITIAL>"%import"	=> (Tok.KW_import);
<INITIAL>"%name"	=> (Tok.KW_name);
<INITIAL>"%start"	=> (Tok.KW_start);
<INITIAL>"%entry"	=> (Tok.KW_entry);
<INITIAL>"%try"		=> (Tok.KW_try);
<INITIAL>"%where"       => (YYBEGIN CODE; clrText(); Tok.KW_where);
<INITIAL>"%dropping"	=> (Tok.KW_dropping);
<INITIAL>"%refcell"	=> (YYBEGIN CONSTR; Tok.KW_refcell);

<INITIAL>"|"	=> (Tok.BAR); 
<INITIAL>"@"	=> (YYBEGIN CODE; clrText(); Tok.AT); 
<INITIAL>"$"	=> (Tok.DOLLAR); 
<INITIAL>"+"	=> (Tok.PLUS);
<INITIAL>"*"	=> (Tok.STAR);
<INITIAL>"?"	=> (Tok.QUERY);
<INITIAL>":"	=> (Tok.COLON);
<INITIAL>";"	=> (Tok.SEMI);
<INITIAL>","	=> (Tok.COMMA);
<INITIAL>"("	=> (Tok.LP);
<INITIAL>")"	=> (Tok.RP);
<INITIAL>"["	=> (Tok.LSB);
<INITIAL>"]"	=> (Tok.RSB);
<INITIAL>"/"	=> (Tok.SLASH);
<INITIAL>"="	=> (Tok.EQ);
<INITIAL>"->"	=> (Tok.ARROW);
<INITIAL>"=>"	=> (YYBEGIN CODE; clrText(); Tok.DARROW);
<INITIAL>"\""	
	        => (YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN INITIAL);
		    Tok.STRING (getText()));

<INITIAL>"(*" 
	=> (comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN INITIAL);
	    continue());
<CONSTR>"(*"
	=> (comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN CONSTR);
	    continue());
<CODE>"(*"
	=> (comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN CODE);
	    continue());

<COM>"(*" 
	=> (comLvl := !comLvl+1; continue());
<COM>"*)"        
	=> (comLvl := !comLvl-1; 
	    if (!comLvl = 0) 
	      then (Tok.BOGUS) 
	      else continue());
<COM>.|{eol}
	=> (continue());

<CODE>"("	=> (if !pcount = 0 then () else addText yytext;
		    inc pcount; continue());
<CODE>")"	=> (dec pcount; 
		    if !pcount = 0 then
		      (YYBEGIN INITIAL;
		       Tok.CODE (getText()))
		    else (addText yytext; continue()));
<CODE>"\""	=> (addText yytext; YYBEGIN STRING; 
		    ignore(continue() before YYBEGIN CODE);
		    continue());
<CODE>[^()"]+	=> (addText yytext; continue());

<STRING>"\""	=> (addText yytext; Tok.BOGUS);
<STRING>{eol}	=> (addText yytext; err (!yylineno, "unclosed string");
 	            Tok.BOGUS);
<STRING>\\	=> (addText yytext; continue());
<STRING>\\\\	=> (addText yytext; continue());
<STRING>[^"\\\n\013]+ 
		=> (addText yytext; continue());
<STRING>\\\"	=> (addText yytext; continue());

<CONSTR>{ws}	=> (continue());
<CONSTR>"of"	=> (Tok.OF);
<CONSTR>{id}	=> (Tok.ID yytext);
<CONSTR>{tyvar} => (Tok.TYVAR yytext);
<CONSTR>{qualid}=> (Tok.IDDOT yytext);
<CONSTR>{int}	=> (Tok.INT yytext);
<CONSTR>"|"	=> (Tok.BAR); 
<CONSTR>"*"	=> (Tok.STAR);
<CONSTR>":"	=> (Tok.COLON);
<CONSTR>";"	=> (YYBEGIN INITIAL; Tok.SEMI);
<CONSTR>"("	=> (Tok.LP);
<CONSTR>")"	=> (Tok.RP);
<CONSTR>"{"	=> (Tok.LCB);
<CONSTR>"}"	=> (Tok.RCB);
<CONSTR>"->"	=> (Tok.ARROW);
<CONSTR>"\""	=> (YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN CONSTR);
		    Tok.STRING (getText()));
<CONSTR>"="	=> (YYBEGIN CODE; clrText(); Tok.EQ);

.	=> (err (!yylineno, 
		 concat["illegal character '", 
			String.toCString yytext, "'"]);
	    continue());
