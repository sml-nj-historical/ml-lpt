structure Tok = struct

    datatype token = EOF
      | MULOP of string
      | ADDOP of string
      | RELOP of string
      | MINUS
      | RP
      | LP
      | RSB
      | LSB
      | DOT
      | SEMI
      | COLON
      | COMMA
      | ASSIGNOP
      | REAL of Real.real
      | INT of IntInf.int
      | ID of string
      | KW_not
      | KW_do
      | KW_while
      | KW_else
      | KW_then
      | KW_if
      | KW_end
      | KW_begin
      | KW_procedure
      | KW_function
      | KW_real
      | KW_integer
      | KW_of
      | KW_array
      | KW_var
      | KW_program

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (MULOP(_)) => "MULOP"
  | (ADDOP(_)) => "ADDOP"
  | (RELOP(_)) => "RELOP"
  | (MINUS) => "MINUS"
  | (RP) => ")"
  | (LP) => "("
  | (RSB) => "]"
  | (LSB) => "["
  | (DOT) => "."
  | (SEMI) => ";"
  | (COLON) => ":"
  | (COMMA) => ","
  | (ASSIGNOP) => ":="
  | (REAL(_)) => "REAL"
  | (INT(_)) => "INT"
  | (ID(_)) => "ID"
  | (KW_not) => "KW_not"
  | (KW_do) => "KW_do"
  | (KW_while) => "KW_while"
  | (KW_else) => "KW_else"
  | (KW_then) => "KW_then"
  | (KW_if) => "KW_if"
  | (KW_end) => "KW_end"
  | (KW_begin) => "KW_begin"
  | (KW_procedure) => "KW_procedure"
  | (KW_function) => "KW_function"
  | (KW_real) => "KW_real"
  | (KW_integer) => "KW_integer"
  | (KW_of) => "KW_of"
  | (KW_array) => "KW_array"
  | (KW_var) => "KW_var"
  | (KW_program) => "program"
(* end case *))
    fun isKW tok =
(case (tok)
 of (EOF) => false
  | (MULOP(_)) => false
  | (ADDOP(_)) => false
  | (RELOP(_)) => false
  | (MINUS) => false
  | (RP) => false
  | (LP) => false
  | (RSB) => false
  | (LSB) => false
  | (DOT) => false
  | (SEMI) => false
  | (COLON) => false
  | (COMMA) => false
  | (ASSIGNOP) => false
  | (REAL(_)) => false
  | (INT(_)) => false
  | (ID(_)) => false
  | (KW_not) => true
  | (KW_do) => true
  | (KW_while) => true
  | (KW_else) => true
  | (KW_then) => true
  | (KW_if) => true
  | (KW_end) => true
  | (KW_begin) => true
  | (KW_procedure) => true
  | (KW_function) => true
  | (KW_real) => true
  | (KW_integer) => true
  | (KW_of) => true
  | (KW_array) => true
  | (KW_var) => true
  | (KW_program) => true
(* end case *))


end (* structure Tok *)

signature LEXER = sig

  type strm
  type pos
  type span = pos * pos

  val lex : strm -> (Tok.token * span * strm) option
  val getPos : strm -> pos

end (* signature LEXER *)

signature REPAIRABLE = sig

  type T
  datatype repair
    = Deletion
    | Insertion of Tok.token
    | Substitution of Tok.token
  exception RepairableError

  val farEnoughWindow : {
	startAt : T,
	endAt : T
      } -> bool

  val farEnoughRepair : {
	startAt : T,
	endAt : T
      } -> bool

  val skip : T * int -> T 
  val isEmpty : T -> bool

  val chooseRepair : {
	startAt : T,
	endAt : T,
	try : T -> T
      } -> {
        errorAt : T, 
	repair : repair,
	repaired : T
      } option

end

functor Parser(Lex : LEXER)(* : sig

    datatype repair_action
      = Insert of Tok.token list
      | Delete of Tok.token list
      | Subst of {
	    old : Tok.token list, 
	    new : Tok.token list
	}

    val parse : 

  end *) = struct

    structure UserCode = struct

      type antlr_annotation = unit

 
  val cat     = String.concat
  val catSp   = String.concatWith " "
  val catNl   = String.concatWith "\n"
  val catNlNl = String.concatWith "\n\n"
  val catCm   = String.concatWith ", "
  val catSemi = String.concatWith "; "
  val catSemiNl = String.concatWith ";\n"


fun program_PROD_1_ACT (ID, LP, RP, DOT, EOF, SR1, SR2, SEMI, compound_statement, id_list, KW_program, ID_SPAN : Lex.span, LP_SPAN : Lex.span, RP_SPAN : Lex.span, DOT_SPAN : Lex.span, EOF_SPAN : Lex.span, SR1_SPAN : Lex.span, SR2_SPAN : Lex.span, SEMI_SPAN : Lex.span, compound_statement_SPAN : Lex.span, id_list_SPAN : Lex.span, KW_program_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  catNl [ 
	 cat ["program ", ID, "(", id_list, ");"],
	 catNl   SR1, catNlNl SR2,
	 compound_statement ^ "."
      ])
fun id_list_PROD_1_ACT (ID, SR, ID_SPAN : Lex.span, SR_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  catCm (ID::SR) )
fun declaration_PROD_1_ACT (SEMI, id_list_type, KW_var, SEMI_SPAN : Lex.span, id_list_type_SPAN : Lex.span, KW_var_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  cat ["var ", id_list_type, ";"] )
fun compound_type_PROD_2_ACT (LSB, RSB, DOT1, DOT2, INT1, INT2, KW_array, standard_type, KW_of, LSB_SPAN : Lex.span, RSB_SPAN : Lex.span, DOT1_SPAN : Lex.span, DOT2_SPAN : Lex.span, INT1_SPAN : Lex.span, INT2_SPAN : Lex.span, KW_array_SPAN : Lex.span, standard_type_SPAN : Lex.span, KW_of_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  cat ["array [", IntInf.toString INT1, "..", IntInf.toString INT2, "] of ", standard_type] )
fun standard_type_PROD_1_ACT (KW_integer, KW_integer_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  "integer" )
fun standard_type_PROD_2_ACT (KW_real, KW_real_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  "real" )
fun id_list_type_PROD_1_ACT (compound_type, COLON, id_list, compound_type_SPAN : Lex.span, COLON_SPAN : Lex.span, id_list_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  catSp [id_list, ":", compound_type] )
fun subprogram_declaration_PROD_1_ACT (SR, SEMI, compound_statement, subprogram_head, SR_SPAN : Lex.span, SEMI_SPAN : Lex.span, compound_statement_SPAN : Lex.span, subprogram_head_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  catNl [subprogram_head, catNl SR, compound_statement ^ ";"] )
fun subprogram_head_PROD_1_ACT (ID, SEMI, standard_type, COLON, arguments, KW_function, ID_SPAN : Lex.span, SEMI_SPAN : Lex.span, standard_type_SPAN : Lex.span, COLON_SPAN : Lex.span, arguments_SPAN : Lex.span, KW_function_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  cat ["function ", ID, arguments, " : ", standard_type, ";"] )
fun subprogram_head_PROD_2_ACT (ID, SEMI, KW_procedure, arguments, ID_SPAN : Lex.span, SEMI_SPAN : Lex.span, KW_procedure_SPAN : Lex.span, arguments_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  cat ["procedure ", ID, arguments, ";"] )
fun arguments_PROD_1_ACT (LP, RP, parameter_list, LP_SPAN : Lex.span, RP_SPAN : Lex.span, parameter_list_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  "(" ^ parameter_list ^ ")" )
fun arguments_PROD_2_ACT (addAnnotation, FULL_SPAN : Lex.span) = 
  (  "" )
fun parameter_list_PROD_1_ACT (SR, id_list_type, SR_SPAN : Lex.span, id_list_type_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  catSemi (id_list_type::SR) )
fun compound_statement_PROD_1_compound_statement_SR1_PROD_1_ACT (SR, KW_begin, statement, SR_SPAN : Lex.span, KW_begin_SPAN : Lex.span, statement_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  catSemiNl (statement::SR) )
fun compound_statement_PROD_1_ACT (SR, KW_begin, KW_end, SR_SPAN : Lex.span, KW_begin_SPAN : Lex.span, KW_end_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  catNl ["begin", getOpt(SR, ""), "end"] )
fun statement_PROD_1_ACT (exp, variable, ASSIGNOP, exp_SPAN : Lex.span, variable_SPAN : Lex.span, ASSIGNOP_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  cat [variable, " := ", exp] )
fun statement_PROD_4_ACT (exp, KW_else, KW_then, statement1, statement2, KW_if, exp_SPAN : Lex.span, KW_else_SPAN : Lex.span, KW_then_SPAN : Lex.span, statement1_SPAN : Lex.span, statement2_SPAN : Lex.span, KW_if_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  cat ["if ", exp, " then ", statement1, " else ", statement2] )
fun statement_PROD_5_ACT (exp, KW_while, KW_do, statement, exp_SPAN : Lex.span, KW_while_SPAN : Lex.span, KW_do_SPAN : Lex.span, statement_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  cat ["while ", exp, " do ", statement] )
fun variable_PROD_2_ACT (ID, LSB, RSB, exp, ID_SPAN : Lex.span, LSB_SPAN : Lex.span, RSB_SPAN : Lex.span, exp_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  cat [ID, "[", exp, "]"] )
fun procedure_statement_PROD_2_ACT (ID, LP, RP, SR, exp, ID_SPAN : Lex.span, LP_SPAN : Lex.span, RP_SPAN : Lex.span, SR_SPAN : Lex.span, exp_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  cat [ID, "(", catCm (exp::SR), ")" ] )
fun exp_PROD_1_exp_SR1_PROD_1_ACT (simple_exp, RELOP, simple_exp_SPAN : Lex.span, RELOP_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  RELOP ^ " " ^ simple_exp )
fun exp_PROD_1_ACT (SR, simple_exp, SR_SPAN : Lex.span, simple_exp_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  catSp [simple_exp, getOpt(SR, "")] )
fun simple_exp_PROD_1_simple_exp_SR1_PROD_1_ACT (signed_term, ADDOP, signed_term_SPAN : Lex.span, ADDOP_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  ADDOP ^ " " ^signed_term )
fun simple_exp_PROD_1_ACT (SR, signed_term, SR_SPAN : Lex.span, signed_term_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  catSp (signed_term::SR) )
fun signed_term_PROD_1_ACT (term, MINUS, term_SPAN : Lex.span, MINUS_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  "-" ^ term )
fun term_PROD_1_term_SR1_PROD_1_ACT (factor, MULOP, factor_SPAN : Lex.span, MULOP_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  MULOP ^ " " ^ factor )
fun term_PROD_1_ACT (SR, factor, SR_SPAN : Lex.span, factor_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  catSp (factor::SR) )
fun factor_PROD_2_ACT (ID, LP, RP, SR, exp, ID_SPAN : Lex.span, LP_SPAN : Lex.span, RP_SPAN : Lex.span, SR_SPAN : Lex.span, exp_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  cat [ID, "(", catCm (exp::SR), ")" ] )
fun factor_PROD_3_ACT (INT, INT_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  IntInf.toString INT )
fun factor_PROD_4_ACT (REAL, REAL_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  Real.toString REAL )
fun factor_PROD_5_ACT (LP, RP, exp, LP_SPAN : Lex.span, RP_SPAN : Lex.span, exp_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  "(" ^ exp ^ ")" )
fun factor_PROD_6_ACT (factor, KW_not, factor_SPAN : Lex.span, KW_not_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span) = 
  (  "not " ^ factor )

    end

    (* "wrapped" streams, which track the number of tokens read
     * and allow "prepending" a sequence of tokens
     *)
    structure WStream = struct

      datatype 'a wstream = WSTREAM of {
	prefix : (Tok.token * Lex.span) list,
	curTok : int,
	strm : 'a
      }

      fun wrap strm =  WSTREAM {prefix = [], strm = strm, curTok = 0}
      fun unwrap (WSTREAM {strm, ...}) = strm

      fun get1 (WSTREAM {prefix = (tok, span)::toks, strm, curTok}) = 
	    (tok, span, WSTREAM {prefix = toks, strm = strm, curTok = curTok + 1})
	| get1 (WSTREAM {prefix = [], strm, curTok}) = let
	    val (tok, span, strm') = case Lex.lex strm
		of SOME x => x
		 | NONE => (Tok.EOF, (Lex.getPos strm, Lex.getPos strm), strm)
	    in (tok, span, WSTREAM {prefix = [], strm = strm', curTok = curTok + 1})
	    end

      fun prepend (toks, WSTREAM {prefix, strm, curTok}) = 
	    WSTREAM {prefix = toks @ prefix, strm = strm, curTok = curTok - (List.length toks)}

      fun subtract (WSTREAM {curTok = p1, ...}, WSTREAM {curTok = p2, ...}) = 
	    p1 - p2

      fun getDiff (ws1, ws2) =
	    if subtract (ws1, ws2) <= 0 then []
	    else let 
		val (t, s, ws2') = get1 ws2
	        in (t, s) :: (getDiff (ws1, ws2'))
                end

      fun getPos ws = let val (_, (left, _), _) = get1 ws in left end
      fun getSpan ws = (getPos ws, getPos ws)

    end (* structure WStream *)
    
    structure EBNF = struct

      fun optional (pred, parse, strm) = 
	    if pred strm
    	    then let
	      val (y, span, strm') = parse strm
	      in 
		(SOME y, span, strm')
	      end
	    else (NONE, WStream.getSpan strm, strm)

      fun closure (pred, parse, strm) = let
            fun iter (strm, (left, right), ys) = 
	          if pred strm
		  then let
		    val (y, (_, right'), strm') = parse strm
		    in iter (strm', (left, right'), y::ys)
		    end
		  else (List.rev ys, (left, right), strm)
            in
              iter (strm, WStream.getSpan strm, [])
            end

      fun posclos (pred, parse, strm) = let
            val (y, (left, _), strm') = parse strm
	    val (ys, (_, right), strm'') = closure (pred, parse, strm')
            in
              (y::ys, (left, right), strm'')
            end

    end (* structure EBNF *)

    structure RepairableStrm : REPAIRABLE = struct

      structure WS = WStream
      type T = Lex.strm WS.wstream
      exception RepairableError

      val minAdvance = 1

      fun farEnoughWindow {startAt, endAt} =
	    WS.subtract (endAt, startAt) > 15

      fun farEnoughRepair {startAt, endAt} =
	    WS.subtract (endAt, startAt) > minAdvance + 1

      datatype repair
	= Deletion
	| Insertion of Tok.token
	| Substitution of Tok.token

    val allRepairs = [Deletion, Insertion Tok.EOF, Insertion Tok.MINUS, Insertion Tok.RP, Insertion Tok.LP, Insertion Tok.RSB, Insertion Tok.LSB, Insertion Tok.DOT, Insertion Tok.SEMI, Insertion Tok.COLON, Insertion Tok.COMMA, Insertion Tok.ASSIGNOP, Insertion Tok.KW_not, Insertion Tok.KW_do, Insertion Tok.KW_while, Insertion Tok.KW_else, Insertion Tok.KW_then, Insertion Tok.KW_if, Insertion Tok.KW_end, Insertion Tok.KW_begin, Insertion Tok.KW_procedure, Insertion Tok.KW_function, Insertion Tok.KW_real, Insertion Tok.KW_integer, Insertion Tok.KW_of, Insertion Tok.KW_array, Insertion Tok.KW_var, Insertion Tok.KW_program, Substitution Tok.EOF, Substitution Tok.MINUS, Substitution Tok.RP, Substitution Tok.LP, Substitution Tok.RSB, Substitution Tok.LSB, Substitution Tok.DOT, Substitution Tok.SEMI, Substitution Tok.COLON, Substitution Tok.COMMA, Substitution Tok.ASSIGNOP, Substitution Tok.KW_not, Substitution Tok.KW_do, Substitution Tok.KW_while, Substitution Tok.KW_else, Substitution Tok.KW_then, Substitution Tok.KW_if, Substitution Tok.KW_end, Substitution Tok.KW_begin, Substitution Tok.KW_procedure, Substitution Tok.KW_function, Substitution Tok.KW_real, Substitution Tok.KW_integer, Substitution Tok.KW_of, Substitution Tok.KW_array, Substitution Tok.KW_var, Substitution Tok.KW_program]


      fun applyRepair ([], repair) = 
	    raise Fail "applyRepair: expected nonempty working list"
	| applyRepair (working, Deletion) = tl working
	| applyRepair ((t, span)::working, Insertion tok) = 
	    (tok, (#2 span, #2 span)) :: (t, span) :: working
	| applyRepair ((_, span)::working, Substitution tok) = 
	    (tok, span) :: working

      fun getWorking (strm, n, accum) = 
	    if n = 0 
	    then (strm, rev accum)
	    else let
	      val (tok, s, strm') = WS.get1 strm
	      in case tok
		  of Tok.EOF => (strm', rev ((Tok.EOF, WStream.getSpan strm') :: accum))
		   | _ => getWorking (strm', n-1, (tok, s)::accum)
	      end

      fun skip (strm, 0) = strm
	| skip (strm, n) = skip (#3 (WS.get1 strm), n - 1)
      fun isEmpty strm = (case (#1 (WS.get1 strm))
			   of Tok.EOF => true
			    | _ => false)

      fun involvesKW (r, t) = (case r
            of Insertion t' => Tok.isKW t'
	     | Deletion => Tok.isKW t
	     | Substitution t' => Tok.isKW t orelse Tok.isKW t'
           (* end case *))

      infix >>
      fun (Insertion _) >> _ = true
	| Deletion >> _ = true
	| _ >> _ = false

      fun chooseCand (c1, c2) = let
	    val (r1, _, _, score1, kw1) = c1
	    val (r2, _, _, score2, kw2) = c2
            in if score1 > score2 then c1
	       else if score2 > score1 then c2
	       else if r1 >> r2 then c1
	       else if r2 >> r1 then c2
	       else if kw1 = false then c1
	       else c2 
            end

      fun chooseRepair {startAt, endAt, try} = let
	    val scoreOffset = WS.subtract (endAt, startAt)
	    val (endAt', working) = getWorking (startAt, scoreOffset + 5, [])
	    fun tryRepairs (prefix, working, repairs, best) = (case (working, repairs)
	      of ([], _) => (case best
			      of SOME (r, prefixLen, strm, _, _) => (*
print (case r
	of Deletion => "DEL\n"
	 | Insertion t => "INS " ^ Tok.toString t ^ "\n"
	 | Substitution t => "SUB " ^ Tok.toString t ^ "\n"); *)
				   SOME {
				     errorAt = skip (startAt, prefixLen),
				     repair = r,
				     repaired = strm
			           }
			       | NONE => NONE
			     (* end case *))
	       | (t::ts, []) => 
		   tryRepairs (prefix @ [t], ts, allRepairs, best)
	       | (_, r::rs) => let
		   val strm = WS.prepend (prefix @ (applyRepair (working, r)), endAt')
		   val strm' = try strm
 		   val score = WS.subtract (strm', strm)
			         + (case r
				     of Deletion => 1
				      | Insertion _ => ~1
				      | Substitution _ => 0)
			         - scoreOffset
		   val kw = involvesKW (r, #1 (hd working))
		   val cand = (r, List.length prefix, strm, score, kw)
		   val valid = if kw
			       then score > minAdvance + 2
			       else score > minAdvance 
		   val best' = if valid then 
				 case best
				  of NONE => SOME cand
				   | SOME c => SOME (chooseCand (c, cand))
			       else best
		   in
		     tryRepairs (prefix, working, rs, best')
		   end
             (* end case *))
            in
	      tryRepairs ([], working, allRepairs, NONE)
	    end
	    

    end (* structure RepairableStrm *)

    functor ErrHandlerFn(R : REPAIRABLE) : sig

      type err_handler
      val mkErrHandler : unit -> err_handler
      val getAnns	: err_handler -> UserCode.antlr_annotation list
      val setAnns	: err_handler * UserCode.antlr_annotation list -> unit
      val addAnnotation : err_handler -> UserCode.antlr_annotation -> unit
      val whileDisabled : err_handler -> (unit -> 'a) -> 'a

      datatype repair 
	= Primary of {
	    errorAt : R.T,
	    repair : R.repair
          }
	| Secondary of {
	    deleteFrom : R.T,
	    deleteTo : R.T
	  }

(*      val wrap   : err_handler -> (R.T -> ('a * R.T)) -> R.T -> ('a * R.T) *)
      val wrap   : err_handler -> (R.T -> 'a) -> R.T -> 'a
      val launch : err_handler -> (R.T -> ('a * 'b * R.T)) -> 
		   R.T -> ('a * R.T * repair list * UserCode.antlr_annotation list)

    end = struct

      type repair_cont = R.T option SMLofNJ.Cont.cont 
      type retry_cont  = R.T        SMLofNJ.Cont.cont

      exception JumpOut of (R.T * retry_cont) list

      datatype repair 
	= Primary of {
	    errorAt : R.T,
	    repair : R.repair
          }
	| Secondary of {
	    deleteFrom : R.T,
	    deleteTo : R.T
	  }

      datatype err_handler = EH of {
	cont : repair_cont option ref,
	enabled : bool ref,
	repairs : repair list ref,
	annotations : UserCode.antlr_annotation list ref
      }

      fun getCont    (EH {cont,    ...}) = !cont
      fun getEnabled (EH {enabled, ...}) = !enabled
      fun getRepairs (EH {repairs, ...}) = !repairs
      fun getAnns    (EH {annotations, ...}) = !annotations

      fun setCont    (EH {cont,    ...}, n) = cont := n
      fun setEnabled (EH {enabled, ...}, n) = enabled := n
      fun addRepair  (EH {repairs, ...}, n) = repairs := (!repairs) @ [n]
      fun setAnns    (EH {annotations, ...}, n) = annotations := n

      fun mkErrHandler () = EH {cont = ref NONE, 
				enabled = ref true,
				repairs = ref [],
			        annotations = ref []}

      fun addAnnotation eh a = setAnns (eh, a :: (getAnns eh))

      fun whileDisabled eh f = let
	    val oldEnabled = getEnabled eh
            in
	      setEnabled (eh, false);
	      f () before setEnabled (eh, oldEnabled)
            end

      fun throwIfEH (eh, t) = 
	    if getEnabled eh then 
	      Option.app (fn k => SMLofNJ.Cont.throw k (SOME t)) (getCont eh)
	    else ()

      fun wrap eh f t = if getEnabled eh then let
	    val cont_ref : retry_cont option ref = ref NONE
	    val anns = getAnns eh
	    val t' = SMLofNJ.Cont.callcc (fn k => (cont_ref := SOME k; t))
	    val retry = (t', valOf (!cont_ref))
            in
	      setAnns (eh, anns);
	      f t'
	      handle R.RepairableError => (
		       throwIfEH (eh, t');
		       raise JumpOut [retry])
		   | JumpOut stack => (
		       throwIfEH (eh, t');
		       raise JumpOut (retry::stack))
            end
          else f t

      fun findWindow (stack) = let
	    val revStack = rev stack
	    val rightMost = hd revStack
	    fun TOf (t, _) = t
	    fun find [] = raise (Fail "BUG: findWindow given an empty stack")
	      | find [top] = (top, rightMost)
	      | find (top::stack) = 
		  if R.farEnoughWindow {startAt = TOf top, endAt = TOf rightMost}
		  then (top, rightMost)
		  else find stack
            in
	      find revStack
            end

      fun tryRepair (eh, cont) t = 
	    (case SMLofNJ.Cont.callcc (fn k => (setCont (eh, SOME k); NONE))
	      of NONE => 
		   (* first time through, try the repair *)
		   SMLofNJ.Cont.throw cont t		
	       | SOME t' => 
		   (* second time through, return the new right-most T *)
		   (setCont (eh, NONE); t')
	     (* end case *))

      fun primaryRepair (eh, stack) = let
	    val ((leftT, leftCont), (rightT, rightCont)) = 
		  findWindow stack
	    val repair = R.chooseRepair {
			     startAt = leftT,
			     endAt = rightT,
			     try = tryRepair (eh, leftCont)
			   }
            in case repair
		of SOME {errorAt, repair, repaired} => 
		     SOME (Primary {errorAt = errorAt, repair = repair}, 
			   leftCont, repaired)
		 | NONE => NONE
            end

      fun secondaryRepair (eh, revStack) = let
	    val stack = rev revStack
	    val (errStrm, errCont) = hd stack
	    fun try ((strm, cont), strm', next) = let
	          val strm'' = tryRepair (eh, cont) strm'
	          in
	            if R.farEnoughRepair {startAt = strm', endAt = strm''}
		    then SOME (Secondary {
			   deleteFrom = strm,
			   deleteTo = strm'
			 }, cont, strm')
		    else next()
	          end
	    fun rightRepair (strm, n) = 
		  if n = 0 then NONE
		  else let 
		    val strm' = R.skip (strm, 1)
		    in 
		      try (hd stack, strm', fn () => rightRepair (strm', n-1))
		    end
	    fun leftRightRepair (strm, []) = 
		  if R.isEmpty strm then
		    raise Fail "Unrecoverable parse error"
		  else leftRightRepair (R.skip (strm, 1), stack)
	      | leftRightRepair (strm, top::stack) = 
		  try (top, strm, fn () => leftRightRepair (strm, stack))
            in
	      case rightRepair (errStrm, 5)
	       of SOME r => r
		| _      => valOf (leftRightRepair (errStrm, []))
            end

      fun repair (eh, stack) = (case primaryRepair (eh, stack)
	    of SOME r => r
	     | NONE => secondaryRepair (eh, stack)
           (* end case *))

      fun launch eh f t = let
	    val (x, _, t') = wrap eh f t 
		handle JumpOut stack => let
		  val (r, cont, t') = repair (eh, stack)
		  in
		    addRepair (eh, r);
		    SMLofNJ.Cont.throw cont t'
		  end
            in
	      throwIfEH (eh, t');
	      (x, t', getRepairs eh, getAnns eh)
            end

    end (* functor ErrHandlerFn *)

    structure Err = ErrHandlerFn(RepairableStrm)

    fun pretryProds eh prods strm = let
	fun try [] = raise RepairableStrm.RepairableError
	  | try (prod :: prods) = let 
	      val anns = Err.getAnns eh
	      in
	        Err.whileDisabled eh (fn () => prod strm)
		handle _ => 
		  (Err.setAnns (eh, anns);
		   try (prods))
	      end
        in
          try prods
        end

    exception ParseError = RepairableStrm.RepairableError

    datatype repair_action
      = Insert of Tok.token list
      | Delete of Tok.token list
      | Subst of {
	    old : Tok.token list, 
	    new : Tok.token list
	}

    structure R = RepairableStrm

    fun unwrapErr (Err.Primary {errorAt, repair = R.Deletion}) =
          (WStream.getPos errorAt, Delete [(#1 (WStream.get1 errorAt))])
      | unwrapErr (Err.Primary {errorAt, repair = R.Insertion t}) =
          (WStream.getPos errorAt, Insert [t])
      | unwrapErr (Err.Primary {errorAt, repair = R.Substitution t}) = 
          (WStream.getPos errorAt, 
  	   Subst {
  	     old = [(#1 (WStream.get1 errorAt))],
  	     new = [t]
           })
      | unwrapErr (Err.Secondary {deleteFrom, deleteTo}) = 
          (WStream.getPos deleteFrom, 
  	   Delete (map #1 (WStream.getDiff (deleteTo, deleteFrom))))

    fun toksToString toks = String.concatWith " " (map Tok.toString toks)

    fun repairToString repair = (case repair
          of Insert toks => "inserting " ^ toksToString toks
	   | Delete toks => "deleting " ^ toksToString toks
	   | Subst {old, new} => 
	       "substituting " ^ toksToString new ^ " for "
	       ^ toksToString old
         (* end case *))

    fun mk () = let
        val eh = Err.mkErrHandler()
	fun wrap f = Err.wrap eh f
	val whileDisabled = Err.whileDisabled eh
	val addAnnotation = Err.addAnnotation eh
	fun tryProds (strm, prods) = 
	      (wrap (pretryProds eh prods)) strm
	fun unwrap (ret, strm, errors, anns) = (ret, WStream.unwrap strm, map unwrapErr errors, anns)
	val lex = WStream.get1
val matchEOF = wrap (fn strm => (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchMULOP = wrap (fn strm => (case (lex(strm))
 of (Tok.MULOP(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchADDOP = wrap (fn strm => (case (lex(strm))
 of (Tok.ADDOP(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchRELOP = wrap (fn strm => (case (lex(strm))
 of (Tok.RELOP(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchMINUS = wrap (fn strm => (case (lex(strm))
 of (Tok.MINUS, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchRP = wrap (fn strm => (case (lex(strm))
 of (Tok.RP, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchLP = wrap (fn strm => (case (lex(strm))
 of (Tok.LP, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchRSB = wrap (fn strm => (case (lex(strm))
 of (Tok.RSB, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchLSB = wrap (fn strm => (case (lex(strm))
 of (Tok.LSB, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchDOT = wrap (fn strm => (case (lex(strm))
 of (Tok.DOT, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchSEMI = wrap (fn strm => (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchCOLON = wrap (fn strm => (case (lex(strm))
 of (Tok.COLON, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchCOMMA = wrap (fn strm => (case (lex(strm))
 of (Tok.COMMA, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchASSIGNOP = wrap (fn strm => (case (lex(strm))
 of (Tok.ASSIGNOP, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchREAL = wrap (fn strm => (case (lex(strm))
 of (Tok.REAL(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchINT = wrap (fn strm => (case (lex(strm))
 of (Tok.INT(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchID = wrap (fn strm => (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_not = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_not, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_do = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_do, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_while = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_while, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_else = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_else, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_then = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_then, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_if = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_if, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_end = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_end, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_begin = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_begin, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_procedure = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_procedure, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_function = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_function, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_real = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_real, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_integer = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_integer, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_of = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_of, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_array = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_array, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_var = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_var, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_program = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_program, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))

val (program_NT) = 
let
fun exp_NT (strm) = let
      val (simple_exp_RES, simple_exp_SPAN, strm') = (wrap simple_exp_NT)(strm)
      fun SR1_NT (strm) = let
            val (RELOP_RES, RELOP_SPAN, strm') = matchRELOP(strm)
            val (simple_exp_RES, simple_exp_SPAN, strm') = (wrap simple_exp_NT)(strm')
            val FULL_SPAN = (#1(RELOP_SPAN), #2(simple_exp_SPAN))
            in
              (UserCode.exp_PROD_1_exp_SR1_PROD_1_ACT (simple_exp_RES, RELOP_RES, simple_exp_SPAN : Lex.span, RELOP_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.RELOP(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(SR1_PRED, (wrap SR1_NT), strm')
      val FULL_SPAN = (#1(simple_exp_SPAN), #2(SR_SPAN))
      in
        (UserCode.exp_PROD_1_ACT (SR_RES, simple_exp_RES, SR_SPAN : Lex.span, simple_exp_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
and simple_exp_NT (strm) = let
      val (signed_term_RES, signed_term_SPAN, strm') = (wrap signed_term_NT)(strm)
      fun SR1_NT (strm) = let
            val (ADDOP_RES, ADDOP_SPAN, strm') = matchADDOP(strm)
            val (signed_term_RES, signed_term_SPAN, strm') = (wrap signed_term_NT)(strm')
            val FULL_SPAN = (#1(ADDOP_SPAN), #2(signed_term_SPAN))
            in
              (UserCode.simple_exp_PROD_1_simple_exp_SR1_PROD_1_ACT (signed_term_RES, ADDOP_RES, signed_term_SPAN : Lex.span, ADDOP_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.ADDOP(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
      val FULL_SPAN = (#1(signed_term_SPAN), #2(SR_SPAN))
      in
        (UserCode.simple_exp_PROD_1_ACT (SR_RES, signed_term_RES, SR_SPAN : Lex.span, signed_term_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
and signed_term_NT (strm) = let
      fun signed_term_PROD_1 (strm) = let
            val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm)
            val (term_RES, term_SPAN, strm') = (wrap term_NT)(strm')
            val FULL_SPAN = (#1(MINUS_SPAN), #2(term_SPAN))
            in
              (UserCode.signed_term_PROD_1_ACT (term_RES, MINUS_RES, term_SPAN : Lex.span, MINUS_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun signed_term_PROD_2 (strm) = let
            val (term_RES, term_SPAN, strm') = (wrap term_NT)(strm)
            val FULL_SPAN = (#1(term_SPAN), #2(term_SPAN))
            in
              (term_RES, FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => signed_term_PROD_2(strm)
          | (Tok.REAL(_), _, strm') => signed_term_PROD_2(strm)
          | (Tok.INT(_), _, strm') => signed_term_PROD_2(strm)
          | (Tok.ID(_), _, strm') => signed_term_PROD_2(strm)
          | (Tok.KW_not, _, strm') => signed_term_PROD_2(strm)
          | (Tok.MINUS, _, strm') => signed_term_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
and term_NT (strm) = let
      val (factor_RES, factor_SPAN, strm') = (wrap factor_NT)(strm)
      fun SR1_NT (strm) = let
            val (MULOP_RES, MULOP_SPAN, strm') = matchMULOP(strm)
            val (factor_RES, factor_SPAN, strm') = (wrap factor_NT)(strm')
            val FULL_SPAN = (#1(MULOP_SPAN), #2(factor_SPAN))
            in
              (UserCode.term_PROD_1_term_SR1_PROD_1_ACT (factor_RES, MULOP_RES, factor_SPAN : Lex.span, MULOP_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.MULOP(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
      val FULL_SPAN = (#1(factor_SPAN), #2(SR_SPAN))
      in
        (UserCode.term_PROD_1_ACT (SR_RES, factor_RES, SR_SPAN : Lex.span, factor_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
and factor_NT (strm) = let
      fun factor_PROD_1 (strm) = let
            val (variable_RES, variable_SPAN, strm') = (wrap variable_NT)(strm)
            val FULL_SPAN = (#1(variable_SPAN), #2(variable_SPAN))
            in
              (variable_RES, FULL_SPAN, strm')
            end
      fun factor_PROD_2 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (exp_RES, exp_SPAN, strm') = (wrap exp_NT)(strm')
            fun SR1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (exp_RES, exp_SPAN, strm') = (wrap exp_NT)(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(exp_SPAN))
                  in
                    (exp_RES, FULL_SPAN, strm')
                  end
            fun SR1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(ID_SPAN), #2(RP_SPAN))
            in
              (UserCode.factor_PROD_2_ACT (ID_RES, LP_RES, RP_RES, SR_RES, exp_RES, ID_SPAN : Lex.span, LP_SPAN : Lex.span, RP_SPAN : Lex.span, SR_SPAN : Lex.span, exp_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun factor_PROD_3 (strm) = let
            val (INT_RES, INT_SPAN, strm') = matchINT(strm)
            val FULL_SPAN = (#1(INT_SPAN), #2(INT_SPAN))
            in
              (UserCode.factor_PROD_3_ACT (INT_RES, INT_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun factor_PROD_4 (strm) = let
            val (REAL_RES, REAL_SPAN, strm') = matchREAL(strm)
            val FULL_SPAN = (#1(REAL_SPAN), #2(REAL_SPAN))
            in
              (UserCode.factor_PROD_4_ACT (REAL_RES, REAL_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun factor_PROD_5 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (exp_RES, exp_SPAN, strm') = (wrap exp_NT)(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.factor_PROD_5_ACT (LP_RES, RP_RES, exp_RES, LP_SPAN : Lex.span, RP_SPAN : Lex.span, exp_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun factor_PROD_6 (strm) = let
            val (KW_not_RES, KW_not_SPAN, strm') = matchKW_not(strm)
            val (factor_RES, factor_SPAN, strm') = (wrap factor_NT)(strm')
            val FULL_SPAN = (#1(KW_not_SPAN), #2(factor_SPAN))
            in
              (UserCode.factor_PROD_6_ACT (factor_RES, KW_not_RES, factor_SPAN : Lex.span, KW_not_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_not, _, strm') => factor_PROD_6(strm)
          | (Tok.REAL(_), _, strm') => factor_PROD_4(strm)
          | (Tok.ID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.MULOP(_), _, strm') => factor_PROD_1(strm)
                | (Tok.ADDOP(_), _, strm') => factor_PROD_1(strm)
                | (Tok.RELOP(_), _, strm') => factor_PROD_1(strm)
                | (Tok.RP, _, strm') => factor_PROD_1(strm)
                | (Tok.RSB, _, strm') => factor_PROD_1(strm)
                | (Tok.LSB, _, strm') => factor_PROD_1(strm)
                | (Tok.SEMI, _, strm') => factor_PROD_1(strm)
                | (Tok.COMMA, _, strm') => factor_PROD_1(strm)
                | (Tok.KW_do, _, strm') => factor_PROD_1(strm)
                | (Tok.KW_else, _, strm') => factor_PROD_1(strm)
                | (Tok.KW_then, _, strm') => factor_PROD_1(strm)
                | (Tok.KW_end, _, strm') => factor_PROD_1(strm)
                | (Tok.LP, _, strm') => factor_PROD_2(strm)
                | _ => raise(ParseError)
              (* end case *))
          | (Tok.INT(_), _, strm') => factor_PROD_3(strm)
          | (Tok.LP, _, strm') => factor_PROD_5(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
and variable_NT (strm) = let
      fun variable_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (ID_RES, FULL_SPAN, strm')
            end
      fun variable_PROD_2 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val (LSB_RES, LSB_SPAN, strm') = matchLSB(strm')
            val (exp_RES, exp_SPAN, strm') = (wrap exp_NT)(strm')
            val (RSB_RES, RSB_SPAN, strm') = matchRSB(strm')
            val FULL_SPAN = (#1(ID_SPAN), #2(RSB_SPAN))
            in
              (UserCode.variable_PROD_2_ACT (ID_RES, LSB_RES, RSB_RES, exp_RES, ID_SPAN : Lex.span, LSB_SPAN : Lex.span, RSB_SPAN : Lex.span, exp_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.MULOP(_), _, strm') => variable_PROD_1(strm)
                | (Tok.ADDOP(_), _, strm') => variable_PROD_1(strm)
                | (Tok.RELOP(_), _, strm') => variable_PROD_1(strm)
                | (Tok.RP, _, strm') => variable_PROD_1(strm)
                | (Tok.RSB, _, strm') => variable_PROD_1(strm)
                | (Tok.SEMI, _, strm') => variable_PROD_1(strm)
                | (Tok.COMMA, _, strm') => variable_PROD_1(strm)
                | (Tok.ASSIGNOP, _, strm') => variable_PROD_1(strm)
                | (Tok.KW_do, _, strm') => variable_PROD_1(strm)
                | (Tok.KW_else, _, strm') => variable_PROD_1(strm)
                | (Tok.KW_then, _, strm') => variable_PROD_1(strm)
                | (Tok.KW_end, _, strm') => variable_PROD_1(strm)
                | (Tok.LSB, _, strm') => variable_PROD_2(strm)
                | _ => raise(ParseError)
              (* end case *))
          | _ => raise(ParseError)
        (* end case *))
      end
fun procedure_statement_NT (strm) = let
      fun procedure_statement_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (ID_RES, FULL_SPAN, strm')
            end
      fun procedure_statement_PROD_2 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (exp_RES, exp_SPAN, strm') = (wrap exp_NT)(strm')
            fun SR1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (exp_RES, exp_SPAN, strm') = (wrap exp_NT)(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(exp_SPAN))
                  in
                    (exp_RES, FULL_SPAN, strm')
                  end
            fun SR1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(ID_SPAN), #2(RP_SPAN))
            in
              (UserCode.procedure_statement_PROD_2_ACT (ID_RES, LP_RES, RP_RES, SR_RES, exp_RES, ID_SPAN : Lex.span, LP_SPAN : Lex.span, RP_SPAN : Lex.span, SR_SPAN : Lex.span, exp_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.SEMI, _, strm') => procedure_statement_PROD_1(strm)
                | (Tok.KW_else, _, strm') => procedure_statement_PROD_1(strm)
                | (Tok.KW_end, _, strm') => procedure_statement_PROD_1(strm)
                | (Tok.LP, _, strm') => procedure_statement_PROD_2(strm)
                | _ => raise(ParseError)
              (* end case *))
          | _ => raise(ParseError)
        (* end case *))
      end
fun compound_statement_NT (strm) = let
      val (KW_begin_RES, KW_begin_SPAN, strm') = matchKW_begin(strm)
      fun SR1_NT (strm) = let
            val (statement_RES, statement_SPAN, strm') = (wrap statement_NT)(strm)
            fun SR1_NT (strm) = let
                  val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
                  val (statement_RES, statement_SPAN, strm') = (wrap statement_NT)(strm')
                  val FULL_SPAN = (#1(SEMI_SPAN), #2(statement_SPAN))
                  in
                    (statement_RES, FULL_SPAN, strm')
                  end
            fun SR1_PRED (strm) = (case (lex(strm))
                   of (Tok.SEMI, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
            val FULL_SPAN = (#1(statement_SPAN), #2(SR_SPAN))
            in
              (UserCode.compound_statement_PROD_1_compound_statement_SR1_PROD_1_ACT (SR_RES, KW_begin_RES, statement_RES, SR_SPAN : Lex.span, KW_begin_SPAN : Lex.span, statement_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.ID(_), _, strm') => true
              | (Tok.KW_while, _, strm') => true
              | (Tok.KW_if, _, strm') => true
              | (Tok.KW_begin, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(SR1_PRED, (wrap SR1_NT), strm')
      val (KW_end_RES, KW_end_SPAN, strm') = matchKW_end(strm')
      val FULL_SPAN = (#1(KW_begin_SPAN), #2(KW_end_SPAN))
      in
        (UserCode.compound_statement_PROD_1_ACT (SR_RES, KW_begin_RES, KW_end_RES, SR_SPAN : Lex.span, KW_begin_SPAN : Lex.span, KW_end_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
and statement_NT (strm) = let
      fun statement_PROD_1 (strm) = let
            val (variable_RES, variable_SPAN, strm') = (wrap variable_NT)(strm)
            val (ASSIGNOP_RES, ASSIGNOP_SPAN, strm') = matchASSIGNOP(strm')
            val (exp_RES, exp_SPAN, strm') = (wrap exp_NT)(strm')
            val FULL_SPAN = (#1(variable_SPAN), #2(exp_SPAN))
            in
              (UserCode.statement_PROD_1_ACT (exp_RES, variable_RES, ASSIGNOP_RES, exp_SPAN : Lex.span, variable_SPAN : Lex.span, ASSIGNOP_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun statement_PROD_2 (strm) = let
            val (procedure_statement_RES, procedure_statement_SPAN, strm') = (wrap procedure_statement_NT)(strm)
            val FULL_SPAN = (#1(procedure_statement_SPAN),
              #2(procedure_statement_SPAN))
            in
              (procedure_statement_RES, FULL_SPAN, strm')
            end
      fun statement_PROD_3 (strm) = let
            val (compound_statement_RES, compound_statement_SPAN, strm') = (wrap compound_statement_NT)(strm)
            val FULL_SPAN = (#1(compound_statement_SPAN),
              #2(compound_statement_SPAN))
            in
              (compound_statement_RES, FULL_SPAN, strm')
            end
      fun statement_PROD_4 (strm) = let
            val (KW_if_RES, KW_if_SPAN, strm') = matchKW_if(strm)
            val (exp_RES, exp_SPAN, strm') = (wrap exp_NT)(strm')
            val (KW_then_RES, KW_then_SPAN, strm') = matchKW_then(strm')
            val (statement1_RES, statement1_SPAN, strm') = (wrap statement_NT)(strm')
            val (KW_else_RES, KW_else_SPAN, strm') = matchKW_else(strm')
            val (statement2_RES, statement2_SPAN, strm') = (wrap statement_NT)(strm')
            val FULL_SPAN = (#1(KW_if_SPAN), #2(statement2_SPAN))
            in
              (UserCode.statement_PROD_4_ACT (exp_RES, KW_else_RES, KW_then_RES, statement1_RES, statement2_RES, KW_if_RES, exp_SPAN : Lex.span, KW_else_SPAN : Lex.span, KW_then_SPAN : Lex.span, statement1_SPAN : Lex.span, statement2_SPAN : Lex.span, KW_if_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun statement_PROD_5 (strm) = let
            val (KW_while_RES, KW_while_SPAN, strm') = matchKW_while(strm)
            val (exp_RES, exp_SPAN, strm') = (wrap exp_NT)(strm')
            val (KW_do_RES, KW_do_SPAN, strm') = matchKW_do(strm')
            val (statement_RES, statement_SPAN, strm') = (wrap statement_NT)(strm')
            val FULL_SPAN = (#1(KW_while_SPAN), #2(statement_SPAN))
            in
              (UserCode.statement_PROD_5_ACT (exp_RES, KW_while_RES, KW_do_RES, statement_RES, exp_SPAN : Lex.span, KW_while_SPAN : Lex.span, KW_do_SPAN : Lex.span, statement_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_while, _, strm') => statement_PROD_5(strm)
          | (Tok.KW_begin, _, strm') => statement_PROD_3(strm)
          | (Tok.ID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.LSB, _, strm') => statement_PROD_1(strm)
                | (Tok.ASSIGNOP, _, strm') => statement_PROD_1(strm)
                | (Tok.LP, _, strm') => statement_PROD_2(strm)
                | (Tok.SEMI, _, strm') => statement_PROD_2(strm)
                | (Tok.KW_else, _, strm') => statement_PROD_2(strm)
                | (Tok.KW_end, _, strm') => statement_PROD_2(strm)
                | _ => raise(ParseError)
              (* end case *))
          | (Tok.KW_if, _, strm') => statement_PROD_4(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun standard_type_NT (strm) = let
      fun standard_type_PROD_1 (strm) = let
            val (KW_integer_RES, KW_integer_SPAN, strm') = matchKW_integer(strm)
            val FULL_SPAN = (#1(KW_integer_SPAN), #2(KW_integer_SPAN))
            in
              (UserCode.standard_type_PROD_1_ACT (KW_integer_RES, KW_integer_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun standard_type_PROD_2 (strm) = let
            val (KW_real_RES, KW_real_SPAN, strm') = matchKW_real(strm)
            val FULL_SPAN = (#1(KW_real_SPAN), #2(KW_real_SPAN))
            in
              (UserCode.standard_type_PROD_2_ACT (KW_real_RES, KW_real_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_real, _, strm') => standard_type_PROD_2(strm)
          | (Tok.KW_integer, _, strm') => standard_type_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun compound_type_NT (strm) = let
      fun compound_type_PROD_1 (strm) = let
            val (standard_type_RES, standard_type_SPAN, strm') = (wrap standard_type_NT)(strm)
            val FULL_SPAN = (#1(standard_type_SPAN), #2(standard_type_SPAN))
            in
              (standard_type_RES, FULL_SPAN, strm')
            end
      fun compound_type_PROD_2 (strm) = let
            val (KW_array_RES, KW_array_SPAN, strm') = matchKW_array(strm)
            val (LSB_RES, LSB_SPAN, strm') = matchLSB(strm')
            val (INT1_RES, INT1_SPAN, strm') = matchINT(strm')
            val (DOT1_RES, DOT1_SPAN, strm') = matchDOT(strm')
            val (DOT2_RES, DOT2_SPAN, strm') = matchDOT(strm')
            val (INT2_RES, INT2_SPAN, strm') = matchINT(strm')
            val (RSB_RES, RSB_SPAN, strm') = matchRSB(strm')
            val (KW_of_RES, KW_of_SPAN, strm') = matchKW_of(strm')
            val (standard_type_RES, standard_type_SPAN, strm') = (wrap standard_type_NT)(strm')
            val FULL_SPAN = (#1(KW_array_SPAN), #2(standard_type_SPAN))
            in
              (UserCode.compound_type_PROD_2_ACT (LSB_RES, RSB_RES, DOT1_RES, DOT2_RES, INT1_RES, INT2_RES, KW_array_RES, standard_type_RES, KW_of_RES, LSB_SPAN : Lex.span, RSB_SPAN : Lex.span, DOT1_SPAN : Lex.span, DOT2_SPAN : Lex.span, INT1_SPAN : Lex.span, INT2_SPAN : Lex.span, KW_array_SPAN : Lex.span, standard_type_SPAN : Lex.span, KW_of_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_array, _, strm') => compound_type_PROD_2(strm)
          | (Tok.KW_real, _, strm') => compound_type_PROD_1(strm)
          | (Tok.KW_integer, _, strm') => compound_type_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun id_list_NT (strm) = let
      val (ID_RES, ID_SPAN, strm') = matchID(strm)
      fun SR1_NT (strm) = let
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val FULL_SPAN = (#1(COMMA_SPAN), #2(ID_SPAN))
            in
              (ID_RES, FULL_SPAN, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.COMMA, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
      val FULL_SPAN = (#1(ID_SPAN), #2(SR_SPAN))
      in
        (UserCode.id_list_PROD_1_ACT (ID_RES, SR_RES, ID_SPAN : Lex.span, SR_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
fun id_list_type_NT (strm) = let
      val (id_list_RES, id_list_SPAN, strm') = (wrap id_list_NT)(strm)
      val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm')
      val (compound_type_RES, compound_type_SPAN, strm') = (wrap compound_type_NT)(strm')
      val FULL_SPAN = (#1(id_list_SPAN), #2(compound_type_SPAN))
      in
        (UserCode.id_list_type_PROD_1_ACT (compound_type_RES, COLON_RES, id_list_RES, compound_type_SPAN : Lex.span, COLON_SPAN : Lex.span, id_list_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
fun declaration_NT (strm) = let
      val (KW_var_RES, KW_var_SPAN, strm') = matchKW_var(strm)
      val (id_list_type_RES, id_list_type_SPAN, strm') = (wrap id_list_type_NT)(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val FULL_SPAN = (#1(KW_var_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.declaration_PROD_1_ACT (SEMI_RES, id_list_type_RES, KW_var_RES, SEMI_SPAN : Lex.span, id_list_type_SPAN : Lex.span, KW_var_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
fun parameter_list_NT (strm) = let
      val (id_list_type_RES, id_list_type_SPAN, strm') = (wrap id_list_type_NT)(strm)
      fun SR1_NT (strm) = let
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
            val (id_list_type_RES, id_list_type_SPAN, strm') = (wrap id_list_type_NT)(strm')
            val FULL_SPAN = (#1(SEMI_SPAN), #2(id_list_type_SPAN))
            in
              (id_list_type_RES, FULL_SPAN, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.SEMI, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
      val FULL_SPAN = (#1(id_list_type_SPAN), #2(SR_SPAN))
      in
        (UserCode.parameter_list_PROD_1_ACT (SR_RES, id_list_type_RES, SR_SPAN : Lex.span, id_list_type_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
fun arguments_NT (strm) = let
      fun arguments_PROD_1 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (parameter_list_RES, parameter_list_SPAN, strm') = (wrap parameter_list_NT)(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.arguments_PROD_1_ACT (LP_RES, RP_RES, parameter_list_RES, LP_SPAN : Lex.span, RP_SPAN : Lex.span, parameter_list_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun arguments_PROD_2 (strm) = let
            val FULL_SPAN = (WStream.getPos(strm), WStream.getPos(strm))
            in
              (UserCode.arguments_PROD_2_ACT (addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm)
            end
      in
        (case (lex(strm))
         of (Tok.SEMI, _, strm') => arguments_PROD_2(strm)
          | (Tok.COLON, _, strm') => arguments_PROD_2(strm)
          | (Tok.LP, _, strm') => arguments_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun subprogram_head_NT (strm) = let
      fun subprogram_head_PROD_1 (strm) = let
            val (KW_function_RES, KW_function_SPAN, strm') = matchKW_function(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (arguments_RES, arguments_SPAN, strm') = (wrap arguments_NT)(strm')
            val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm')
            val (standard_type_RES, standard_type_SPAN, strm') = (wrap standard_type_NT)(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_function_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.subprogram_head_PROD_1_ACT (ID_RES, SEMI_RES, standard_type_RES, COLON_RES, arguments_RES, KW_function_RES, ID_SPAN : Lex.span, SEMI_SPAN : Lex.span, standard_type_SPAN : Lex.span, COLON_SPAN : Lex.span, arguments_SPAN : Lex.span, KW_function_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      fun subprogram_head_PROD_2 (strm) = let
            val (KW_procedure_RES, KW_procedure_SPAN, strm') = matchKW_procedure(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (arguments_RES, arguments_SPAN, strm') = (wrap arguments_NT)(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_procedure_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.subprogram_head_PROD_2_ACT (ID_RES, SEMI_RES, KW_procedure_RES, arguments_RES, ID_SPAN : Lex.span, SEMI_SPAN : Lex.span, KW_procedure_SPAN : Lex.span, arguments_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_procedure, _, strm') => subprogram_head_PROD_2(strm)
          | (Tok.KW_function, _, strm') => subprogram_head_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun subprogram_declaration_NT (strm) = let
      val (subprogram_head_RES, subprogram_head_SPAN, strm') = (wrap subprogram_head_NT)(strm)
      fun SR1_NT (strm) = let
            val (declaration_RES, declaration_SPAN, strm') = (wrap declaration_NT)(strm)
            val FULL_SPAN = (#1(declaration_SPAN), #2(declaration_SPAN))
            in
              (declaration_RES, FULL_SPAN, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_var, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
      val (compound_statement_RES, compound_statement_SPAN, strm') = (wrap compound_statement_NT)(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val FULL_SPAN = (#1(subprogram_head_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.subprogram_declaration_PROD_1_ACT (SR_RES, SEMI_RES, compound_statement_RES, subprogram_head_RES, SR_SPAN : Lex.span, SEMI_SPAN : Lex.span, compound_statement_SPAN : Lex.span, subprogram_head_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
fun program_NT (strm) = let
      val (KW_program_RES, KW_program_SPAN, strm') = matchKW_program(strm)
      val (ID_RES, ID_SPAN, strm') = matchID(strm')
      val (LP_RES, LP_SPAN, strm') = matchLP(strm')
      val (id_list_RES, id_list_SPAN, strm') = (wrap id_list_NT)(strm')
      val (RP_RES, RP_SPAN, strm') = matchRP(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      fun SR1_NT (strm) = let
            val (declaration_RES, declaration_SPAN, strm') = (wrap declaration_NT)(strm)
            val FULL_SPAN = (#1(declaration_SPAN), #2(declaration_SPAN))
            in
              (declaration_RES, FULL_SPAN, strm')
            end
      fun SR1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_var, _, strm') => true
              | _ => false
            (* end case *))
      val (SR1_RES, SR1_SPAN, strm') = EBNF.closure(SR1_PRED, (wrap SR1_NT), strm')
      fun SR2_NT (strm) = let
            val (subprogram_declaration_RES, subprogram_declaration_SPAN, strm') = (wrap subprogram_declaration_NT)(strm)
            val FULL_SPAN = (#1(subprogram_declaration_SPAN),
              #2(subprogram_declaration_SPAN))
            in
              (subprogram_declaration_RES, FULL_SPAN, strm')
            end
      fun SR2_PRED (strm) = (case (lex(strm))
             of (Tok.KW_procedure, _, strm') => true
              | (Tok.KW_function, _, strm') => true
              | _ => false
            (* end case *))
      val (SR2_RES, SR2_SPAN, strm') = EBNF.closure(SR2_PRED, (wrap SR2_NT), strm')
      val (compound_statement_RES, compound_statement_SPAN, strm') = (wrap compound_statement_NT)(strm')
      val (DOT_RES, DOT_SPAN, strm') = matchDOT(strm')
      val (EOF_RES, EOF_SPAN, strm') = matchEOF(strm')
      val FULL_SPAN = (#1(KW_program_SPAN), #2(EOF_SPAN))
      in
        (UserCode.program_PROD_1_ACT (ID_RES, LP_RES, RP_RES, DOT_RES, EOF_RES, SR1_RES, SR2_RES, SEMI_RES, compound_statement_RES, id_list_RES, KW_program_RES, ID_SPAN : Lex.span, LP_SPAN : Lex.span, RP_SPAN : Lex.span, DOT_SPAN : Lex.span, EOF_SPAN : Lex.span, SR1_SPAN : Lex.span, SR2_SPAN : Lex.span, SEMI_SPAN : Lex.span, compound_statement_SPAN : Lex.span, id_list_SPAN : Lex.span, KW_program_SPAN : Lex.span, addAnnotation, FULL_SPAN : Lex.span),
          FULL_SPAN, strm')
      end
in
  (program_NT)
end
val program_NT =  fn s => unwrap (Err.launch eh (program_NT ) (WStream.wrap s))

in (program_NT) end

fun parse s = let val (program_NT) = mk() in program_NT s end


end (* structure Parser *)
