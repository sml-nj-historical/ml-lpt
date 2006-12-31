structure 
CalcParseToks = struct

    datatype token = EOF
      | DummyExp of int
      | SEMI
      | RP
      | LP
      | MINUS
      | TIMES
      | PLUS
      | EQ
      | NUM of Int.int
      | ID of string
      | KW_in
      | KW_let

    val allToks = [EOF, SEMI, RP, LP, MINUS, TIMES, PLUS, EQ, KW_in, KW_let]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (DummyExp(_)) => "DummyExp"
  | (SEMI) => ";"
  | (RP) => ")"
  | (LP) => "("
  | (MINUS) => "-"
  | (TIMES) => "*"
  | (PLUS) => "+"
  | (EQ) => "="
  | (NUM(_)) => "NUM"
  | (ID(_)) => "ID"
  | (KW_in) => "in"
  | (KW_let) => "let"
(* end case *))
    fun isKW tok =
(case (tok)
 of (EOF) => false
  | (DummyExp(_)) => false
  | (SEMI) => false
  | (RP) => false
  | (LP) => false
  | (MINUS) => false
  | (TIMES) => false
  | (PLUS) => false
  | (EQ) => false
  | (NUM(_)) => false
  | (ID(_)) => false
  | (KW_in) => false
  | (KW_let) => false
(* end case *))


  fun toksToString toks = String.concatWith " " (map toString toks)

  fun isEOF EOF = true
    | isEOF _ = false

end

functor CalcParse(Lex : LEXER) = struct

  local
    structure Tok = 
CalcParseToks
    infix :==

    type 'a refcell = (unit -> 'a) * ('a -> unit)
    fun (r, w) :== n = w n
    fun !! (r, w) = r()

    structure UserCode = struct



fun exp_PROD_1_ACT (EQ, ID, env, exp1, exp2, KW_in, KW_let, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), exp1_SPAN : (Lex.pos * Lex.pos), exp2_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), vars, nums) = 
  (  vars :== ID::(!!vars); exp2 )
fun addExp_PROD_1_ACT (SR, env, multExp, SR_SPAN : (Lex.pos * Lex.pos), multExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), vars, nums) = 
  (  List.foldl op+ multExp SR )
fun multExp_PROD_1_ACT (SR, env, prefixExp, SR_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), vars, nums) = 
  (  List.foldl op* prefixExp SR )
fun prefixExp_PROD_2_ACT (env, MINUS, prefixExp, MINUS_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), vars, nums) = 
  (  ~prefixExp )
fun atomicExp_PROD_1_ACT (ID, env, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), vars, nums) = 
  (  valOf(AtomMap.find (env, Atom.atom ID)) )
fun atomicExp_PROD_2_ACT (NUM, env, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), vars, nums) = 
  (  nums :== NUM::(!!nums); NUM )
fun atomicExp_PROD_1_PRED (ID, env, vars, nums) = 
  (  AtomMap.inDomain (env, Atom.atom ID) )
fun ARGS_1 (vars, nums) = 
  (AtomMap.empty)
fun ARGS_3 (EQ, ID, env, KW_let, vars, nums) = 
  (env)
fun ARGS_4 (EQ, ID, env, exp1, KW_in, KW_let, vars, nums) = 
  (AtomMap.insert(env, Atom.atom ID, exp1))
fun ARGS_5 (env, vars, nums) = 
  (env)
fun ARGS_8 (env, PLUS, multExp, vars, nums) = 
  (env)
fun ARGS_7 (env, vars, nums) = 
  (env)
fun ARGS_11 (env, TIMES, prefixExp, vars, nums) = 
  (env)
fun ARGS_10 (env, vars, nums) = 
  (env)
fun ARGS_12 (env, vars, nums) = 
  (env)
fun ARGS_14 (env, MINUS, vars, nums) = 
  (env)
fun ARGS_18 (LP, env, vars, nums) = 
  (env)

      val ehargs = 
{vars =  [], nums =  []} : {vars : string list, nums : int list}
    end

    structure R = RepairableStrm(Tok)(Lex)
    structure Err = ErrHandler(R)
    structure EBNF = EBNF(R)

    exception ParseError = Err.RepairableError

    fun mk lexFn = let
        val eh = Err.mkErrHandler UserCode.ehargs
	fun wrap f = Err.wrap eh f
	val whileDisabled = Err.whileDisabled eh
	fun tryProds (strm, prods) = (wrap (Err.tryProds eh prods)) strm
      val vars_REFC = (fn () => #vars (Err.getState eh),fn n => let val {vars, nums} = Err.getState eh in Err.setState (eh, {nums = nums, vars = n}) end)
      val nums_REFC = (fn () => #nums (Err.getState eh),fn n => let val {vars, nums} = Err.getState eh in Err.setState (eh, {vars = vars, nums = n}) end)
      fun unwrap (ret, strm, repairs, state) = (ret, R.unwrap strm, repairs, state)
	val lex = R.get1
val matchEOF = wrap (fn strm => (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchDummyExp = wrap (fn strm => (case (lex(strm))
 of (Tok.DummyExp(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchSEMI = wrap (fn strm => (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
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
val matchMINUS = wrap (fn strm => (case (lex(strm))
 of (Tok.MINUS, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchTIMES = wrap (fn strm => (case (lex(strm))
 of (Tok.TIMES, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchPLUS = wrap (fn strm => (case (lex(strm))
 of (Tok.PLUS, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchEQ = wrap (fn strm => (case (lex(strm))
 of (Tok.EQ, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchNUM = wrap (fn strm => (case (lex(strm))
 of (Tok.NUM(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchID = wrap (fn strm => (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_in = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_in, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))
val matchKW_let = wrap (fn strm => (case (lex(strm))
 of (Tok.KW_let, span, strm') => ((), span, strm')
  | _ => raise(ParseError)
(* end case *)))

val (prog_NT, exp_NT) = 
let
fun exp_NT (env_RES) (strm) = let
      fun exp_PROD_1 (strm) = let
            val (KW_let_RES, KW_let_SPAN, strm') = matchKW_let(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (exp1_RES, exp1_SPAN, strm') = (wrap (exp_NT (UserCode.ARGS_3 (EQ_RES, ID_RES, env_RES, KW_let_RES, vars_REFC, nums_REFC))))(strm')
            val (KW_in_RES, KW_in_SPAN, strm') = matchKW_in(strm')
            val (exp2_RES, exp2_SPAN, strm') = (wrap (exp_NT (UserCode.ARGS_4 (EQ_RES, ID_RES, env_RES, exp1_RES, KW_in_RES, KW_let_RES, vars_REFC, nums_REFC))))(strm')
            val FULL_SPAN = (#1(KW_let_SPAN), #2(exp2_SPAN))
            in
              (UserCode.exp_PROD_1_ACT (EQ_RES, ID_RES, env_RES, exp1_RES, exp2_RES, KW_in_RES, KW_let_RES, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), exp1_SPAN : (Lex.pos * Lex.pos), exp2_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), vars_REFC, nums_REFC),
                FULL_SPAN, strm')
            end
      fun exp_PROD_2 (strm) = let
            val (addExp_RES, addExp_SPAN, strm') = (wrap (addExp_NT (UserCode.ARGS_5 (env_RES, vars_REFC, nums_REFC))))(strm)
            val FULL_SPAN = (#1(addExp_SPAN), #2(addExp_SPAN))
            in
              (addExp_RES, FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.DummyExp(_), _, strm') => exp_PROD_2(strm)
          | (Tok.LP, _, strm') => exp_PROD_2(strm)
          | (Tok.MINUS, _, strm') => exp_PROD_2(strm)
          | (Tok.NUM(_), _, strm') => exp_PROD_2(strm)
          | (Tok.ID(_), _, strm') => exp_PROD_2(strm)
          | (Tok.KW_let, _, strm') => exp_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
and addExp_NT (env_RES) (strm) = let
      val (multExp_RES, multExp_SPAN, strm') = (wrap (multExp_NT (UserCode.ARGS_7 (env_RES, vars_REFC, nums_REFC))))(strm)
      fun subrule1_NT (strm) = let
            val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm)
            val (multExp_RES, multExp_SPAN, strm') = (wrap (multExp_NT (UserCode.ARGS_8 (env_RES, PLUS_RES, multExp_RES, vars_REFC, nums_REFC))))(strm')
            val FULL_SPAN = (#1(PLUS_SPAN), #2(multExp_SPAN))
            in
              (multExp_RES, FULL_SPAN, strm')
            end
      fun subrule1_PRED (strm) = (case (lex(strm))
             of (Tok.PLUS, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(subrule1_PRED, (wrap subrule1_NT), strm')
      val FULL_SPAN = (#1(multExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.addExp_PROD_1_ACT (SR_RES, env_RES, multExp_RES, SR_SPAN : (Lex.pos * Lex.pos), multExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), vars_REFC, nums_REFC),
          FULL_SPAN, strm')
      end
and multExp_NT (env_RES) (strm) = let
      val (prefixExp_RES, prefixExp_SPAN, strm') = (wrap (prefixExp_NT (UserCode.ARGS_10 (env_RES, vars_REFC, nums_REFC))))(strm)
      fun subrule1_NT (strm) = let
            val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm)
            val (prefixExp_RES, prefixExp_SPAN, strm') = (wrap (prefixExp_NT (UserCode.ARGS_11 (env_RES, TIMES_RES, prefixExp_RES, vars_REFC, nums_REFC))))(strm')
            val FULL_SPAN = (#1(TIMES_SPAN), #2(prefixExp_SPAN))
            in
              (prefixExp_RES, FULL_SPAN, strm')
            end
      fun subrule1_PRED (strm) = (case (lex(strm))
             of (Tok.TIMES, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(subrule1_PRED, (wrap subrule1_NT), strm')
      val FULL_SPAN = (#1(prefixExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.multExp_PROD_1_ACT (SR_RES, env_RES, prefixExp_RES, SR_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), vars_REFC, nums_REFC),
          FULL_SPAN, strm')
      end
and prefixExp_NT (env_RES) (strm) = let
      fun prefixExp_PROD_1 (strm) = let
            val (atomicExp_RES, atomicExp_SPAN, strm') = (wrap (atomicExp_NT (UserCode.ARGS_12 (env_RES, vars_REFC, nums_REFC))))(strm)
            val FULL_SPAN = (#1(atomicExp_SPAN), #2(atomicExp_SPAN))
            in
              (atomicExp_RES, FULL_SPAN, strm')
            end
      fun prefixExp_PROD_2 (strm) = let
            val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm)
            val (prefixExp_RES, prefixExp_SPAN, strm') = (wrap (prefixExp_NT (UserCode.ARGS_14 (env_RES, MINUS_RES, vars_REFC, nums_REFC))))(strm')
            val FULL_SPAN = (#1(MINUS_SPAN), #2(prefixExp_SPAN))
            in
              (UserCode.prefixExp_PROD_2_ACT (env_RES, MINUS_RES, prefixExp_RES, MINUS_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), vars_REFC, nums_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.MINUS, _, strm') => prefixExp_PROD_2(strm)
          | (Tok.DummyExp(_), _, strm') => prefixExp_PROD_1(strm)
          | (Tok.LP, _, strm') => prefixExp_PROD_1(strm)
          | (Tok.NUM(_), _, strm') => prefixExp_PROD_1(strm)
          | (Tok.ID(_), _, strm') => prefixExp_PROD_1(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
and atomicExp_NT (env_RES) (strm) = let
      fun atomicExp_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            in
              if (UserCode.atomicExp_PROD_1_PRED (ID_RES, env_RES, vars_REFC, nums_REFC))
                then let
                  val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
                  in
                    (UserCode.atomicExp_PROD_1_ACT (ID_RES, env_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), vars_REFC, nums_REFC),
                      FULL_SPAN, strm')
                  end
                else raise ParseError
            end
      fun atomicExp_PROD_2 (strm) = let
            val (NUM_RES, NUM_SPAN, strm') = matchNUM(strm)
            val FULL_SPAN = (#1(NUM_SPAN), #2(NUM_SPAN))
            in
              (UserCode.atomicExp_PROD_2_ACT (NUM_RES, env_RES, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), vars_REFC, nums_REFC),
                FULL_SPAN, strm')
            end
      fun atomicExp_PROD_3 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (exp_RES, exp_SPAN, strm') = (wrap (exp_NT (UserCode.ARGS_18 (LP_RES, env_RES, vars_REFC, nums_REFC))))(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (exp_RES, FULL_SPAN, strm')
            end
      fun atomicExp_PROD_4 (strm) = let
            val (DummyExp_RES, DummyExp_SPAN, strm') = matchDummyExp(strm)
            val FULL_SPAN = (#1(DummyExp_SPAN), #2(DummyExp_SPAN))
            in
              (DummyExp_RES, FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.DummyExp(_), _, strm') => atomicExp_PROD_4(strm)
          | (Tok.NUM(_), _, strm') => atomicExp_PROD_2(strm)
          | (Tok.ID(_), _, strm') => atomicExp_PROD_1(strm)
          | (Tok.LP, _, strm') => atomicExp_PROD_3(strm)
          | _ => raise(ParseError)
        (* end case *))
      end
fun prog_NT (strm) = let
      fun subrule1_NT (strm) = let
            val (exp_RES, exp_SPAN, strm') = (wrap (exp_NT (UserCode.ARGS_1 (vars_REFC, nums_REFC))))(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(exp_SPAN), #2(SEMI_SPAN))
            in
              (exp_RES, FULL_SPAN, strm')
            end
      fun subrule1_PRED (strm) = (case (lex(strm))
             of (Tok.DummyExp(_), _, strm') => true
              | (Tok.LP, _, strm') => true
              | (Tok.MINUS, _, strm') => true
              | (Tok.NUM(_), _, strm') => true
              | (Tok.ID(_), _, strm') => true
              | (Tok.KW_let, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(subrule1_PRED, (wrap subrule1_NT), strm)
      val (EOF_RES, EOF_SPAN, strm') = matchEOF(strm')
      val FULL_SPAN = (#1(SR_SPAN), #2(EOF_SPAN))
      in
        (SR_RES, FULL_SPAN, strm')
      end
in
  (prog_NT, exp_NT)
end
val prog_NT =  fn s => unwrap (Err.launch eh (prog_NT ) (R.wrap (s, lexFn)))
val exp_NT =  fn x => fn s => unwrap (Err.launch eh (exp_NT x ) (R.wrap (s, lexFn)))

in (prog_NT, exp_NT) end
  in
fun parse lexFn  s = let val (prog_NT, exp_NT) = mk lexFn in prog_NT s end

fun parseexp lexFn  x s = let val (prog_NT, exp_NT) = mk lexFn in exp_NT x s end

  end

end
