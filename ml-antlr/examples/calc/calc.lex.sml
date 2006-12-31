structure CalcLex  = struct

    datatype yystart_state = 
INITIAL
    local
    structure UserDeclarations = 
      struct

 
  open CalcParseToks
  type lex_result = token

  fun eof() = EOF


      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    fun innerLex (yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yygetPos() = ULexBuffer.getpos (!yystrm)
        (* start position of token -- can be updated via skip() *)
	  val yystartPos = ref (yygetPos())
	(* get one char of input *)
	  fun yygetc strm = (case UTF8.getu ULexBuffer.getc strm
                of (SOME (0w10, s')) => 
		     (StreamPos.markNewLine yysm (ULexBuffer.getpos strm);
		      SOME (0w10, s'))
		 | x => x)
	(* create yytext *)
	  fun yymksubstr(strm) = ULexBuffer.subtract (strm, !yystrm)
	  fun yymktext(strm) = Substring.string (yymksubstr strm)
	  fun yymkunicode(strm) = UTF8.getList Substring.getc (yymksubstr strm)
          open UserDeclarations
          fun lex () = let
            fun yystuck (yyNO_MATCH) = raise Fail "lexer reached a stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yygetPos()
	    fun yygetlineNo strm = StreamPos.lineNo yysm (ULexBuffer.getpos strm)
	    fun continue() = 
let
fun yyAction0 (strm, lastMatch) = (yystrm := strm;   KW_let )
fun yyAction1 (strm, lastMatch) = (yystrm := strm;   KW_in )
fun yyAction2 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   ID (yytext) 
      end
fun yyAction3 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   NUM (valOf (Int.fromString (yytext))) 
      end
fun yyAction4 (strm, lastMatch) = (yystrm := strm;   EQ )
fun yyAction5 (strm, lastMatch) = (yystrm := strm;   PLUS )
fun yyAction6 (strm, lastMatch) = (yystrm := strm;   MINUS )
fun yyAction7 (strm, lastMatch) = (yystrm := strm;   TIMES )
fun yyAction8 (strm, lastMatch) = (yystrm := strm;   LP )
fun yyAction9 (strm, lastMatch) = (yystrm := strm;   RP )
fun yyAction10 (strm, lastMatch) = (yystrm := strm;   SEMI )
fun yyAction11 (strm, lastMatch) = (yystrm := strm;   continue() )
fun yyQ1 (strm, lastMatch) = yyAction4(strm, yyNO_MATCH)
fun yyQ2 (strm, lastMatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ3 (strm, lastMatch) = yyAction6(strm, yyNO_MATCH)
fun yyQ4 (strm, lastMatch) = yyAction7(strm, yyNO_MATCH)
fun yyQ5 (strm, lastMatch) = yyAction8(strm, yyNO_MATCH)
fun yyQ6 (strm, lastMatch) = yyAction9(strm, yyNO_MATCH)
fun yyQ7 (strm, lastMatch) = yyAction10(strm, yyNO_MATCH)
fun yyQ8 (strm, lastMatch) = yyAction11(strm, yyNO_MATCH)
fun yyQ9 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ9(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction3(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ9(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction2(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ10(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ10(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction1(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ10(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ10(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ10(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ10(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction2(strm, yyNO_MATCH)
                      else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx6E
              then yyQ15(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp <= 0wx60
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ10(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ10(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction0(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ10(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ10(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ10(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ10(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction2(strm, yyNO_MATCH)
                      else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx74
              then yyQ14(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx74
              then if inp <= 0wx60
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction2(strm, yyNO_MATCH)
                      else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx65
              then yyQ13(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx65
              then if inp <= 0wx60
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then if ULexBuffer.eof(strm)
                  then UserDeclarations.eof(())
                  else yystuck(lastMatch)
            else if inp < 0wx3A
              then if inp = 0wx29
                  then yyQ6(strm', lastMatch)
                else if inp < 0wx29
                  then if inp = 0wx20
                      then yyQ8(strm', lastMatch)
                    else if inp < 0wx20
                      then if inp = 0wx9
                          then yyQ8(strm', lastMatch)
                        else if inp < 0wx9
                          then if ULexBuffer.eof(strm)
                              then UserDeclarations.eof(())
                              else yystuck(lastMatch)
                        else if inp <= 0wxA
                          then yyQ8(strm', lastMatch)
                        else if ULexBuffer.eof(strm)
                          then UserDeclarations.eof(())
                          else yystuck(lastMatch)
                    else if inp = 0wx28
                      then yyQ5(strm', lastMatch)
                    else if ULexBuffer.eof(strm)
                      then UserDeclarations.eof(())
                      else yystuck(lastMatch)
                else if inp = 0wx2D
                  then yyQ3(strm', lastMatch)
                else if inp < 0wx2D
                  then if inp = 0wx2B
                      then yyQ2(strm', lastMatch)
                    else if inp = 0wx2A
                      then yyQ4(strm', lastMatch)
                    else if ULexBuffer.eof(strm)
                      then UserDeclarations.eof(())
                      else yystuck(lastMatch)
                else if inp <= 0wx2F
                  then if ULexBuffer.eof(strm)
                      then UserDeclarations.eof(())
                      else yystuck(lastMatch)
                  else yyQ9(strm', lastMatch)
            else if inp = 0wx61
              then yyQ10(strm', lastMatch)
            else if inp < 0wx61
              then if inp = 0wx3E
                  then if ULexBuffer.eof(strm)
                      then UserDeclarations.eof(())
                      else yystuck(lastMatch)
                else if inp < 0wx3E
                  then if inp = 0wx3C
                      then if ULexBuffer.eof(strm)
                          then UserDeclarations.eof(())
                          else yystuck(lastMatch)
                    else if inp = 0wx3B
                      then yyQ7(strm', lastMatch)
                      else yyQ1(strm', lastMatch)
                else if inp = 0wx41
                  then yyQ10(strm', lastMatch)
                else if inp < 0wx41
                  then if ULexBuffer.eof(strm)
                      then UserDeclarations.eof(())
                      else yystuck(lastMatch)
                else if inp <= 0wx5A
                  then yyQ10(strm', lastMatch)
                else if ULexBuffer.eof(strm)
                  then UserDeclarations.eof(())
                  else yystuck(lastMatch)
            else if inp = 0wx6C
              then yyQ12(strm', lastMatch)
            else if inp < 0wx6C
              then if inp = 0wx69
                  then yyQ11(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp <= 0wx7A
              then yyQ10(strm', lastMatch)
            else if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of INITIAL => yyQ0(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            and skip() = (yystartPos := yygetPos(); continue())
	    in (continue(), (!yystartPos, yygetPos()), !yystrm, !yyss) end
          in 
            lex()
          end
  in
    type pos = StreamPos.pos
    type span = StreamPos.span
    type tok = UserDeclarations.lex_result

    datatype prestrm = STRM of ULexBuffer.stream * 
		(yystart_state * tok * span * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex sm (STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex (yystrm, ss, sm)
	     val strm' = STRM (yystrm', ref NONE);
	     in 
	       memo := SOME (ss, tok, span, strm', ss');
	       (tok, span, (strm', ss'))
	     end
	   | SOME (ss', tok, span, strm', ss'') => 
	       if ss = ss' then
		 (tok, span, (strm', ss''))
	       else (
		 memo := NONE;
		 lex sm (STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify inputN = (STRM (ULexBuffer.mkStream inputN, ref NONE), 
			    INITIAL)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end
