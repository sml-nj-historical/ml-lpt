(* wrapped-strm.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * "wrapped" streams, which track the number of tokens read
 * and allow "prepending" a sequence of tokens.
 *)

functor WrappedStrm (Tok : ANTLR_TOKENS) (Lex : ANTLR_LEXER) = struct

  datatype wstream = WSTREAM of {
    prefix : (Tok.token * StreamPos.span) list,
    curTok : int,
    strm : Lex.strm,
    lex : (Lex.strm -> Tok.token * StreamPos.span * Lex.strm) ref
  }

  fun wrap (strm, lex) = let val lex = ref lex in 
        WSTREAM {prefix = [], strm = strm, curTok = 0, lex = lex}
      end
  fun unwrap (WSTREAM {strm, ...}) = strm

  fun get1 (WSTREAM {prefix = (tok, span)::toks, strm, curTok, lex}) = 
        (tok, span, 
	 WSTREAM {prefix = toks, strm = strm, lex = lex, curTok = curTok + 1})
    | get1 (WSTREAM {prefix = [], strm, curTok, lex}) = let
	val (tok, span, strm') = (!lex) strm
        in (tok, span, 
	    WSTREAM {prefix = [], lex = lex, strm = strm', curTok = curTok + 1})
        end

  fun prepend (toks, WSTREAM {prefix, strm, curTok, lex}) = 
        WSTREAM {prefix = toks @ prefix, strm = strm, lex = lex,
		 curTok = curTok - (List.length toks)}

  fun subtract (WSTREAM {curTok = p1, ...}, WSTREAM {curTok = p2, ...}) = 
        p1 - p2

  fun getDiff (ws1, ws2) =
        if subtract (ws1, ws2) <= 0 then []
	else let 
	  val (t, s, ws2') = get1 ws2
	  in (t, s) :: (getDiff (ws1, ws2'))
          end

  (* get position AFTER trimming whitespace *)
  fun getPos ws = let val (_, (left, _), _) = get1 ws in left end
  fun getSpan ws = (getPos ws, getPos ws)

end