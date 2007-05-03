structure 
@tokmod@
 = struct

@tokens@


  fun toksToString toks = String.concatWith " " (map toString toks)

  fun isEOF EOF = true
    | isEOF _ = false

end

@header@
 = struct

  local
    structure Tok = 
@tokmod@

    structure UserCode = struct

@defs@

    end

    structure R = RepairableStrm(Tok)(Lex)
    structure Err = ErrHandler(R)
    structure EBNF = EBNF(R)

    exception ParseError = Err.RepairableError

    fun mk lexFn = let
@ehargs@

        val eh = Err.mkErrHandler {get = getS, put = putS}
	fun wrap f = Err.wrap eh f
	fun reqEOF' f s = let
	      val (act, span, s') = f s
	      in (wrap (fn s' => (case R.get1 s'
		  of (Tok.EOF, _, _) => (act, span, s')
		   | _ => raise ParseError))) s'
	      end
	fun reqEOF f = wrap (reqEOF' f)
	val whileDisabled = Err.whileDisabled eh
	fun tryProds (strm, prods) = (wrap (Err.tryProds eh prods)) strm
	val lex = R.get1
@matchfns@

@parser@

end
