structure 
@tokmod@
 = struct

@tokens@

  fun isEOF EOF = true
    | isEOF _ = false

end

@header@
 = struct

  local
    structure Tok = 
@tokmod@

    structure UserCode =
      struct
@usrdefs@

@actions@
      end (* UserCode *)

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)
    structure EBNF = AntlrEBNF(
      struct
	type strm = Err.wstream
	val getSpan = Err.getSpan
      end)

    fun mk lexFn = let
@ehargs@
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) = 
	        (Err.whileDisabled eh (fn() => prod strm)) 
		handle Err.ParseError => try (prods)
          in try prods end
@matchfns@

@parser@

end
