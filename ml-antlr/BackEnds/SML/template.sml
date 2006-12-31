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

    infix :==

    type 'a refcell = (unit -> 'a) * ('a -> unit)
    fun (r, w) :== n = w n
    fun !! (r, w) = r()

    structure UserCode = struct

@defs@

      val ehargs = 
@ehargs@

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
@refcells@

	val lex = R.get1
@matchfns@

@parser@

end
