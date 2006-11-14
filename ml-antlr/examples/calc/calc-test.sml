structure CalcTest = 
  struct

    structure ListLex = struct
      type strm = Tok.token list
      fun lex [] = NONE
	| lex (t::ts) = SOME (t, ts)
    end

    structure CP = CalcParse(ListLex)

    fun fragToToks (SMLofNJ.QUOTE s) = let
          val sref = ref true
          fun input _ = if !sref then
			  (sref := false; s)
			else ""
          fun loop (NONE, accum) = rev accum
	    | loop (SOME (s, strm), accum) = loop (CalcLex.lex strm, s::accum)
          in
            loop (CalcLex.lex (CalcLex.streamify input), [])
          end
      | fragToToks (SMLofNJ.ANTIQUOTE i) = [Tok.DummyExp i]

    fun % frags = let
      val (r, s', errs) = CP.parseexp AtomMap.empty (List.concat (map fragToToks frags))
    in
      app (fn (_, repair) => print (CP.repairToString repair ^ "\n")) errs;
      (r, s')
    end

(*    val _ = Control.quotation := true *)

  end
