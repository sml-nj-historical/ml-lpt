structure CalcTest = 
  struct

    structure Tok = CalcParseToks

    structure ListLex = struct
      type strm = Tok.token list
      fun lex [] = (Tok.EOF, (0, 0), [])
	| lex (t::ts) = (t, (0, 0), ts)
      type pos = StreamPos.pos
      type span = pos * pos
      fun getPos _ = 0
    end

    structure CP = CalcParse(ListLex)

    fun fragToToks (SMLofNJ.QUOTE s) = let
          val sref = ref true
          fun input _ = if !sref then
			  (sref := false; s)
			else ""
	  val lex = CalcLex.lex (StreamPos.mkSourcemap())
          fun loop ((Tok.EOF, _, _), accum) = rev accum
	    | loop ((s, _, strm), accum) = loop (lex strm, s::accum)
          in
            loop (lex (CalcLex.streamify input), [])
          end
      | fragToToks (SMLofNJ.ANTIQUOTE i) = [Tok.DummyExp i]

    fun % frags = let
      val (r, s', errs, {vars, nums}) = CP.parseexp ListLex.lex AtomMap.empty (List.concat (map fragToToks frags))
    in
      app (fn (_, repair) => print (Repair.actionToString Tok.toksToString repair ^ "\n")) errs;
      print (" -- VARS: " ^ (String.concatWith ", " vars) ^ "\n");
      print (" -- NUMS: " ^ (String.concatWith ", " (map Int.toString nums)) ^ "\n");
      (r, s')
    end

(*    val _ = Control.quotation := true *)

  end
