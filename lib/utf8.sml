(* utf8.sml
 *
 * COPYRIGHT (c) 2004 The Moby Project (moby.cs.uchicago.edu)
 * All rights reserved.
 * USED WITH PERMISSION
 *
 * Routines for working with UTF8 encoded strings.
 *
 *	Unicode value		1st byte    2nd byte    3rd byte    4th byte
 *	-----------------	--------    --------    --------    --------
 *	00000000 0xxxxxxx	0xxxxxxx	
 *	00000yyy yyxxxxxx	110yyyyy    10xxxxxx
 *	zzzzyyyy yyxxxxxx	1110zzzz    10yyyyyy	10xxxxxx
 *	110110ww wwzzzzyy+
 *	110111yy yyxxxxxx	11110uuu    10uuzzzz	10yyyyyy    10xxxxxx!
 *
 * (!) where uuuuu = wwww+1
 *
 * TODO:
 *    Add support for surrogate pairs (this will require changing the type
 *    of wchar to Word32.word).
 *)

structure UTF8 = struct

  structure W = Word32
  type wchar = W.word

  exception Incomplete
  (* raised by some operations when applied to incomplete strings. *)

  fun getu getc strm = let
    fun getContByte (strm, wc) = 
	(case getc strm
	  of NONE => raise Incomplete
	   | SOME(c, strm') => let
	       val b = W.fromInt(Char.ord c)
	     in
	       if (W.andb(0wxc0, b) = 0wx80)
	       then (strm', W.orb(W.<<(wc, 0w6), W.andb(0wx3f, b)))
	       else raise Incomplete
	     end
         (* end case *))
  in case getc strm
      of NONE => NONE
       | SOME(c, strm') => let
	   val w = W.fromInt(Char.ord c)
	 in
	   if (w < 0w128)
	   then SOME (w, strm')
	   else (case (W.andb(0wxe0, w))
		  of 0wxc0 => let
		       val (strm', wc) = getContByte(strm', W.andb(0wx1f, w))
		     in
		       SOME (wc, strm')
		     end
		   | 0wxe0 => let
		       val (strm', wc) =
			   getContByte(
				       getContByte(strm', W.andb(0wx0f, w)))
		     in
		       SOME (wc, strm')
		     end
		   | _ => raise Incomplete
	         (* end case *))
	 end
     (* end case *)
  end

  fun getList getc strm = let
    val get1 = getu getc
    fun iter (strm, accum) = 
	(case get1 strm
	  of NONE => rev accum
	   | SOME (w, strm') => iter (strm', w::accum)
	 (* end case *))
  in
    iter (strm, [])
  end

end