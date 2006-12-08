(* ulex-buffer.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Forward-chained buffers for lexing
 *)

structure ULexBuffer : sig

  type stream
  val mkStream : (int -> string) -> stream
  val getc : stream -> (Char.char * stream) option
  val getpos : stream -> StreamPos.pos
  val subtract : stream * stream -> Substring.substring
  val eof : stream -> bool

end = struct

  val chunkSize = 4096

  datatype stream = S of (buf * int) 
  and buf = B of { 
    data : string,
    basePos : StreamPos.pos,
    more : more ref,
    inputN : int -> string
  }
  and more = UNKNOWN | YES of buf | NO
        
  fun mkStream inputN = 
        (S (B {data = "", basePos = 0, 
	       more = ref UNKNOWN,
	       inputN = inputN},
	    0))

  fun getc (S (buf as B {data, basePos, more, inputN}, pos)) = 
        if pos < String.size data then let
	    val c = String.sub (data, pos)
	  in
	    SOME (c, S (buf, pos+1))
	  end
	else (case !more
	       of NO => NONE
		| YES buf' => getc (S (buf', 0))
		| UNKNOWN => 
		    (case inputN chunkSize
		      of "" => (more := NO; NONE)
		       | data' => let 
			   val buf' = B {
                               data = data',
			       basePos = StreamPos.forward (basePos, String.size data),
			       more = ref UNKNOWN,
			       inputN = inputN
			     }
			   in
			     more := YES buf';
			     getc (S (buf', 0))
			   end
		     (* end case *))
              (* end case *))

  fun getpos (S (B {basePos, ...}, pos)) = StreamPos.forward (basePos, pos)

  fun subtract (new, old) = let
        val (S (B {data = ndata, basePos = nbasePos, ...}, npos)) = new
	val (S (B {data = odata, basePos = obasePos, 
		   more, inputN}, opos)) = old
        in
          if nbasePos = obasePos then
	    Substring.substring (ndata, opos, npos-opos)
	  else case !more
		of NO =>      raise Fail "BUG: yyInput.subtract, but buffers are unrelated"
		 | UNKNOWN => raise Fail "BUG: yyInput.subtract, but buffers are unrelated"
		 | YES buf => 
		     Substring.extract (
		       Substring.concat [
			 Substring.extract (odata, opos, NONE),
			 subtract (new, S (buf, 0))],
		       0, NONE)
        end

  fun eof s = not (isSome (getc s))

end