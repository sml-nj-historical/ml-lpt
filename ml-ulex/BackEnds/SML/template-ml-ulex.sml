@header@
  = struct

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

    structure yyUTF8 = struct

      structure W = Word32
      type wchar = W.word

      exception Incomplete
	(* raised by some operations when applied to incomplete strings. *)

      fun getu getc strm = let
          fun getContByte (strm, wc) = (case getc strm
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
	    fun iter (strm, accum) = (case get1 strm
		  of NONE => rev accum
		   | SOME (w, strm') => iter (strm', w::accum)
	         (* end case *))
            in
	      iter (strm, [])
            end

    end

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getline : stream -> int
	val subtract : stream * stream -> Substring.substring
	val eof : stream -> bool

      end = struct

        val chunkSize = 4096

        datatype stream = S of (buf * int * int) 
	and buf = B of {
	    data : string,
	    basePos : int,
	    more : more ref,
	    inputN : int -> string
          }
	and more = UNKNOWN | YES of buf | NO
        
	fun mkStream inputN = 
	      (S (B {data = "", basePos = 0, 
		     more = ref UNKNOWN,
		     inputN = inputN},
		  0, 0))

	fun getc (S (buf as B {data, basePos, more, inputN}, pos, line)) = 
	      if pos < String.size data then let
		val c = String.sub (data, pos)
		val line' = if c = #"\n" then line+1 else line
		in
		  SOME (c, S (buf, pos+1, line'))
		end
	      else (case !more
		     of NO => NONE
		      | YES buf' => getc (S (buf', 0, line))
		      | UNKNOWN => 
			  (case inputN chunkSize
			    of "" => (more := NO; NONE)
			     | data' => let 
				 val buf' = B {data = data',
					       basePos = basePos + 
						 String.size data,
					       more = ref UNKNOWN,
					       inputN = inputN}
			         in
				   more := YES buf';
				   getc (S (buf', 0, line))
			         end
			   (* end case *))
		    (* end case *))

	fun getpos (S (B {basePos, ...}, pos, _)) = basePos + pos
	fun getline (S (_, _, line)) = line

	fun subtract (new, old) = let
	      val (S (B {data = ndata, basePos = nbasePos, ...}, npos, _)) = new
	      val (S (B {data = odata, basePos = obasePos, 
			 more, inputN}, opos, _)) = old
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
			       subtract (new, S (buf, 0, 0))],
			     0, NONE)
	      end

	fun eof (S (B {data, more, ...}, pos, _)) = 
	      pos >= String.size data andalso 
	      (case !more
		of NO => true
		 | _ => false)

      end

    datatype 'a yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * 'a action * 'a yymatch
    withtype 'a action = yyInput.stream * 'a yymatch -> 'a

    datatype yystart_state = 
@startstates@

    structure UserDeclarations = 
      struct

@userdecls@


      end

    exception yyEOF

    fun innerLex (yystrm_, yyss_) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	(* get one char of input *)
	  val yygetc = yyUTF8.getu yyInput.getc 
	(* create yytext *)
	  fun yymksubstr(strm) = yyInput.subtract (strm, !yystrm)
	  fun yymktext(strm) = Substring.string (yymksubstr strm)
	  fun yymkunicode(strm) = yyUTF8.getList Substring.getc (yymksubstr strm)
          open UserDeclarations
          fun lex 
@args@ 
 = let
            fun yystuck (yyNO_MATCH) = raise Fail "lexer reached a stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getline
	    fun continue() = 
@lexer@

	    in (continue(), !yystrm, !yyss) end
          in 
            lex
	    handle IO.Io{cause, ...} => raise cause
          end
    
    type tok = UserDeclarations.lex_result
    datatype prestrm = STRM of yyInput.stream * 
		(yystart_state * tok * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex(STRM (yystrm, memo), ss) = (case !memo
	  of NONE => (let
	     val (tok, yystrm', ss') = innerLex (yystrm, ss) ()
	     val strm' = STRM (yystrm', ref NONE);
	     in 
	       memo := SOME (ss, tok, strm', ss');
	       SOME (tok, (strm', ss'))
	     end
	     handle yyEOF => NONE)
	   | SOME (ss', tok, strm', ss'') => 
	       if ss = ss' then
		 SOME (tok, (strm', ss''))
	       else (
		 memo := NONE;
		 lex (STRM (yystrm, memo), ss))
         (* end case *))

(*	  
    fun lex(STRM (ref(EVAL t))) = SOME t
      | lex(STRM (s as ref(UNEVAL f))) = let
	  val tok = f()
          val t = (tok, STRM(ref(UNEVAL f))) 
          in
	    s := EVAL t; 
	    SOME(t)
          end
	  handle yyEOF => NONE

    fun streamify inputN = STRM(ref(UNEVAL (mk (yyInput.mkStream inputN))))
*)
(*    fun cons(a,s) = STRM(ref(EVAL(a,s))) *)

    fun streamify inputN = (STRM (yyInput.mkStream inputN, ref NONE), INITIAL)

    fun getLineNo (STRM (strm, _), _) = yyInput.getline strm
    fun getPos    (STRM (strm, _), _) = yyInput.getpos strm

  end
