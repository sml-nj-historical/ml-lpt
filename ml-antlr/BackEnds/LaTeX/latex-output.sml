(* 
signature BACK_END =
  sig
    val output : (LLKSpec.grammar * TextIO.outstream * string)
                   -> unit
    end
*)

structure LaTeXOutput (* : BACK_END *) =
  struct

    structure S = LLKSpec

    (* escape : char -> string -> string *)
    fun escape c = let
          fun backslash [] = []
            | backslash (x::xs) = 
                if x = c 
		then #"\\" :: x :: (backslash xs)
		else x :: (backslash xs)
          in
            (implode o backslash o explode)
          end

    val catw = String.concatWith

(*    fun prod p = let
          val (S.PROD {rhs, ...}) = p
          val i2s = Item.toString
          in
            catw "{}" (map (fn i => concat ["{\\tt ", i2s i, "}"]) rhs)
          end
*)
    fun prods nt = let
          val (S.NT {prods, ...}) = nt
          in
	    !prods
          end

    fun lhs p = let
          val (S.PROD {lhs, ...}) = p
          in
            lhs
          end

    fun rhs p = let
          val (S.PROD {rhs, ...}) = p
          in
            rhs
          end

    val nt2s = Nonterm.toString
    val i2s = Item.toString

    val spacer = " $\\;$ " (* " \\hspace{0.2cm} " *)

    fun tt s = concat ["{\\tt ", s, "}"]
    fun bf s = concat ["{\\bf ", s, "}"]

    (* keyword hack! TODO remove this *)
    fun keywordHack s =
          if String.isPrefix "KW_" s
          then bf (String.extract (s, 3, NONE))
          else s

    fun itemBlab i = print (concat [(case i
				      of S.TOK _ => "TOK"
				       | S.NONTERM _ => "NONTERM"
				       | S.CLOS nt => "CLOS"
				       | S.POSCLOS _ => "POSCLOS"
				       | S.OPT _ => "OPT"),
				    " ", i2s i, "\n"])

    fun mkFirst p = 
          concat ["\\GFirst{\\it ",
		  nt2s (lhs p),
		  "}{\\it ",
		  catw spacer (map (fn i => item(i,false,true)) (rhs p)),
		  "}{}\n"]

    and mkNext p =
          concat ["\\GNext{\\it ", 
		  catw spacer (map (fn i => item(i,false,true)) (rhs p)), 
		  "}{}\n"]

    and prod (p, isSubrule, isFirst) =
          if isSubrule
          then catw spacer (map (fn i => item (i, isSubrule, isFirst)) (rhs p)) 
          else if isFirst
               then mkFirst p
               else mkNext p

    and nonterm (nt, isSubrule) = let
          val (p::ps) = prods nt
          in
            concat ((prod (p, isSubrule, true)) 
		    :: (map (fn p => prod (p, isSubrule, false)) ps))
          end

    and item (i, isSubrule, isFirst) = (case i
          of S.TOK _ => (keywordHack o i2s) i
	   | S.NONTERM _ => (keywordHack o i2s) i
	   | S.CLOS nt => concat ["(", (fn t => nonterm (t, true)) nt, ")*"] 
	   | S.POSCLOS nt => concat ["(", (fn t => nonterm (t, true)) nt, ")+"]
	   | S.OPT nt => concat ["(", (fn t => nonterm (t, true)) nt, ")?"]
         (* end case *))

    fun gram s = concat ["\\Grammar{\n", s, "\n}\n"]

    fun nonterms [] = "the null grammar" 
      | nonterms ns = let
            val ns' = map (fn t => nonterm (t, false)) ns
            in
              gram (catw "\n\\\\" ns')
            end

    (* output grammar *)
    fun grammarHook spec strm = let
	  val (S.Grammar {sortedTops, ...}, _) = spec
          val nts = List.concat sortedTops
	  val g = nonterms nts
          in
            TextIO.output (strm, g)
          end

    val template = ExpandFile.mkTemplate "BackEnds/LaTeX/template.tex"

    fun output (grm, pm, fname) = 
          ExpandFile.expand' {
	      src = template,
	      dst = fname ^ ".tex",
	      hooks = [("grammar", grammarHook (grm, pm))]
	    }

  end
