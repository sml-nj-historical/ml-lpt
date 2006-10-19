structure Preitem = 
  struct

    structure S = LLKSpec

    fun toString (S.TOK t) = Token.toString t
      | toString (S.NONTERM (nt, args)) = 
	  String.concat ([Nonterm.toString nt] @ (
	    case args
	     of SOME args =>
		["@(", Action.toString args, ")"]
	      | _ =>  []))
      | toString (S.CLOS nt) = Nonterm.toString nt ^ "*"
      | toString (S.POSCLOS nt) = Nonterm.toString nt ^ "+"
      | toString (S.OPT nt) = Nonterm.toString nt ^ "?"

    fun listToString l = String.concatWith " " (map toString l)

    fun name (S.TOK t) = Token.name t
      | name (S.NONTERM (nt, _)) = Nonterm.name nt
      | name (S.CLOS nt) = Nonterm.name nt
      | name (S.POSCLOS nt) = Nonterm.name nt
      | name (S.OPT nt) = Nonterm.name nt

  end