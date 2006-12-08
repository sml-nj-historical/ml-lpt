%name CalcParse;
%entry exp;

%tokens
  : KW_let ("let")   |  KW_in   ("in")
  | ID of string     |  NUM of Int.int
  | EQ       ("=")   |  PLUS    ("+")
  | TIMES    ("*")   |  MINUS   ("-")
  | LP       ("(")   |  RP      (")")
  | SEMI     (";")
  | DummyExp of int
  ;

%refcell vars : string list :== ([]);
%refcell nums : int list :== ([]);

prog
  : (exp@(AtomMap.empty) ";")*
  ;
exp(env)
  : "let" ID "=" exp@(env)
    "in" exp@(AtomMap.insert(env, Atom.atom ID, exp1))
      => ( vars :== ID::(!!vars); exp2 )
  | addExp@(env)
  ;
addExp(env)
  : multExp@(env) ("+" multExp@(env))*
      => ( List.foldl op+ multExp SR )
  ;
multExp(env)
  : prefixExp@(env) ("*" prefixExp@(env))*
      => ( List.foldl op* prefixExp SR )
  ;
prefixExp(env)
  : atomicExp@(env)
  | "-" prefixExp@(env)
      => ( ~prefixExp )
  ;
atomicExp(env)
  : ID
      %where ( AtomMap.inDomain (env, Atom.atom ID) )
      => ( valOf(AtomMap.find (env, Atom.atom ID)) )
  | NUM
      => ( nums :== NUM::(!!nums); NUM )
  | "(" exp@(env) ")"
  | DummyExp
  ;
