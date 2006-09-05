(* lex-gen.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * DFA generation using RE derivatives
 *)

structure LexGen :
  sig

    val gen : LexSpec.spec -> LexOutputSpec.spec

  end = struct

    structure RE = RegExp
    structure SIS = RegExp.SymSet
    structure LO = LexOutputSpec

    structure Map = RedBlackMapFn (
      struct
	type ord_key = RE.re Vector.vector
	val compare = Vector.collate RE.compare
      end)

  (* given a list of RE vectors (start states), produce a DFA recognizer 
   * NOTE: invoked once per start state (each start state has a DFA)
   *)
    fun mkDFA startVecs = let
	  val n = ref 0 (* next state id *)
	  val states = ref []
	(* return the state that the re vector maps to and 
	 * a flag set to true if the state is new.
	 *)
	  fun mkState (stateMap, res, asSS) = (case Map.find(stateMap, res)
		 of NONE => let
		      val id = !n
		      fun addFinal (idx, re, finals) = 
			    if RE.nullable re
			    then idx :: finals
			    else finals
		      val q = LO.State {
			      id = id, startState = asSS, label = res,
			      final = Vector.foldri addFinal [] res,
			      next = ref []
			    }
		      in
			n := id+1;
			states := q :: !states;
			(true, q, Map.insert(stateMap, res, q))
		      end
		  | SOME q => (false, q, stateMap)
		(* end case *))
	  fun initIter (states, stateMap, []) = (List.rev states, stateMap)
	    | initIter (states, stateMap, vec::vecs) = let
		val (_, q, stateMap') = mkState (stateMap, vec, true)
                in initIter (q :: states, stateMap', vecs)
                end
	  val (initStates, initStatemap) = initIter ([], Map.empty, startVecs)
	  fun f (stateMap, []) = stateMap
	    | f (stateMap, LO.State{next, label, ...}::workList) = let
		fun move ((res, edge), (stateMap, workList)) = 
		      if Vector.all RE.isNone res (* if error transition *)
		        then (stateMap, workList)
		        else let
			  val (isNew, q, stateMap) = mkState (stateMap, res, false)
			  in
			    next := (edge, q) :: !next;
			    if isNew
			      then (stateMap, q::workList)
			      else (stateMap, workList)
			  end
                val edges = RE.derivatives label
		in
		  f (List.foldl move (stateMap, workList) edges)
		end
	  in
	    ignore (f (initStatemap, initStates));
	    (initStates, List.rev(!states), !n)
	  end

  (* clamp a machine to the right character set *)
    fun clamp clampTo states = let
	  val ascii127 = SIS.interval (0w0, 0w127)
          fun clampTrans (edge, q) = 
	        (SIS.intersect (ascii127, edge), q)
          fun clampState (LO.State{next, ...}) = 
		next := List.map clampTrans (!next)
          in 
            (List.app clampState states;
	     states)
          end

    fun minimize (initStates, states, numStates) = let
          val statesVec = Vector.fromList states
          val marked = Array2.array (numStates, numStates, false)
	  fun isMarked (i, j) = if i < j then Array2.sub (marked, i, j)
				else Array2.sub (marked, j, i)
	  fun mark (i, j) = if i < j then Array2.update (marked, i, j, true)
			    else Array2.update (marked, j, i, true)
	  fun appAll f = let
	        fun iter i = if i < numStates then (f i; iter (i+1))
			     else ()
                in iter 0 end
	  fun appAll2 f = 
	        appAll (fn i => 
		  appAll (fn j =>
		    if i < j then f (i, j)
		    else ()))
	  fun markAll i = appAll (fn j => mark (i, j))
	  fun idOf (LO.State {id, ...}) = id
	  fun iter() = let
	    val changed = ref false
	    fun diffEdge (_, []) = false
	      | diffEdge ((set1, state1), (set2, state2)::edges) = 
		  if SIS.isEmpty (SIS.intersect (set1, set2))
		  then diffEdge ((set1, state1), edges)
		  else 
		    if isMarked (idOf state1, idOf state2)
		    then true
		    else diffEdge ((set1, state1), edges)
	    fun getEdgesAndLabels (i) = let
	          val LO.State {next, ...} = Vector.sub (statesVec, i)
		  val allLabels = foldl SIS.union SIS.empty 
					(map (fn (set, _) => set) (!next))
	          in
	            (!next, allLabels)
	          end
	    fun check (i, j) = 
		if isMarked (i, j) then ()
		else let
		  val (edgesi, labelsi) = getEdgesAndLabels i
		  val (edgesj, labelsj) = getEdgesAndLabels j
		  fun tryEdge edge = diffEdge (edge, edgesj)
                  in
		    case SIS.compare (labelsi, labelsj)
		     of EQUAL => 
			  if List.exists tryEdge edgesi
			  then (mark (i, j); changed := true)
			  else ()
		      | _ => (mark (i, j); changed := true)
		  end  
	    in
	      appAll2 check;
	      if !changed then iter() else ()
	    end
	  fun merge() = let
	    val totSaved = ref (0 : int)
	    in
	      appAll2 (fn (i, j) => if isMarked (i, j) then ()
				    else totSaved := !totSaved + 1);
	      print (" " ^ Int.toString (numStates - (!totSaved)) ^
		     " states in minimized DFA\n");
	      (initStates, states, numStates)
            end
          in
            app (fn LO.State {id, ...} => markAll id) initStates;
            app (fn LO.State {final = [], ...} => ()
		  | LO.State {id, ...} => markAll id)
		states;
	    iter();
	    merge()
          end

    fun gen spec = let
(* TODO: check for invalid start states on rules *)
	  val LexSpec.Spec {decls, conf, rules} = spec
	  val LexSpec.Conf {structName, header,
			    arg, startStates, ...} = conf
	  val startStates' = AtomSet.add (startStates, Atom.atom "INITIAL")
(*
	(* split out actions and associate each ruleSpec to an action ID
	 *
	 * Note: matchActions tries to find textually idential actions and map
	 * them to the same entry in the action vector
	 *)
	  fun matchActions rules = let
	        fun iter ((ruleSpec, action)::rules, 
			  ruleSpecs, actions, actionMap, n) = let
		      val key = Atom.atom action
		      val (i, actions', actionMap', n') = 
			    case AtomMap.find (actionMap, key)
			     of NONE => (n, action::actions,
					 AtomMap.insert (actionMap, key, n),
					 n+1)
			      | SOME i => (i, actions, actionMap, n)
		      in
			iter (rules, (i, ruleSpec)::ruleSpecs,
			      actions', actionMap', n')
		      end
		  | iter ([], ruleSpecs, actions, _, _) = 
		      (List.rev ruleSpecs, List.rev actions)
	        in
	          iter (rules, [], [], AtomMap.empty, 0)
		end
	  val (ruleSpecs, actions) = matchActions rules
*)
	  val (ruleSpecs, actions) = ListPair.unzip rules
	  val actionsVec = Vector.fromList actions
	  val startStates = AtomSet.listItems startStates'
	  fun SSVec label = let
	        fun hasRule (NONE, re) = re
		  | hasRule (SOME ss, re) = 
		      if AtomSet.member (ss, label)
		      then re
		      else RegExp.none
		val rules = List.map hasRule ruleSpecs
                in Vector.fromList rules
		end
	  val (initStates, states, numStates) = 
	        mkDFA (List.map SSVec startStates)
	  val (initStates, states, numStates) = 
	        if !Options.minimize then
		  minimize (initStates, states, numStates)
		else (initStates, states, numStates)
          in LO.Spec {
               decls = decls,
	       header = (if String.size header = 0
			 then "structure " ^ 
			        (if String.size structName = 0
				 then "Mlex"
				 else structName)
			 else header),
	       arg = arg,
	       actions = actionsVec,
	       dfa = states,
	       startStates = ListPair.zip 
			       (List.map Atom.toString startStates, 
				initStates)
	     }
          end

  end
