(* err.sml
 *
 * COPYRIGHT (c) 2006 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Global error handling, including printing of error messages
 * and a flag to trigger halting of the tool.
 *)

structure Err = 
  struct

  (* signal that the program should be aborted with no further messages printed *)
    exception Abort

  (* global flag to record the existance of errors *)
    val anyErrors = ref false
    val leftRecurs : string list ref = ref []

    fun abortIfErr() = if !anyErrors then raise Abort else ()

    fun errMsg l = (
	  anyErrors := true;
	  TextIO.output(TextIO.stdErr, String.concat l ^ "\n"))

    fun warning l = 
	  TextIO.output(TextIO.stdErr, String.concat l ^ "\n")

  (* error function for lexers *)
    fun lexErr filename (lnum, msg) = errMsg [
	    "Error [", filename, ":", Int.toString lnum, "]: ", msg
	  ]

  (* error function for parsers *)
    fun parseErr filename (msg, p1, p2) = if (p1 = p2)
	  then lexErr filename (p1, msg)
	  else errMsg [
	      "Error [", filename, ":", Int.toString p1, "-", Int.toString p2, "]: ",
	      msg
	    ]
  (* left recursion detected *)
    fun leftRecur name = 
	if List.exists (fn n => (n = name)) (!leftRecurs)
	then ()
	else (
	  leftRecurs := name::(!leftRecurs);
	  print (String.concat 
	    ["Left recursion detected: ", name, " -> ", name, " ...\n"]))

    val printDebug = ref true

  (* print a debugging message *)
    fun debug s = if !printDebug then (print s; print "\n") else ()
    fun debugs ss = debug (concat ss)

  (* print a status message *)
    fun status s = print (concat ["[ml-antlr: ", s, "]\n"])
    fun statuss ss = status (concat ss)

  end
