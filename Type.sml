structure Type :> Type =
struct

  (* Use "raise Error (message,position)" for error messages *)
  exception Error of string*(int*int)
	   
  type pos = int*int

  (* 4 diffent datatypes. Only 2 in S100 *)
  datatype Type = Int
		| Char
		| IntRef
		| CharRef
		
  (* takes a S100.Type and returns equal Type.Type for S100.Val *)
  fun convertType (S100.Int _) = Int
    | convertType (S100.Char _) = Char  

  (* takes a S100.Type and returns equal Type.Type for S100.Ref *)
  fun convertTypeRef (S100.Int _) = IntRef
    | convertTypeRef (S100.Char _) = CharRef

  (* return the name for variable *)
  fun getName (S100.Val (f,p)) = f
    | getName (S100.Ref (f,p)) = f

  (* combines the covertTypes *)
  fun getType t (S100.Val (f,p)) = convertType t
    | getType t (S100.Ref (f,p)) = convertTypeRef t

  (* lookup function for symbol table as list of (name,value) pairs *)
  fun lookup x [] = NONE
    | lookup x ((y,v)::table)
      = if x=y then SOME v else lookup x table

  (* checks expression and returns type *)
  fun checkExp e vtable ftable =
      case e of
       S100.NumConst _ => Int
     | S100.CharConst _ => Char
     | S100.StringConst s => CharRef
     | S100.LV lv => checkLval lv vtable ftable
     | S100.Assign (lv,e1,p) =>
       let
         val t1 = checkLval lv vtable ftable
         val t2 = checkExp e1 vtable ftable
       in
         case (t1,t2) of 
           (Int,IntRef) => Int
         | (Char,CharRef) => Char
         | (Char,Int) => Char
         | (Int,Char) => Int
         | (_,_) =>
            if t1=t2 then t2
            else raise Error ("Type mismatch in assignment",p)
         end
     | S100.Plus (e1,e2,p) =>
       (case (checkExp e1 vtable ftable,
            checkExp e2 vtable ftable) of
         (Int,Int) => Int
       | (Int,IntRef) => IntRef
       | (Int,CharRef) => CharRef
       | (_,_) => raise Error ("Type mismatch in assignment",p))

     | S100.Minus (e1,e2,p) =>
       (case (checkExp e1 vtable ftable,
	      checkExp e2 vtable ftable) of
         (Int, Int) => Int
       | (IntRef, Int) => Int
       | (IntRef, IntRef) => Int
       | (CharRef, Int) => CharRef
       | (CharRef, CharRef) => Int
       | (_,_) => raise Error ("Type mismatch in assignment",p))
     | S100.Less (e1,e2,p) =>
       if checkExp e1 vtable ftable = checkExp e2 vtable ftable
       then Int else raise Error ("Can't compare different types",p)
     | S100.Equal (e1,e2,p) =>
       if checkExp e1 vtable ftable = checkExp e2 vtable ftable
       then Int else raise Error ("Can't compare different types",p)
     | S100.Call (f,es,p) =>
       (case lookup f ftable of
	 NONE => raise Error ("Unknown function: "^f,p)
       | SOME (parT,resultT) =>
	 let
	     val argT = List.map (fn e => checkExp e vtable ftable) es
	 in
	     if parT = argT then resultT
	     else raise Error ("Arguments don't match declaration of "^f, p)
	 end)
  (* checks Lval and returns type if it exeists *)
  and checkLval lv vtable ftable =
      case lv of
       S100.Var (x,p) =>
       (case lookup x vtable of
	    SOME t => t
	  | NONE => raise Error ("Unknown variable: "^x,p))
     | S100.Deref (x,p) =>
       (case lookup x vtable of
	    SOME t => if t = Int then IntRef else CharRef
	  | NONE => raise Error ("Unknown pointer: "^x,p))
     (* checks if lookup is made on reference, and raises error if not *)
     | S100.Index (s,e,p) =>
       (case lookup s vtable of
	 SOME t => if t = IntRef orelse t = CharRef
                   then if t = IntRef then Int else Char
                   else raise Error ("This is not a reference: "^s,p)
       | NONE => raise Error ("Unkown pointer: "^s,p))

  (* extends symbol table if variable not present *)
  fun extend [] _ vtable = vtable
    | extend (S100.Val (x,p)::sids) t vtable =
      (case lookup x vtable of
	NONE => extend sids t ((x,convertType t)::vtable)
      | SOME _ => raise Error ("Double declaration of "^x,p))
    | extend (S100.Ref (x,p)::sids) t vtable =
      (case lookup x vtable of
	NONE => extend sids t ((x,convertTypeRef t)::vtable)
      | SOME _ => raise Error ("Double declaration of "^x,p))
 
  (* checks declarations and returns table *)
  fun checkDecs [] = []
    | checkDecs ((t,sids)::ds) =
      extend (List.rev sids) t (checkDecs ds)

  (* check statement, and return unit type when no error occured *)
  fun checkStat s vtable ftable =
    case s of
     S100.EX e => (checkExp e vtable ftable; ())
   | S100.If (e,s1,p) =>
     if checkExp e vtable ftable = Int
     then
	 (* if If-statement is made with a block, do not use regular block-scope *)
	 case s1 of
	  S100.Block(d,s,p) => 
	  let
	      val statlist = List.map (fn st => checkStat st vtable ftable) s
	  in
	      ()
	  end
	(* If-statement without block *)
	|  _ =>  checkStat s1 vtable ftable
     else raise Error ("Condition should be integer",p)
   | S100.IfElse (e,s1,s2,p) =>
     if checkExp e vtable ftable = Int
     then
	 (* when if and else statements is made with blocks *)
	 case (s1,s2) of
	  (S100.Block(d,s,p),S100.Block(d2,se,p2)) => 
	  let
	      val statlist = List.map (fn st => checkStat st vtable ftable) s
	      val statlist2 = List.map (fn st => checkStat st vtable ftable) se
	  in
	      ()
	  end
	(* when only if statement is made with block *)
	| (S100.Block(d,s,p),_) =>
	  let
	      val statlist = List.map (fn st => checkStat st vtable ftable) s
	  in
	      checkStat s2 vtable ftable
	  end
	(* when only else statement is made with block *)
	| (_,S100.Block(d,s,p)) =>
	  let
	      val statlist = List.map (fn st => checkStat st vtable ftable) s
	  in
	      checkStat s1 vtable ftable
	  end
	|  (_,_) =>  (checkStat s1 vtable ftable; checkStat s2 vtable ftable)
     else raise Error ("Condition should be integer",p)
   | S100.While (e,s1,p) =>
     if checkExp e vtable ftable = Int then
	 case s1 of
	     (* if while-statement is made with block *)	
	     S100.Block(d,s,p) => 
	     let
		 val statlist = List.map (fn st => checkStat st vtable ftable) s
	     in
		 ()
	     end
	   |  _ =>  checkStat s1 vtable ftable
     else raise Error ("Condition should be integer",p)
   (* for now just return unit type, check is done later *)
   | S100.Return (e,p) => ()
   | S100.Block (d,stats,p) =>
     let
	 val decs = checkDecs d
	 val statlist = List.map (fn st => checkStat st (decs@vtable) ftable) stats
     in
	 () (* if function reaches this, all stats and decs in block are ok *)
     end
    
  (* checks if statement contains return statement in all paths *)
  fun checkReturn(s) = 
      let
	  (* checks if a boolean-list contains a 'true' *)
	  fun exists [] = false
	    | exists [x] = x
	    | exists (x::xs) = x orelse exists xs
      in
   	  case s of
	   S100.Return _ => true (* return is obviously true *)
	 | S100.Block (d,se,p) =>
	   let
	       (* create a list of returnchecked statements and check if a return exists here *)
	       val returnlist = (map checkReturn se)
	   in
	       exists returnlist
	   end
	 (* both the if statement and the else statement must contain a return statement before its true
	    the if and the else part can also hold more if else parts, block etc. *)
	    | S100.IfElse (e, st1,st2,p) =>
	      (case (st1,st2) of
		(S100.Block (d,s,p), S100.Block (d2,s2,p2)) =>
		let
		    val return1 = (map checkReturn s)
		    val return2 = (map checkReturn s2)
		in
		    exists return1 andalso exists return2
		end
	      | (S100.Block (d,s,p), S100.Return _) => 
		let
		    val returnlist = (map checkReturn s)
		in
		    exists returnlist
		end						  
	      | (S100.Block (d,s,_), S100.IfElse (e,s1,s2,_)) =>
		let
		    val returnlist = (map checkReturn s)
		in
		    if exists returnlist andalso (checkReturn(s1) andalso checkReturn (s2))then true else false
		end
	      | (S100.Block (d,s,p), _) => false
	      | (S100.Return _, S100.Block (d,s,p)) => 
		let
		    val returnlist = (map checkReturn s)
		in
		    if exists returnlist then true else false
		end
	      | (S100.IfElse (e,s1,s2,_), S100.Block (d,s,_)) =>
		let
		    val returnlist = (map checkReturn s)
		in
		    exists returnlist andalso (checkReturn(s1) andalso checkReturn (s2))
		end
	      | (_, S100.Block (d,s,p)) => false
	      | (S100.Return _, S100.Return _) => true
	      | (_,_) => false)
	    | _ => false
      end

  (* checks function *)
  fun checkFunDec(t,sf,decs,body,p) ftable =
      let
	  val vtable = checkDecs decs
      in
	  (* checks if the returned type is same as declared in function *)
	  case body of 
	      S100.Return (e,p) => 
	      if getType t sf = checkExp e vtable ftable
	      then checkStat body vtable ftable
	      else raise Error("Returning type is not the same as the declared type",p)
	    | S100.Block (d,s,p) => 
	      let
		  val foreach = List.map (fn ch => checkFunDec (t,sf,d@decs,ch,p) ftable) s
	      in
		  ()
	      end
	    | _ => checkStat body vtable ftable
      end

  fun getFuns [] ftable = ftable
    | getFuns ((t,sf,decs,_,p)::fs) ftable =
      case lookup (getName sf) ftable of
       NONE =>
       let
           val parT = (List.map (#2) (checkDecs decs))
	   val resultT = getType t sf
       in
           getFuns fs ((getName sf, (parT,resultT)) :: ftable)
       end
     | SOME _ => raise Error ("Redeclaration of "^ getName sf,p)

  fun checkProg fs =
      let
	  (* the built-in functions in 100 *)
	  val ftable = getFuns fs [("walloc",([Int],IntRef)),
				   ("balloc",([Int],CharRef)),
				   ("getint",([],Int)),
				   ("putint",([Int],Int)),
				   ("getstring",([Int],CharRef)),
				   ("putstring",([CharRef],CharRef))]
	  (* checks if return exists in function *)
	  fun contain (_,_,_,b,p) = if (checkReturn b) then () else raise Error("No return statement in all paths",p)
      in
	  List.app (fn f => checkFunDec f ftable) fs;
	  List.app (fn f => contain f) fs;
	  (* check if main funciton exists *)
	  case lookup "main" ftable of
	   NONE => raise Error ("No main function found",(0,0))
	 | SOME ([],Int) => ()
	 | _ => raise Error ("main function has illegal type",(0,0))
      end
end
