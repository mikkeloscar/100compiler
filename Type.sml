structure Type :> Type =
struct

  (* Use "raise Error (message,position)" for error messages *)
  exception Error of string*(int*int)

  type pos = int*int

  datatype Type = Int
	    | Char
	    | IntRef
	    | CharRef
		

  fun convertType (S100.Int _) = Int
    | convertType (S100.Char _) = Char  

  fun convertTypeRef (S100.Int _) = IntRef
    | convertTypeRef (S100.Char _) = CharRef

  fun getName (S100.Val (f,p)) = f
    | getName (S100.Ref (f,p)) = f

  fun getType t (S100.Val (f,p)) = convertType t
    | getType t (S100.Ref (f,p)) = convertTypeRef t

  (* lookup function for symbol table as list of (name,value) pairs *)
  fun lookup x []
        = NONE
    | lookup x ((y,v)::table)
        = if x=y then SOME v else lookup x table

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
	| (_,_) =>
	  if t1=t2 then t2
	  else raise Error ("Type mismatch in assignment",p)
	end
    | S100.Plus (e1,e2,p) =>
       (case (checkExp e1 vtable ftable,
	       checkExp e2 vtable ftable) of
	    (Int,Int) => Int
	  |  (Int,Ref) => Ref
	  |  (Ref,Int) => Ref
	  |  (_,_) => raise Error ("Type mismatch in assignment",p))

    | S100.Minus (e1,e2,p) =>
      (case (checkExp e1 vtable ftable,
	     checkExp e2 vtable ftable) of
	   (Int, Int) => Int
	 | (IntRef, Int) => Int
	 | (IntRef, IntRef) => Int
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
    | S100.Lookup (s,e,p) =>
        (case lookup s vtable of
	    SOME t => if t = IntRef orelse t = CharRef
                  then if t = IntRef then Int else Char
                  else raise Error ("This is not a reference: "^s,p)

	  | NONE => raise Error ("Unkown pointer: "^s,p))

  fun extend [] _ vtable = vtable
    | extend (S100.Val (x,p)::sids) t vtable =
        (case lookup x vtable of
	   NONE => extend sids t ((x,convertType t)::vtable)
    | SOME _ => raise Error ("Double declaration of "^x,p))
    | extend (S100.Ref (x,p)::sids) t vtable =
        (case lookup x vtable of
	   NONE => extend sids t ((x,convertTypeRef t)::vtable)
	 | SOME _ => raise Error ("Double declaration of "^x,p))
 
  fun checkDecs [] = []
    | checkDecs ((t,sids)::ds) =
      extend (List.rev sids) t (checkDecs ds)


  fun checkStat s vtable ftable =
    case s of
      S100.EX e => (checkExp e vtable ftable; ())
    | S100.If (e,s1,p) =>
        if checkExp e vtable ftable = Int
	then
	    case s1 of
		S100.Block(d,s,p) => 
		let
		    val statlist = List.map (fn st => checkStat st vtable ftable) s
		in
		    ()
		end
	      |  _ =>  checkStat s1 vtable ftable
	else raise Error ("Condition should be integer",p)
    | S100.IfElse (e,s1,s2,p) =>
      if checkExp e vtable ftable = Int
      then
	  case (s1,s2) of
	      (S100.Block(d,s,p),S100.Block(d2,se,p2)) => 
	      let
		  val statlist = List.map (fn st => checkStat st vtable ftable) s
		  val statlist2 = List.map (fn st => checkStat st vtable ftable) se
	      in

			  ()
	      end
	    | (S100.Block(d,s,p),_) =>
	      let
		  val statlist = List.map (fn st => checkStat st vtable ftable) s
	      in
		  checkStat s2 vtable ftable
	      end

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
	      S100.Block(d,s,p) => 
	      let
		  val statlist = List.map (fn st => checkStat st vtable ftable) s
	      in
		  ()
	      end
	    |  _ =>  checkStat s1 vtable ftable
	else raise Error ("Condition should be integer",p)
    | S100.Return (e,p) => (checkExp e vtable ftable; ())
    | S100.Block (d,stats,p) =>
      let
	   val decs = checkDecs d
	   val statlist = List.map (fn st => checkStat st decs ftable) stats
      in
	   () (* if function reaches this, all stats and decs in block are ok *)
      end


  fun checkFunDec(t,sf,decs,body,p) ftable =
      let  
	  val vtable = checkDecs decs
	  val numReturns = []
      in
	  case body of 
	      S100.Return (e,p) => 

	      if getType t sf = checkExp e vtable ftable
		  then checkStat body vtable ftable
	      else raise Error("Return type is not the same as function type",p)
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
      val ftable = getFuns fs [("walloc",([Int],IntRef)),
                   ("balloc",([Int],CharRef)),
                   ("getint",([],Int)),
			       ("putint",([Int],Int)),
                   ("getstring",([Int],CharRef)),(* getstring har ikke korrekt
                                                returtype endnu, kun for at teste *)
                   ("putstring",([CharRef],CharRef))] (* korrekt type? *) 
    in
      List.app (fn f => checkFunDec f ftable) fs;
      case lookup "main" ftable of
	NONE => raise Error ("No main function found",(0,0))
      | SOME ([],Int) => ()
      | _ => raise Error ("main function has illegal type",(0,0))
    end
end
