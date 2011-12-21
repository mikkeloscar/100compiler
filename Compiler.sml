(* Compiler for 100 *)
(* Compile by mosmlc -c Compiler.sml *)

structure Compiler :> Compiler =
struct


  (* Use "raise Error (message,position)" for error messages *)
  exception Error of string*(int*int)

  (* Name generator.  Call with, e.g., t1 = "tmp"^newName () *)
  val counter = ref 0

  fun newName () = (counter := !counter + 1;
                  "_" ^ Int.toString (!counter)^ "_")

  (* Number to text with spim-compatible sign symbol *)
  fun makeConst n = if n>=0 then Int.toString n
                    else "-" ^ Int.toString (~n)

  (* Create CharConst in correct format e.g. #"a" -> 'a' *)
  fun makeConstChar c = "'" ^ Char.toString(c) ^ "'"

  fun lookup x [] = NONE
    | lookup x ((y,v)::table) = if x=y then SOME v else lookup x table

  fun isIn x [] = false
    | isIn x (y::ys) = x=y orelse isIn x ys

  (* link register *)
  val RA = "31"
  (* Register for stack pointer *)
  val SP = "29"
  (* Register for heap pointer *)
  val HP = "28"
  (* Register for frame pointer *)
  val FP = "25"

  (* Suggested register division *)
  val maxCaller = 15   (* highest caller-saves register *)
  val maxReg = 24      (* highest allocatable register *)

  (* handle declarations and function arguments *)
  fun moveArgs [] r = ([], [], 0)
    | moveArgs ((t,ss)::ds) r =
        moveArgs1 ss (Type.convertType t) ds r
  and moveArgs1 [] t ds r = moveArgs ds r
    | moveArgs1 (s::ss) t ds r =
    let
        val y = newName ()
        val (x,ty,loc) = (
            case s of
              S100.Val (x,p) => (x, t, x^y)
            | S100.Ref (x,p) => (x, t, x^y))
        val rname = Int.toString r
        val (code, vtable, stackSpace) = moveArgs1 ss t ds (r+1)
    in
        if r<=maxCaller
        then (Mips.MOVE (loc, rname) :: code,
             (x,(ty,loc)) :: vtable, stackSpace)
        else (Mips.LW (loc, FP, makeConst stackSpace) :: code,
             (x,(ty,loc)) :: vtable, stackSpace + 4)
      end

  datatype Location = Reg of string (* value is in register *)
                    | Mem of string

  (* compile expression *)
  fun compileExp e vtable ftable place =
    case e of
      S100.NumConst (n,pos) =>
        if n<32768 then
	  (Type.Int,[Mips.LI (place, makeConst n)])
	else
	  (Type.Int,
	   [Mips.LUI (place, makeConst (n div 65536)),
	   Mips.ORI (place, place, makeConst (n mod 65536))])
    | S100.CharConst (c,pos) =>
      (Type.Char,[Mips.LI (place, makeConstChar c)])
    | S100.StringConst (s,pos) => 
      (Type.CharRef, [])
      (* Not implemented *)

    | S100.LV lval =>
        let
          val (code,ty,loc) = compileLval lval vtable ftable
        in
          case (ty,loc) of
            (Type.Int, Reg x) =>
              (Type.Int,
               code @ [Mips.MOVE (place,x)])
          | (Type.Int, Mem x) =>
              (Type.Int,
               code @ [Mips.LW (place,x,"0")])
          | (Type.Char, Reg x) =>
              (Type.Char,
               code @ [Mips.MOVE (place,x)])
          | (Type.Char, Mem x) =>
              (Type.Char,
               code @ [Mips.LW (place,x,"0")])
          | _ => raise Error("LV not implemented",(0,0))
        end
    | S100.Assign (lval,e,p) =>
        let
          val t = "_assign_"^newName()
          val (code0,ty,loc) = compileLval lval vtable ftable
          val (_,code1) = compileExp e vtable ftable t
        in
          case (ty,loc) of
            (Type.Int, Reg x) =>
              (Type.Int,
               code0 @ code1 @ [Mips.MOVE (x,t), Mips.MOVE (place,t)])
          | (Type.Int, Mem x) =>
            (Type.Int,
	     code0 @ code1 @ [Mips.SW (t,x,"0")])
          | (Type.Char, Reg x) =>
            (Type.Char,
             code0 @ code1 @ [Mips.MOVE (x,t), Mips.MOVE (place,t)])
	  | (Type.Char, Mem x) =>
	    (Type.Char,
	     code0 @ code1 @ [Mips.SW (t,x,"0")])

          | _ => raise Error("Not assignable",p)
        end
    | S100.Plus (e1,e2,pos) =>
        let
	      val t1 = "_plus1_"^newName()
	      val t2 = "_plus2_"^newName()
          val (ty1,code1) = compileExp e1 vtable ftable t1
          val (ty2,code2) = compileExp e2 vtable ftable t2
	    in
	      case (ty1,ty2) of
	        (Type.Int, Type.Int) =>
	          (Type.Int,
	          code1 @ code2 @ [Mips.ADD (place,t1,t2)])
	    end
    | S100.Minus (e1,e2,pos) =>
        let
	      val t1 = "_minus1_"^newName()
	      val t2 = "_minus2_"^newName()
          val (ty1,code1) = compileExp e1 vtable ftable t1
          val (ty2,code2) = compileExp e2 vtable ftable t2
	    in
	      case (ty1,ty2) of
	        (Type.Int, Type.Int) =>
	          (Type.Int,
	          code1 @ code2 @ [Mips.SUB (place,t1,t2)])
	    end
    | S100.Less (e1,e2,pos) =>
        let
	      val t1 = "_less1_"^newName()
	      val t2 = "_less2_"^newName()
          val (_,code1) = compileExp e1 vtable ftable t1
          val (_,code2) = compileExp e2 vtable ftable t2
	    in
	      (Type.Int, code1 @ code2 @ [Mips.SLT (place,t1,t2)])
	    end
    | S100.Call (f,es,pos) =>
	let
	  val rTy = case lookup f ftable of
		      SOME (_,t) => t
		    | NONE => raise Error ("unknown function "^f,pos)
	  val (code1,args) = compileExps es vtable ftable
	  fun moveArgs [] r = ([],[],0)
	    | moveArgs (arg::args) r =
	        let
		  val (code,parRegs,stackSpace) = moveArgs args (r+1)
		  val rname = makeConst r
		in
	          if r<=maxCaller then
		    (Mips.MOVE (rname,arg) :: code,
		     rname :: parRegs,
		     stackSpace)
		  else
		    (Mips.SW (arg,SP,makeConst stackSpace) :: code,
		     parRegs,
		     stackSpace + 4)
		end
	  val (moveCode, parRegs, stackSpace) = moveArgs args 2
	in
	  (rTy,
	   if stackSpace>0 then
	     [Mips.ADDI (SP,SP,makeConst (~stackSpace))]
	     @ code1 @ moveCode @
	     [Mips.JAL (f, parRegs),
	      Mips.MOVE (place,"2"),
	      Mips.ADDI (SP,SP,makeConst stackSpace)]
	   else
	     code1 @ moveCode @
	     [Mips.JAL (f, parRegs),
	      Mips.MOVE (place,"2")])
	end

  and compileExps [] vtable ftable = ([], [])
    | compileExps (e::es) vtable ftable =
        let
	  val t1 = "_exps_"^newName()
          val (_,code1) = compileExp e vtable ftable t1
	  val (code2, regs) = compileExps es vtable ftable
	in
	  (code1 @ code2, t1 :: regs)
	end

  and compileLval lval vtable ftable =
    case lval of
      S100.Var (x,p) =>
        (case lookup x vtable of
	      SOME (ty,y) => ([],ty,Reg y)
	    | NONE => raise Error ("Unknown variable "^x,p))
    | S100.Deref (x,p) =>
        (case lookup x vtable of
          SOME (ty,y) => ([],ty,Mem y)
        | NONE => raise Error("Unkown reference "^x,p))
    | S100.Index (x,e,p) => 
        (case lookup x vtable of
           SOME (ty,y) => 
             let
               
               val t = "_index1_"^newName()
               val result = "_index2_"^newName()
               val code1 = #2 (compileExp e vtable ftable t)
               val code2 = code1 @ [Mips.SLL (t,t,"2"), Mips.ADD (t,t,y)]
             in
               (code2, ty, Mem t)
             end
         | NONE => raise Error ("Unknown index "^x,p))
         

  fun compileStat s vtable ftable exitLabel =
    case s of
      S100.EX e => #2 (compileExp e vtable ftable "0")
    | S100.If (e,s1,p) =>
        let
          val t = "_if_"^newName()
          val l1 = "_endif_"^newName()
        in
          case s1 of
            S100.Block (d,s,p) =>
             let 
                val (_,code0) = compileExp e vtable ftable t
                val statlist = List.map
                    (fn st => compileStat st vtable ftable exitLabel) s
                val code1 = foldl (fn (x,y) => y @ x) (hd statlist) (tl statlist)
             in
                  code0 @ [Mips.BEQ (t,"0",l1)] @ code1 @ [Mips.LABEL l1]
             end
          | _ =>
             let 
                val (_,code0) = compileExp e vtable ftable t
                val code1 = compileStat s1 vtable ftable exitLabel
             in
                  code0 @ [Mips.BEQ (t,"0",l1)] @ code1 @ [Mips.LABEL l1]
             end

        end
    | S100.IfElse (e,s1,s2,p) =>
        let
          val t = "_if_"^newName()
          val l1 = "_else_"^newName()
          val l2 = "_endif_"^newName()
          val (_,code0) = compileExp e vtable ftable t
        in  
          case (s1,s2) of
            (S100.Block (_,s1,_), S100.Block (_,s2,_)) =>
              let
                val statlist = List.map
                        (fn st => compileStat st vtable ftable exitLabel) s1
                val code1 = foldl (fn (x,y) => y @ x) (hd statlist) (tl statlist)
                val statlist2 = List.map
                        (fn st => compileStat st vtable ftable exitLabel) s2
                val code2 = foldl (fn (x,y) => y @ x) (hd statlist2) (tl statlist2)
                (* val code1 = compileStat s1 vtable ftable exitLabel *)
                (* val code2 = compileStat s2 vtable ftable exitLabel *)
              in
                code0 @ [Mips.BEQ (t,"0",l1)] @ code1
                @ [Mips.J l2, Mips.LABEL l1] @ code2 @ [Mips.LABEL l2]
              end

          | (S100.Block (_,s,_),_) =>
              let
                val statlist = List.map
                        (fn st => compileStat st vtable ftable exitLabel) s
                val code1 = foldl (fn (x,y) => y @ x) (hd statlist) (tl statlist)
                val code2 = compileStat s2 vtable ftable exitLabel
              in
                code0 @ [Mips.BEQ (t,"0",l1)] @ code1
                @ [Mips.J l2, Mips.LABEL l1] @ code2 @ [Mips.LABEL l2]
              end
          | (_,S100.Block (_,s,_)) =>
              let
                val code1 = compileStat s1 vtable ftable exitLabel
                val statlist = List.map
                        (fn st => compileStat st vtable ftable exitLabel) s
                val code2 = foldl (fn (x,y) => y @ x) (hd statlist) (tl statlist)
              in
                code0 @ [Mips.BEQ (t,"0",l1)] @ code1
                @ [Mips.J l2, Mips.LABEL l1] @ code2 @ [Mips.LABEL l2]
              end
          | _ =>
              let
                val code1 = compileStat s1 vtable ftable exitLabel
                val code2 = compileStat s2 vtable ftable exitLabel
              in
                code0 @ [Mips.BEQ (t,"0",l1)] @ code1
                @ [Mips.J l2, Mips.LABEL l1] @ code2 @ [Mips.LABEL l2]
              end
         end
    | S100.Return (e,p) =>
        let
	  val t = "_return_"^newName()
	  val (_,code0) = compileExp e vtable ftable t
	in
	  code0 @ [Mips.MOVE ("2",t), Mips.J exitLabel]
	end
    | S100.Block (d,s,p) => 
        let
           val l1 = "_block_"^newName()
           (* val (_,decs) = compileExp e vtable ftable l1 *)
	  
	  val (parcode,d_vtable,stackParams) (* move parameters to arguments *)
            = moveArgs d 2
           val statlist = List.map (fn st => compileStat st (d_vtable @ vtable) ftable
           exitLabel) s
          val stats = foldl (fn (x,y) => y @ x) (hd statlist) (tl statlist)
          val (stats1, _, maxr,spilled)  (* call register allocator *)
            = RegAlloc.registerAlloc
                (parcode @ stats) [] 2 maxCaller maxReg 0
        in
          stats1
        end

    | S100.While (e,s,p) => 
        let
            val l1 = "_while_"^newName()
            val l2 = "_endwhile_"^newName()
            val (_,code0) = compileExp e vtable ftable l1
        in
            case s of
              S100.Block (d,s,p) => 
              let
                val statlist = List.map
                      (fn st => compileStat st vtable ftable exitLabel) s
                val code1 = foldl (fn (x,y) => y @ x) (hd statlist) (tl statlist)
              in
                [Mips.LABEL l1] @ code0 @ [Mips.BEQ (l1,"0",l2)] @
                code1 @ [Mips.J l1] @ [Mips.LABEL l2]
              end
            | _ =>
              let 
                val code1 = compileStat s vtable ftable exitLabel
              in
                [Mips.LABEL l1] @ code0 @ [Mips.BEQ (l1,"0",l2)] @
                code1 @ [Mips.J l1] @ [Mips.LABEL l2]
              end
        end

  (* code for saving and restoring callee-saves registers *)
  fun stackSave currentReg maxReg savecode restorecode offset =
    if currentReg > maxReg
    then (savecode, restorecode, offset)  (* done *)
    else stackSave (currentReg+1)
                   maxReg
                   (Mips.SW (makeConst currentReg,
                                 SP,
                                 makeConst offset)
                    :: savecode) (* save register *)
                   (Mips.LW (makeConst currentReg,
                                 SP,
                                 makeConst offset)
                    :: restorecode) (* restore register *)
                   (offset+4) (* adjust offset *)

  (* compile function declaration *)
  and compileFun ftable (typ, sf, args, body, (line,col)) =
        let
	  val fname = Type.getName sf
	  val rty = Type.getType typ sf
	  fun moveArgs [] r = ([], [], 0)
	    | moveArgs ((t,ss)::ds) r =
	        moveArgs1 ss (Type.convertType t) ds r
	  and moveArgs1 [] t ds r = moveArgs ds r
	    | moveArgs1 (s::ss) t ds r =
	       let
		 val y = newName ()
		 val (x,ty,loc) = (case s of
			         S100.Val (x,p) => (x, t, x^y)
			       | S100.Ref (x,p) => (x, t, x^y))
		 val rname = Int.toString r
		 val (code, vtable, stackSpace) = moveArgs1 ss t ds (r+1)
	       in
		   if r<=maxCaller then
		     (Mips.MOVE (loc, rname) :: code,
		      (x,(ty,loc)) :: vtable,
		      stackSpace)
		   else
		     (Mips.LW (loc, FP, makeConst stackSpace) :: code,
		      (x,(ty,loc)) :: vtable,
		      stackSpace + 4)
	       end
	  val (parcode,vtable,stackParams) (* move parameters to arguments *)
            = moveArgs args 2
          val body = compileStat body vtable ftable (fname ^ "_exit")
          val (body1, _, maxr,spilled)  (* call register allocator *)
            = RegAlloc.registerAlloc
                (parcode @ body) [] 2 maxCaller maxReg 0
          val (savecode, restorecode, offset) = (* save/restore callee-saves *)
                stackSave (maxCaller+1) (maxr+1) [] [] (4*spilled)
		(* save one extra callee-saves register for saving SP *)
	  val ctext = if spilled>0
		  then "Spill of "^makeConst spilled ^ " variables occurred"
		  else ""
        in
            [Mips.COMMENT ctext,
             Mips.LABEL fname]  (* function label *)
	  @ (if stackParams>0 then [Mips.MOVE (FP,SP)] else [])
	  @ [Mips.ADDI (SP,SP,makeConst (~4-offset)), (* move SP down *)
             Mips.SW (RA, SP, makeConst offset)] (* save return address *)
          @ savecode  (* save callee-saves registers *)
          @ body1  (* code for function body *)
	  @ [Mips.LABEL (fname^"_exit")] (* exit label *)
          @ restorecode  (* restore callee-saves registers *)
          @ [Mips.LW (RA, SP, makeConst offset), (* restore return addr *)
             Mips.ADDI (SP,SP,makeConst (offset+4)), (* move SP up *)
             Mips.JR (RA, [])] (* return *)
        end

  (* compile program *)
  fun compile funs =
    let
      val ftable =
	  Type.getFuns funs [("walloc",([Type.Int],Type.IntRef)),
			     ("balloc",([Type.Int],Type.CharRef)),
                             ("getint",([],Type.Int)),
			     ("putint",([Type.Int],Type.Int))]
      val funsCode = List.concat (List.map (compileFun ftable) funs)
    in
      [Mips.TEXT "0x00400000",
       Mips.GLOBL "main",
       Mips.LA (HP, "_heap_")]    (* initialise heap pointer *)
      @ [Mips.JAL ("main",[]),    (* run program *)
	 Mips.LI ("2","10"),      (* syscall control = 10 *)
         Mips.SYSCALL]            (* exit *)
      @ funsCode		  (* code for functions *)

      @ [
     Mips.LABEL "walloc", (* walloc not implemented *)

     Mips.SLL("2","2","2"), (* mult by 2 *)
     Mips.ADDI("4","2","0"), (* add parameter to input *)
     Mips.LI("2","9"), (* syscall sbrk *)
     Mips.SYSCALL, (* execute *)
     Mips.JR (RA,[]),

     Mips.LABEL "balloc", (* balloc not implemented *)
     Mips.SLL("2","2","2"), (* mult by 2 *)
     Mips.ADDI("4","2","0"), (* add parameter to input *)
     Mips.LI("2","9"), (* syscall sbrk *)
     Mips.SYSCALL, (* execute *)
     Mips.JR (RA,[]),


     Mips.LABEL "getstring", (* getstring not implemented *)
     
     Mips.LABEL "putstring", (* putstring not implemented *)
     (* Mips. *)
     (* Mips.SYSCALL, *)
      
     Mips.LABEL "putint",     (* putint function *)
	 Mips.ADDI(SP,SP,"-8"),
	 Mips.SW ("2",SP,"0"),    (* save used registers *)
	 Mips.SW ("4",SP,"4"),
	 Mips.MOVE ("4","2"),
	 Mips.LI ("2","1"),       (* write_int syscall *)
	 Mips.SYSCALL,
	 Mips.LI ("2","4"),       (* writestring syscall *)
	 Mips.LA("4","_cr_"),
	 Mips.SYSCALL,            (* write CR *)
	 Mips.LW ("2",SP,"0"),    (* reload used registers *)
	 Mips.LW ("4",SP,"4"),
	 Mips.ADDI(SP,SP,"8"),
	 Mips.JR (RA,[]),

	 Mips.LABEL "getint",     (* getint function *)
	 Mips.LI ("2","5"),       (* read_int syscall *)
	 Mips.SYSCALL,
	 Mips.JR (RA,[]),

	 Mips.DATA "",
	 Mips.ALIGN "2",
	 Mips.LABEL "_cr_",       (* carriage return string *)
	 Mips.ASCIIZ "\n",
	 Mips.ALIGN "2",

	 Mips.LABEL "_heap_",     (* heap space *)
	 Mips.SPACE "100000"]
    end

end
