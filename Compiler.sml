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
        let
            val size = String.size(s)
            val t = "_string_"^newName()
            fun saveMem [] = []
              | saveMem [c] = [Mips.ADDI (t,"0", makeConstChar c),
                               Mips.SW (t,"2","0")]
              | saveMem (c::cs) = [Mips.ADDI (t,"0", makeConstChar c),
                                   Mips.SW (t,"2","0"),
                                   Mips.ADDI ("2", "2", "4")] @ saveMem cs
        in 
          (* Use balloc to allocate space *)
          (Type.CharRef,
            [Mips.ADDI ("2","0",Int.toString(size+1)), (* nullbyte fix *)
             Mips.JAL ("balloc",[]), 
             Mips.ADDI (place, "2", "0")] @
             saveMem (explode(s)) @ [Mips.ADDI ("2","2","4"),
                                     Mips.ADDI (t, "0", "0"),
                                     Mips.SW (t,"2","0")])              
        end
    | S100.LV lval =>
        let
          val (code,ty,loc) = compileLval lval vtable ftable
        in
          case (ty,loc) of
            (t, Reg x) =>
              (t,
               code @ [Mips.MOVE (place,x)])
          | (Type.Int, Mem x) => 
	       (Type.IntRef,
               code @ [Mips.LW (place,x,"0")])  
	  | (Type.Char, Mem x) =>
               (Type.CharRef,
                code @ [Mips.LW (place,x,"0")])
	  | _ => raise Error ("LV do not exists",(0,0))
        end
    | S100.Assign (lval,e,p) =>
        let
          val t = "_assign_"^newName()
          val (code0,ty,loc) = compileLval lval vtable ftable
          val (_,code1) = compileExp e vtable ftable t
        in
	  (* the different cases asign tolerates *)
          case (ty,loc) of
            (typ, Reg x) =>
              (typ,
               code0 @ code1 @ [Mips.MOVE (x,t), Mips.MOVE (place,t)])
          | (Type.Int, Mem x) =>
               (Type.IntRef,
		code0 @ code1 @ [Mips.SW (t,x,"0")]) 
          | (Type.Char, Mem x) =>
            (Type.CharRef,
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
	      (* regular add *)
	      (Type.Int, Type.Int) =>
	            (Type.Int, code1 @ code2 @ [Mips.ADD (place,t1,t2)])
	    (* multiply parameter with 8, and add mem adress to int *)
	    | (Type.IntRef, Type.Int) => 
  	            (Type.IntRef, code1 @ code2 @ [Mips.SLL(t2,t2,"3"), Mips.ADD (place,t1,t2)]) 
	    | (Type.Int, Type.IntRef) => 
  	            (Type.IntRef, code1 @ code2 @ [Mips.SLL(t2,t2,"3"), Mips.ADD (place,t1,t2)]) 
	    | (Type.CharRef, Type.Int) =>
	            (Type.CharRef, code1 @ code2 @ [Mips.SLL (t2,t2,"3"), Mips.ADD (place,t1,t2)])
	    | (Type.Int, Type.CharRef) =>
	            (Type.CharRef, code1 @ code2 @ [Mips.SLL (t2,t2,"3"), Mips.ADD (place,t1,t2)])
	    | _ => raise Error ("Mismatch in expressions",pos)
      end
    | S100.Minus (e1,e2,pos) =>
      let
	    val t1 = "_minus1_"^newName()
	    val t2 = "_minus2_"^newName()
            val (ty1,code1) = compileExp e1 vtable ftable t1
            val (ty2,code2) = compileExp e2 vtable ftable t2
      in
	  case (ty1,ty2) of
	      (* regular minus *)
	      (Type.Int, Type.Int) => (Type.Int, code1 @ code2 @ [Mips.SUB (place,t1,t2)])
	    | (Type.IntRef, Type.Int) => 
  	            (Type.IntRef, code1 @ code2 @ [Mips.SLL(t2,t2,"3"), Mips.SUB (place,t1,t2)]) 
	    (* multiply by 8, and substract paremter from mem adress *)
	    | (Type.CharRef, Type.Int) =>
	            (Type.CharRef, code1 @ code2 @ [Mips.SLL (t2,t2,"3"), Mips.SUB (place,t1,t2)])
	    | (Type.IntRef, Type.IntRef) =>
	            (Type.Int, code1 @ code2 @ [Mips.SUB (place,t1,t2)])
	    | (Type.CharRef, Type.CharRef) =>
	            (Type.Int, code1 @ code2 @ [Mips.SUB (place,t1,t2)])
	    | _ => raise Error ("Mismatch in expressions",pos)

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
    | S100.Equal (e1,e2,p) => 
      let
	  val t1 = "_less1_"^newName()
	  val t2 = "_less2_"^newName()
	  val l1 = "_eq_"^newName()
	  val l2 = "_eq_end_"^newName()
	  val (_,code1) = compileExp e1 vtable ftable t1
	  val (_,code2) = compileExp e2 vtable ftable t2
      in
	  (* of theyre not equal, just to label that sets "label" to 0, otherwise
	     just to label that sets "label" to one. jump out in the end *)
	  (Type.Int, code1 @ code2 @ [Mips.BNE(t1,t2,l1),
				      Mips.ADDI(place,"0","1"), Mips.J(l2),
				      Mips.LABEL l1, Mips.ADDI(place,"0","0"),
				      Mips.LABEL l2])
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
	     (* force type to be referece *)
	     SOME (ty,y) => ([], (if ty = Type.Int then Type.IntRef else Type.CharRef), Reg y)
	   | NONE => raise Error("Unkown reference "^x,p))
    | S100.Index (x,e,p) => 
        (case lookup x vtable of
           SOME (ty,y) => 
             let
               val t = "_index1_"^newName()
               val code1 = #2 (compileExp e vtable ftable t)
	       (* multiply by 4 and add offset *)
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
	      (* if statement constructed with block *)          
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
	  (* built in functions *)
	  Type.getFuns funs [("walloc",([Type.Int],Type.IntRef)),
			     ("balloc",([Type.Int],Type.CharRef)),
                             ("getint",([],Type.Int)),
			     ("putint",([Type.Int],Type.Int)),
		             ("putstring",([Type.CharRef],Type.CharRef)),
			     ("getstring",([Type.Int],Type.CharRef))]

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

     Mips.LABEL "walloc",
     Mips.SLL("2","2","2"),       (* mult by 4 *)   
     Mips.ADDI("4","2","0"),      (* add parameter to input *) 
     Mips.LI("2","9"),            (* syscall sbrk *)
     Mips.SYSCALL,                (* execute *)
     Mips.JR (RA,[]),

     Mips.LABEL "balloc", 
     Mips.SLL("2","2","2"),       (* mult by 4 *)
     Mips.ADDI("4","2","0"),      (* add parameter to input *)
     Mips.LI("2","9"),            (* syscall sbrk *)
     Mips.SYSCALL,                (* execute *)
     Mips.JR (RA,[]),


     Mips.LABEL "getstring",      (* getstring *)
     Mips.ADDI(HP,HP,"8"),        (* space on heap pointer *)
     Mips.ADDI ("5","2","0"),     (* argument 1 to reg 5 *)  
     Mips.ADDI ("4",HP,"0"),      (* argument 2 to reg 4 *)    
     Mips.LI ("2","8"),           (* init function read_string *)
     Mips.SYSCALL,                (* call function *)
     Mips.ADDI("2",HP,"0"),       (* add result to output reg 2 *)    
     Mips.JR (RA, []),

     Mips.LABEL "putstring", 
     Mips.ADDI(SP,SP,"-8"),       (* make space on stack pointer *)
     Mips.SW ("2",SP,"0"),        (* save used registers *)
     Mips.SW ("4",SP,"4"),        
     Mips.MOVE ("4","2"),         (* put argument *)       

     Mips.LABEL "put_char_loop",  
          Mips.LI ("2","4"),      (* print reg 2 *)
          Mips.SYSCALL,           (* write  *)
          Mips.LW("8","4","0"),   (* load the adress in to temp reg 8 *)
          Mips.ADDI("4","4","4"), (* increment *)
     
     Mips.BNE("8","0","put_char_loop"), (* if temp reg 8 = 0 end, else print next word *)
     
         Mips.LI ("2","4"),       (* write new line syscall *)
         Mips.LA("4","_cr_"),
         Mips.SYSCALL,            (* write CR *)

	 Mips.LW ("2",SP,"0"),    (* reload used registers *)
	 Mips.LW ("4",SP,"4"),
	 Mips.ADDI(SP,SP,"8"),

     Mips.JR (RA,[]), (* end putstring *)
         
     Mips.LABEL "putint",     
     Mips.ADDI(SP,SP,"-8"),        (* stack pointer space *)
     Mips.SW ("2",SP,"0"),         (* save used registers *)
     Mips.SW ("4",SP,"4"),         
     Mips.MOVE ("4","2"),          (* put argument *)
     Mips.LI ("2","1"),            (* write_int syscall *)
     Mips.SYSCALL,
     Mips.LI ("2","4"),            (* writestring syscall *)
     Mips.LA("4","_cr_"),
     Mips.SYSCALL,                 (* write CR *)
     Mips.LW ("2",SP,"0"),         (* reload used registers *)
     Mips.LW ("4",SP,"4"),
     Mips.ADDI(SP,SP,"8"),
     Mips.JR (RA,[]),

     Mips.LABEL "getint",          (* getint function *)
     Mips.LI ("2","5"),            (* read_int syscall *)
     Mips.SYSCALL,                 (* call *)
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
