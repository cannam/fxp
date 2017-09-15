signature CanonOutput =
   sig
      type Dtd 

      val printError   : Errors.Position * Errors.Error -> unit
      val printWarning : Errors.Position * Errors.Warning -> unit

      val putProcInst  : CanonEncode.File * HookData.ProcInstInfo -> CanonEncode.File
      val putStartTag  : Dtd -> CanonEncode.File * HookData.StartTagInfo -> CanonEncode.File
      val putEndTag    : Dtd -> CanonEncode.File * HookData.EndTagInfo -> CanonEncode.File
   end

functor CanonOutput (structure ParserOptions : ParserOptions) : CanonOutput =
   struct
      open 
	 Base UniChar DfaData Dtd Errors ParserOptions 
	 CanonEncode CanonOptions HookData 

      fun printError(pos,err) = if !O_SILENT then () else TextIO.output
	 (!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
	  (Position2String pos
           ::(if isFatalError err then "Fatal error:" else "Error:")
           ::errorMessage err))
      fun printWarning(pos,warn) = if !O_SILENT then () else TextIO.output
	 (!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
	  (Position2String pos^" Warning:"::warningMessage warn))

      fun putAttValue (f,cv) = 
	 let 
	    val f1 = putChar(f,0wx22)
	    val f2 = putDataVector(f1,cv)
	    val f3 = putChar(f2,0wx22)
	 in f3
	 end
	 
      fun putAttSpec(f,(att,cv)) = 
	 let 
	    val f1 = putBlank f
	    val f2 = putData(f1,att)
	    val f3 = putChar(f2,0wx3D)
	    val f4 = putAttValue(f3,cv)
	 in f4
	 end
      
      fun putEndTag dtd (f,(_,eidx,_)) = 
	 let 
	    val f1 = putData(f,[0wx3C,0wx2F]) (* "</" *)
	    val f2 = putData(f1,Index2Element dtd eidx)
	 in putChar(f2,0wx3E) (* #">" *)
	 end
      fun putStartTag dtd (f,(pp,eidx,atts,_,mt)) = 
	 let 
	    val f1 = putChar(f,0wx3C) (* #"<" *)
	    val f2 = putData(f1,Index2Element dtd eidx)
	    val atts1 = List.mapPartial 
	       (fn (aidx,ap,_) => 
		case ap 
		  of AP_PRESENT(_,cv,_) => SOME(Index2AttNot dtd aidx,cv)
		   | AP_DEFAULT(_,cv,_) => SOME(Index2AttNot dtd aidx,cv)
		   | _ => NONE) atts
	    val atts2 = UtilList.sort (fn ((a1,_),(a2,_)) => compareData (a1,a2)) atts1
	    val f3 = foldl (fn (spec,f) => putAttSpec (f,spec)) f2 atts2
	    val f4 = putChar(f3,0wx3E) (* #">" *)
	 in if mt then putEndTag dtd (f4,(pp,eidx,NONE)) else f4
	 end

      fun putProcInst(f,(_,target,_,text)) = 
	 let 
	    val f1 = putData(f,[0wx3c,0wx3f])
	    val f2 = putData(f1,target)
	    val f3 = putBlank f2
	    val f4 = putVector(f3 ,text)
	 in putData(f3,[0wx3f,0wx3e])
	 end
   end
