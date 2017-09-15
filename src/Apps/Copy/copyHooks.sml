










functor CopyHooks (structure ParserOptions : ParserOptions) =
   struct
      structure CopyOutput = CopyOutput (structure ParserOptions = ParserOptions)

      open 
	 Base Dtd Encoding
	 CopyOptions CopyOutput

      datatype Where = SOMEWHERE | SUBSET | CONTENT of int | REFERENCE of Where

      type AppData  = OS.Process.status * File * Dtd * Where
      type AppFinal = OS.Process.status

      fun copyStart dtd = (OS.Process.success,noFile,dtd,SOMEWHERE)

      fun hookXml ((status,_,dtd,wher),(fname,enc,xmlDecl)) = 
	 let val (vers,_,stand) = case xmlDecl 
				      of SOME ves => ves
				       | NONE => (NONE,NONE,NONE)
	     val (outEnc,outEncName) = case !O_OUTPUT_ENCODING 
					 of NONE => (enc,encodingName enc)
					  | SOME x => (NOENC,x)
	     val f = openFile(!O_OUTPUT_FILE,outEnc,outEncName)
	 in (status,putXmlDecl(f,(vers,SOME outEncName,stand)),dtd,wher)
	 end

      fun hookFinish (a as (status,f,_,_)) = status before closeFile f
	 
      fun hookError ((status,f,dtd,wher),err) = (OS.Process.failure,f,dtd,wher) 
	 before printError err
      fun hookWarning (a,warn) = a before printWarning warn

      fun hookProcInst (a as (status,f,dtd,wher),pi) = 
	 case wher 
	   of REFERENCE _ => a
	    | _ => (status,putProcInst(f,pi),dtd,wher)
      fun hookComment (a as (status,f,dtd,wher),com) = 
	 case wher 
	   of REFERENCE _ => a
	    | _ => (status,putComment(f,com),dtd,wher)
      fun hookWhite (a as (status,f,dtd,wher),ws) = 
	 case wher 
	   of REFERENCE _ => a
	    | _ => (status,putVector(f,ws),dtd,wher)

      fun hookDecl (a as (status,f,dtd,wher),decl) = 
	 case wher 
	   of SUBSET => (status,putDecl(f,dtd,decl),dtd,wher) 
	    | _ => a

      fun hookStartTag (a as (status,f,dtd,wher),stag) =
	 case wher
	   of CONTENT level => let val f1 = putStartTag(f,dtd,stag)
				   val level1 = if #5 stag then level else level+1
			       in (status,f1,dtd,CONTENT level1)
			       end
	    | SOMEWHERE => let val f1 = putStartTag(f,dtd,stag)
			   in if #5 stag then (status,putNl f1,dtd,wher)
			      else (status,f1,dtd,CONTENT 1)
			   end
	    | _ => a
      fun hookEndTag (a as (status,f,dtd,wher),etag) =
	 case wher
	   of CONTENT level => let val f1 = putEndTag(f,dtd,etag) 
				   val (f2,wher1) = if level>1 then (f1,CONTENT(level-1))
						    else (f1,SOMEWHERE)
			       in (status,f2,dtd,wher1)
			       end
	    | _ => a

      fun hookData (a as (status,f,dtd,wher),(_,text,_)) = 
	 case wher
	   of CONTENT _ => (status,putDataVector(f,text),dtd,wher)
	    | _ => a
      fun hookCData (a as (status,f,dtd,wher),(_,text)) = 
	 case wher
	   of CONTENT _ => (status,putCData(f,text),dtd,wher)
	    | _ => a

      fun hookCharRef (a as (status,f,dtd,wher),(_,ch,cv)) = 
	 case wher
	   of CONTENT _ => 
	      let val f1 = if !O_EXPAND_CONT_CHAR 
			      then putDataChar(f,ch) else putVector(f,cv)
	      in (status,f1,dtd,wher)
	      end
	    | _ => a
      fun hookGenRef (a as (status,f,dtd,wher),(_,idx,ent,included)) = 
	 case wher
	   of CONTENT l => 
	      if not included then (status,putGenRef(f,Index2GenEnt dtd idx),dtd,wher)
	      else if (if isExtGen ent then !O_EXPAND_CONT_EXT else !O_EXPAND_CONT_INT) then a
		   else (status,putGenRef(f,Index2GenEnt dtd idx),dtd,REFERENCE wher)
	    | REFERENCE _ => if included then (status,f,dtd,REFERENCE wher) else a
	    | _ => a
      fun hookParRef (a as (status,f,dtd,wher),(_,idx,ent,included)) = 
	 case wher
	   of SUBSET =>
	      if not included then (status,putNl(putParRef(f,Index2ParEnt dtd idx)),dtd,wher)
	      else if (if isExtPar ent then !O_EXPAND_SUBSET_EXT else !O_EXPAND_SUBSET_INT) then a
		   else (status,putNl(putParRef(f,Index2ParEnt dtd idx)),dtd,REFERENCE wher)
	    | REFERENCE _ => if included then (status,f,dtd,REFERENCE wher) else a
	    | _ => if included then (status,f,dtd,REFERENCE wher) else a
      fun hookEntEnd (a as (status,f,dtd,wher),_) = 
	 case wher 
	   of REFERENCE wher1 => (status,f,dtd,wher1)
	    | _ => a

      fun hookDocType (a as (status,f,dtd,wher),(didx,extId)) = 
	 case wher 
	   of SOMEWHERE => 
	      let 
		 val f1 = putData(f,startDtd)
		 val f2 = putData(f1,Index2Element dtd didx)
		 val f3 = if !O_INCLUDE_EXT_SUBSET then f2
			  else case extId
				 of NONE => f2
				  | SOME x => putExtId(putBlank f2,x)
	      in (status,f1,dtd,wher)
	      end
	    | _ => a 
      fun hookSubset (a as (status,f,dtd,wher),_) = 
	 case wher 
	   of SOMEWHERE => (status,putData(f,[0wx20,0wx5B]),dtd,SUBSET) (* " [" *)
	    | _ => a
      fun hookExtSubset (a as (status,f,dtd,wher),_) = 
	 if !O_INCLUDE_EXT_SUBSET 
	    then case wher 
		   of SOMEWHERE => (status,putData(f,[0wx20,0wx5B,0wx0A]),dtd,SUBSET) (* " [\n" *) 
		    | SUBSET => (status,putChar(f,0wx0A),dtd,SUBSET) 
		    | _ => a
	 else case wher 
		of SOMEWHERE => (status,f,dtd,REFERENCE wher) 
		 | SUBSET => (status,putChar(f,0wx5D),dtd,REFERENCE SOMEWHERE) (* #"]" *)
		 | _ => a
      fun hookEndDtd(a as (status,f,dtd,wher),_) =
	 case wher 
	   of SOMEWHERE => (status,putData(f,endDecl),dtd,wher) 
	    | SUBSET => (status,putData(putChar(f,0wx5D),endDecl),dtd,SOMEWHERE) 
	    | REFERENCE wher => (status,putData(f,endDecl),dtd,wher)
	    | _ => a
   end
