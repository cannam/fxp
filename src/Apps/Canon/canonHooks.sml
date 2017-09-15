functor CanonHooks (structure ParserOptions : ParserOptions) =
   struct
      structure CanonOutput = CanonOutput (structure ParserOptions = ParserOptions)
      open 
	 Base Dtd
	 CanonEncode CanonOptions CanonOutput

      datatype Where = SOMEWHERE | SUBSET | CONTENT of int | REFERENCE of Where

      type AppData  = Dtd * OS.Process.status * CanonEncode.File * Where
      type AppFinal = OS.Process.status

      fun canonStart dtd = (dtd,OS.Process.success,CanonEncode.noFile,SOMEWHERE)

      fun hookXml ((dtd,status,_,wher),_) = (dtd,status,openFile(),wher)
      fun hookFinish (a as (_,status,f,_)) = status before closeFile f
	 
      fun hookError   ((dtd,status,f,wher),err) = (dtd,OS.Process.failure,f,wher) 
	 before printError err
      fun hookWarning (a,warn) = a before printWarning warn

      fun hookProcInst (a as (dtd,status,f,wher),pi) = 
	 case wher 
	   of REFERENCE _ => a
	    | _ => (dtd,status,putProcInst(f,pi),wher)
      fun hookWhite (a as (dtd,status,f,wher),ws) = 
	 case wher 
	   of CONTENT _ => (dtd,status,putDataVector(f,ws),wher)
	    | _ => a
	      
      fun hookDecl (a,_) = a
      fun hookComment (a,_) = a

      fun hookStartTag (a as (dtd,status,f,wher),stag) =
	 case wher
	   of CONTENT level => (dtd,status,putStartTag dtd (f,stag),
				CONTENT(if #5 stag then level else level+1))
	    | SOMEWHERE => (dtd,status,putStartTag dtd (f,stag),
			    if #5 stag then wher else CONTENT 1)
	    | _ => a
      fun hookEndTag (a as (dtd,status,f,wher),etag) =
	 case wher
	   of CONTENT level => (dtd,status,putEndTag dtd (f,etag), 
				if level>1 then CONTENT(level-1) else SOMEWHERE)
	    | _ => a

      fun hookCData (a as (dtd,status,f,wher),(_,text)) = 
	 case wher
	   of CONTENT _ => (dtd,status,putDataVector(f,text),wher)
	    | _ => a
      fun hookData (a,(pp,text,ignore)) = hookCData(a,(pp,text))
      fun hookCharRef (a,(pp,ch,_)) = hookCData(a,(pp,Vector.fromList [ch]))

      fun hookGenRef (a as (dtd,status,f,wher),(_,_,_,included)) = 
	 case wher
	   of REFERENCE _ => if included then (dtd,status,f,REFERENCE wher) else a
	    | _ => a
      fun hookParRef (a as (dtd,status,f,wher),(_,_,_,included)) = 
	 if included then (dtd,status,f,REFERENCE wher) else a
      fun hookEntEnd (a as (dtd,status,f,wher),_) = 
	 case wher 
	   of REFERENCE wher1 => (dtd,status,f,wher1)
	    | _ => a

      fun hookDocType (a,_) = a
      fun hookSubset (a as (dtd,status,f,wher),_) = 
	 case wher 
	   of SOMEWHERE => (dtd,status,f,SUBSET) 
	    | _ => a
      fun hookExtSubset (a as (dtd,status,f,wher),_) = 
	 case wher 
	   of SOMEWHERE => (dtd,status,f,SUBSET)
	    | _ => a
      fun hookEndDtd(a as (dtd,status,f,wher),_) =
	 case wher 
	   of SUBSET => (dtd,status,f,SOMEWHERE) 
	    | REFERENCE wher => (dtd,status,f,wher)
	    | _ => a
   end
