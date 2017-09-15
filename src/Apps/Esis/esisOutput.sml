signature EsisOutput =
   sig
      type Dtd

      val printError   : Errors.Position * Errors.Error -> unit
      val printWarning : Errors.Position * Errors.Warning -> unit

      val outDocument  : Dtd -> EsisData.Content -> unit
   end

functor EsisOutput (structure Resolve       : Resolve
		    structure ParserOptions : ParserOptions) : EsisOutput =
   struct
      open 
	 Base Dtd Errors HookData IntSets ParserOptions UniChar UniClasses Uri
	 EsisData EsisOptions Resolve

      fun printError(pos,err) = if !O_SILENT then () else TextIO.output
	 (!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
	  (Position2String pos
           ::(if isFatalError err then "Fatal error:" else "Error:")
           ::errorMessage err))
      fun printWarning(pos,warn) = if !O_SILENT then () else TextIO.output
	 (!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
	  (Position2String pos^" Warning:"::warningMessage warn))

      val outFile = ref TextIO.stdOut

      fun openOutFile() = if !O_OUTPUT_FILE="-" then outFile := TextIO.stdOut
			  else outFile := TextIO.openOut(!O_OUTPUT_FILE)
      fun closeOutFile() = if !O_OUTPUT_FILE="-" then ()
			   else TextIO.closeOut(!outFile)

      fun out str = TextIO.output(!outFile,str)
      fun out1 c = TextIO.output1(!outFile,c)
      val outList = app out
      fun outNewLine() = out1 #"\n"
      fun outChar c =  
	 case c 
	   of 0wx09 => out "\\t"
	    | 0wx0A => out "\\n"
	    | 0wx5C => out "\\\\"
	    | _ => if Chars.andb(c,0wx7F)>=0wx20 andalso !O_NON_DIRECT>c 
		      then out1(Char2char c)
		   else out ("\\U+"^Chars.toString c^";")
      val outData = app outChar 
      val outVector = Vector.app outChar

      fun outExtId putFile (id as EXTID(pub,sys)) = 
	 let val _ = case pub 
		       of NONE => ()
			| SOME(x,_) => out ("\np"^x)
	     val _ = case sys
			  of NONE => ()
			   | SOME(b,x,_) => (out "\ns"; out (Uri2String x))
	     val _ = if putFile 
			then let val uri = resolveExtId id
			     in (out "\nf<OSFILE>"; out (Uri2String uri))
			     end
			  handle NoSuchFile _ => ()
		     else ()
	 in ()
	 end
      fun defNot dtd ns (idx,name) = 
	 if inIntSet(idx,ns) then ns
	 else let val _ = case getNotation dtd idx
			    of NONE => ()
			     | SOME extId => outExtId false extId
		  val _ = out "\nN"
		  val _ = outData name
	      in addIntSet(idx,ns)
	      end
      fun defNotation dtd (es,ns) name = 
	 (es,defNot dtd ns (AttNot2Index dtd name,name))
      fun defEntity dtd (sets as (es,ns)) name = 
	 let val idx = GenEnt2Index dtd name
	 in if inIntSet(idx,es) then sets
	    else let val (ent,_) = getGenEnt dtd idx
		     val ns1 = case ent
			       of GE_UNPARSED(extId,nidx,_) => 
				  let 
				     val not = Index2AttNot dtd nidx
				     val ns1 = defNot dtd ns (nidx,not)
				     val _ = outExtId true extId
				     val _ = out "\nE"
				     val _ = outData name
				     val _ = out " NDATA "
				     val _ = outData not
				  in ns1
				  end
				| _ => ns 
		 in (addIntSet(idx,es),ns1)
		 end
	 end
      fun defEntities dtd sets av = foldl 
	 (fn (ent,sets) => defEntity dtd sets ent)
	 sets (UtilList.split (fn c => isS c) av)
      fun preAttSpec dtd sets (at,av) =
	 case at
	   of AT_CDATA => (sets," CDATA ")
	    | AT_NOTATION _ => (defNotation dtd sets (Vector2Data av)," NOTATION ")
	    | AT_ENTITY => (defEntity dtd sets (Vector2Data av)," ENTITY ")
	    | AT_ENTITIES => (defEntities dtd sets (Vector2Data av)," ENTITY ")
	    | _ => (sets," TOKEN ")
      fun outAttSpec dtd sets (att,at,av) =
	 let 
	    val (sets1,typ) = preAttSpec dtd sets (at,av)
	    val _ = out "\nA"
	    val _ = outData att
	    val  _ = out typ
	    val _ = outVector av
	 in sets1
	 end
      fun outImplied att =
	 let 
	    val _ = out "\nA"
	    val _ = outData att
	 in out " IMPLIED"
	 end
      fun outAttVal dtd specs sets (idx,at,_,_) =
	 let val att = Index2AttNot dtd idx
	 in case List.find (fn(i,_) => i=idx) specs
	      of NONE => sets before outImplied att
	       | SOME(_,AP_MISSING) => sets before outImplied att 
	       | SOME(_,AP_IMPLIED) => sets before outImplied att 
	       | SOME(_,AP_DEFAULT(_,av,_)) => outAttSpec dtd sets (att,at,av)
	       | SOME(_,AP_PRESENT(_,av,_)) => outAttSpec dtd sets (att,at,av)
	 end
      fun outAttNonVal dtd defs sets (idx,ap) =
	 let fun outPresent av =
	    let 
	       val att = Index2AttNot dtd idx
	       val at = case List.find (fn(i,_,_,_) => i=idx) defs
			  of NONE => AT_CDATA
			   | SOME(_,at,_,_) => at
	    in outAttSpec dtd sets (att,at,av)
	    end
	 in case ap
	      of AP_MISSING => sets
	       | AP_IMPLIED => sets
	       | AP_DEFAULT(_,av,_) => outPresent av
	       | AP_PRESENT(_,av,_) => outPresent av
	 end
      fun outStartTag dtd sets (idx,elem,atts) = 
	 let
	    val defs = case #atts(getElement dtd idx)
			 of NONE => nil
			  | SOME(defs,_) => defs
	    val sets1 = 
	       if !O_VALIDATE andalso hasDtd dtd
		  then foldl 
		     (fn (def,sets) => outAttVal dtd atts sets def) 
		     sets defs
	       else foldl 
		  (fn (spec,sets) => outAttNonVal dtd defs sets spec) 
		  sets atts
	    val _ = out "\n("
	    val _ = outData elem
	 in sets1
	 end
      fun outEndTag elem = 
	 let val _ = out "\n)"
	 in outData elem
	 end
      fun outCData(prefix,text) = 
	 let val _ = out prefix
	 in outVector text
	 end
      fun outPi(target,text) =
	 let 
	    val _ = out "\n?"
	    val _ = outData target
	    val _ = out " "
	 in outVector text
	 end
      fun outContent dtd sets items = 
	 let val (sets1,_) = foldl 
	    (fn (item,(sets,prefix)) => case item
					  of PI pi => (sets,"\n-") before outPi pi
					   | DATA text => (sets,"") before outCData(prefix,text)
					   | ELEM el => (outElem dtd sets el,"\n-"))  
	    (sets,"\n-") items
	 in sets1
	 end
      and outElem dtd sets ((idx,atts),content) =
	 let
	    val elem = Index2Element dtd idx
	    val sets1 = outStartTag dtd sets (idx,elem,atts)
	    val sets2 = outContent dtd sets1 content
	    val _ = outEndTag elem
	 in sets2 
	 end

      fun outDocument dtd content =
	 let 
	    val _ = openOutFile()
	    val _ = ignore (outContent dtd (emptyIntSet,emptyIntSet) content)
	       handle IO.Io _ => ()
	    val _ = outNewLine()
	    val _ = closeOutFile()
	 in ()
	 end 
      handle IO.Io _ => ()
   end
