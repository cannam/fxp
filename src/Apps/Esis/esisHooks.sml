functor EsisHooks (structure Resolve       : Resolve
		   structure ParserOptions : ParserOptions) =
   struct
      structure EsisOutput = EsisOutput (structure Resolve = Resolve
					 structure ParserOptions = ParserOptions) 
	 
      open EsisData EsisOutput IgnoreHooks

      type AppFinal = OS.Process.status
      type AppData  = Dtd * OS.Process.status * Content * (Stag * Content) list

      fun esisStart dtd = (dtd,OS.Process.success,nil,nil) : AppData

      fun hookError   ((dtd,status,items,stack),err) = 
	 (dtd,OS.Process.failure,items,stack) before printError err
      fun hookWarning (a,warn) = a before printWarning warn

      fun hookProcInst ((dtd,status,items,stack),(_,target,_,text)) = 
	 (dtd,status,PI(target,text)::items,stack)

      fun hookStartTag ((dtd,status,items,stack),(_,elem,atts,_,mt)) =
	 let val stag = (elem,map(fn(a,v,_) => (a,v)) atts)
	 in if mt then (dtd,status,ELEM(stag,nil)::items,stack) 
	    else (dtd,status,nil,(stag,items)::stack)
	 end
      fun hookEndTag ((dtd,status,items,(stag,items1)::stack),_) = 
	 (dtd,status,ELEM(stag,rev items)::items1,stack)
	| hookEndTag (a,_)  = a
	 
      fun hookData ((dtd,status,items,stack),(_,text,_)) = 
	 (dtd,status,DATA text::items,stack)
      fun hookCData ((dtd,status,items,stack),(_,text)) = 
	 (dtd,status,DATA text::items,stack)
      fun hookCharRef ((dtd,status,items,stack),(_,ch,_)) = 
	 (dtd,status,DATA (Vector.fromList [ch])::items,stack)

      fun hookXml (a,_) = a
      fun hookFinish (dtd,status,items,_) = status before outDocument dtd (rev items)
   end
