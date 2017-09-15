functor VizHooks ( structure Dtd : Dtd ) =
   struct
      open Base Dtd Errors HookData IgnoreHooks UniChar UtilString VizOptions

      val THIS_MODULE = "VizHooks"
	 
      datatype DataItem = 
	 CREF of HookData.CharRefInfo
       | DATA of HookData.DataInfo
       | CDATA of HookData.CDataInfo
      type StackItem = HookData.StartTagInfo * int * string * string list
      type Current = int * string * string list * (string * DataItem) list
      type AppData = Dtd * TextIO.outstream * Current * StackItem list 
      type AppFinal = unit
      fun vizStart dtd = (dtd,TextIO.stdOut,(0,"",nil,nil),nil)

      fun inContent (_,_,_,stack) = not (null stack)

      fun Char2String c =  
	 case c 
	   of 0wx09 => "\t"
	    | 0wx0A => "\n"
	    | 0wx22 => "\\\""
	    | 0wx5C => "\\\\"
	    | _ => if Chars.andb(c,0wx7F)>=0wx20 andalso c<0wx100 
		      then String.implode [Char2char c]
		   else "\\U+"^Chars.toString c^";"
      fun Data2String cs = String.concat (map Char2String cs)
      fun Vector2String cv = String.concat
	 (Vector.foldr (fn (c,strs) => Char2String c::strs) nil cv)
	 
      fun printError(pos,err) = if !O_SILENT then () else TextIO.output
	 (!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
	  (Position2String pos
           ::(if isFatalError err then "Fatal error:" else "Error:")
           ::errorMessage err))
      fun printWarning(pos,warn) = if !O_SILENT then () else TextIO.output
	 (!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
	  (Position2String pos^" Warning:"::warningMessage warn))

      val (piColor ,piRgb ) = ("51","255 208 192")
      val (comColor,comRgb) = ("52","255 255 192")
      val (txtColor,txtRgb) = ("53","255 244 232")
      val (elColor ,elRgb ) = ("54","208 232 255")
      val (valColor,valRgb) = ("55",txtRgb)
      val (attColor,attRgb) = ("56","208 255 208")
      val (misColor,misRgb) = ("57","224 0   0  ")
      val (typColor,typRgb) = ("58","0   144 0  ")
	 
      fun out(f,s) = f before TextIO.output(f,s)
      fun outNl f = out(f,"\n") 
      fun outLine (f,line) = outNl(out(f,line))
      fun outLines (f,lines) = foldl (fn (line,f) => outLine(f,line)) f lines 

      fun outPiNode (f,i,title,((pos,_),target,_,text)) =
	 let 
	    val f1 = outLines 
	       (f,["",
		   "  node: {",
		   "    title: \""^title^"\"",
		   "    label: \"<?"^Data2String target^" "^Vector2String text^"?>\"",
		   "    color: "^piColor,
		   "    info1: \""^Position2String pos^"\"",
		   "    horizontal_order: "^Int.toString i,
		   "  }"
		   ])
	 in f1
	 end

      fun outComNode (f,i,title,((pos,_),text)) =
	 let 
	    val f1 = outLines 
	       (f,["",
		   "  node: {",
		   "    title: \""^title^"\"",
		   "    label: \"<!--"^Vector2String text^"-->\"",
		   "    color: "^comColor,
		   "    info1: \""^Position2String pos^"\"",
		   "    horizontal_order: "^Int.toString i,
		   "  }"
		   ])
	 in f1
	 end

      fun outDataNode (f,path,(i,item)) =
	 let 
	    val title = path^i
	    val (pos,label,text) = 
	       case item 
		 of DATA((pos,_),cv,_) => let val str = Vector2String cv
					  in (pos,str,str)
					  end
		  | CDATA((pos,_),cv) => let val str = Vector2String cv
					 in (pos,"<![CDATA["^str^"]]>",str)
					 end
		  | CREF((pos,_),c,cv) => (pos,Vector2String cv,Char2String c)
	    val f1 = outLines 
	       (f,["    node: {",
		   "      title: \""^title^"\"",
		   "      label: \""^label^"\"",
		   "      color: "^txtColor,
		   "      info1: \""^Position2String pos^"\"",
		   "      horizontal_order: "^i,
		   "    }"
		   ])
	 in (f1,pos,title,text)
	 end

      fun outDataGraph (f,i,path,items) = 
	 if null items then NONE
	 else let val i1 = i+1
		  val iS = Int.toString i1
		  val title = path^iS
		  val f1 = outLines 
		     (f,["",
			 "  graph: {",
			 "    title  : \""^title^"\"",
			 "    color  : "^txtColor,
			 "    folding: 1",
			 "    horizontal_order: "^iS
			 ])
		  val (f2,pos,titles,texts) =
		     foldl (fn (item,(f,_,titles,texts)) 
			    => let val (f1,pos,title,text) = outDataNode (f,path,item)
			       in (f1,pos,title::titles,text::texts)
			       end) (f1,nullPosition,nil,nil) items
		  val label = String.concat texts
		  val (firstTitle,f3) = foldr 
		     (fn (title,(next,f)) => 
		      case next
			of NONE => (SOME title,f)
			 | SOME nextTitle => 
			   (SOME title,outLines
			    (f,["",
				"    nearedge: {",
				"      sourcename: \""^title^"\"",
				"      targetname:\""^nextTitle^"\"",
				"    }"])))
		     (NONE,f2) titles
		  val f4 = outLines
		     (f3,["    label: \""^label^"\"",
			  "    info1: \""^Position2String pos^"\"",
			  "    }"
			  ])
	      in SOME (f4,i1,valOf firstTitle)
	      end

      fun outData (a as (dtd,f,(i,path,titles,data),stack)) =
	 case outDataGraph (f,i,path,data)
	   of NONE => a
	    | SOME(f1,i1,title) => (dtd,f1,(i1,path,title::titles,nil),stack)

      fun attValType dtd avOpt = 
	 case avOpt 
	   of NONE => NONE
	    | SOME av => SOME
	      (case av
		 of AV_CDATA _ => "CDATA"
		  | AV_NMTOKEN _ => "NMTOKEN"
		  | AV_NMTOKENS _ => "NMTOKENS"
		  | AV_ENTITY _ => "ENTITY"
		  | AV_ENTITIES _ => "ENTITIES"
		  | AV_ID _ => "ID"
		  | AV_IDREF _=> "IDREF"
		  | AV_IDREFS _=> "IDREFS"
		  | AV_GROUP(is,_) => List2xString 
		    ("(","|",")") (Data2String o (Index2AttNot dtd)) is
		  | AV_NOTATION(is,_) => List2xString 
		    ("NOTATION(","|",")") (Data2String o (Index2AttNot dtd)) is)
			  
      fun outAttNode dtd (f,path,(i,ap,_)) =
	 let 
	    val iS = Int.toString i
	    val attTitle = path^iS
	    val attLabel = Data2String(Index2AttNot dtd i)
	    val valTitle = attTitle^".0"
	    val (valLabel,specText,typInfo) = 
	       case ap 
		 of AP_IMPLIED => ("\f"^typColor^"#IMPLIED","",NONE)
		  | AP_MISSING => ("\f"^misColor^"MISSING","",NONE)
		  | AP_DEFAULT(_,cv,av) => 
		    let val str = Vector2String cv
		    in ("\f"^typColor^"#DEFAULT:\f31"^str,"",attValType dtd av)
		    end
		  | AP_PRESENT(_,cv,av) => 
		    let val str = Vector2String cv
		    in (str," "^attLabel^"=\\\""^str^"\\\"",attValType dtd av)
		    end
	    val f1 = outLines 
	       (f,["",
		   "    node: {",
		   "      title: \""^attTitle^"\"",
		   "      label: \""^attLabel^"\"",
		   "      color: "^attColor,
		   "      horizontal_order: 0",
		   "    }",
		   "    node: {",
		   "      title: \""^valTitle^"\"",
		   "      label: \""^valLabel^"\""
		   ])
	    val f2 = case typInfo
		       of NONE => f1
			| SOME info => outLine(f1,"      info2: \""^info^"\"")
	    val f3 = outLines
	       (f2,["      color: "^valColor,
		    "    }",
		    "    edge: {",
		    "      sourcename: \""^attTitle^"\"",
		    "      targetname: \""^valTitle^"\"",
		    "    }"
		    ])
	 in (f3,attTitle,specText)
	 end

      fun outElemGraph dtd (f,i,path,children,((pos,_),el,atts,_,_)) = 
	 let 
	    val iS = Int.toString i
	    val title = path^iS
	    val nodeTitle = title^".0"
	    val nodeLabel = Data2String (Index2Element dtd el)
	    val attPath = nodeTitle^"."
	    val f1 = outLines(f,["  graph: {",
				 "    title  : \""^title^"\"",
				 "    color  : "^elColor,
				 "    folding: 1",
				 "    horizontal_order: "^iS,
				 "",
				 "    node: {",
				 "      title: \""^nodeTitle^"\"",
				 "      color: "^elColor,
				 "      horizontal_order: "^iS,
				 "      label: \""^nodeLabel^"\"",
				 "      info1: \""^Position2String pos^"\"",
				 "    }"
				 ])
	    val (f2,attTitles,attTexts) = foldl 
	       (fn (att,(f,titles,texts)) 
		=> let val (f1,title,text) = outAttNode dtd (f,attPath,att)
		   in (f1,title::titles,text::texts)
		   end)
	       (f1,nil,nil) atts

	    val f3 = foldr
		(fn (attTitle,f) => outLines(f,["",
						"  edge: {",
						"    sourcename: \""^nodeTitle^"\"",
						"    targetname:\""^attTitle^"\"",
						"  }"]))
		f2 attTitles
		
	    val attsText = String.concat attTexts
	    val label = nodeLabel^attsText
		
	    val f4 = outLines(f3,["    label: \""^label^"\"",
				  "    info1: \""^Position2String pos^"\"",
				  "  }"])
	    val f5 = foldr
	       (fn (child,f) => outLines(f,["",
					    "  edge: {",
					    "    sourcename: \""^title^"\"",
					    "    targetname:\""^child^"\"",
					    "  }"]))
	       f4 children
	 in (f5,title)
	 end

      fun outRoot (f,titles) = 
	 let val f1 = outLines 
	    (f,["",
		"  node: {",
		"    title : \"root\"",
		"    label : \"\"",
		"    width : 0",
		"    height: 0", 
		"  }"])
	     val f2 = 
		foldr (fn (title,f) => 
		       outLines (f,["",
				    "    edge: {",
				    "      sourcename: \"root\"",
				    "      targetname: \""^title^"\"",
				    "      linestyle : invisible",
				    "    }"])) f1 titles
	 in f2
	 end

      fun hookProcInst (a,pi) = 
	 let 
	    val (dtd,f,(i,path,titles,_),stack) = outData a
	    val i1 = i+1
	    val title = path^Int.toString i1
	    val f1 = outPiNode(f,i1,title,pi)
	 in 
	    (dtd,f1,(i1,path,title::titles,nil),stack)
	 end

      fun hookComment (a,com) = 
	 let
	    val (dtd,f,(i,path,titles,_),stack) = outData a
	    val i1 = i+1
	    val title = path^Int.toString i1
	    val f1 = outComNode(f,i1,title,com)
	 in 
	    (dtd,f1,(i1,path,title::titles,nil),stack)
	 end
      
      fun hookEndTag (a,_) = 
	 let val (dtd,f,(_,_,titles,data),stack) = outData a
	 in case stack 
	      of nil => a
	       | (stag,i,path,titles')::rest => 
		 let 
		    val (f1,title) = outElemGraph dtd (f,i,path,titles,stag)
		 in 
		    (dtd,f1,(i,path,title::titles',nil),rest)
		 end
	 end

      fun hookStartTag (a,stag as (_,_,_,_,mt)) = 
	 let 
	    val (dtd,f,(i,path,titles,data),stack) = outData a
	    val i1 = i+1
	    val title = path^Int.toString i1
	    val a1 = (dtd,f,(0,title^".",nil,nil),(stag,i1,path,titles)::stack)
	 in 
	    if mt then hookEndTag (a1,()) else a1
	 end

      fun hookData0 ((dtd,f,(i,path,titles,data),stack),item) =
	 let 
	    val i1 = i+1
	    val iS = Int.toString i1
	 in 
	    (dtd,f,(i1,path,titles,(iS,item)::data),stack)
	 end

      fun hookData (a,x) = hookData0(a,DATA x)
      fun hookCData (a,x) = hookData0(a,CDATA x)
      fun hookCharRef (a,x) = hookData0(a,CREF x)

      fun hookError   (a,pe) = a before printError pe
      fun hookWarning (a,pw) = a before printWarning pw 

      fun hookXml((dtd,_,_,_),_) = 
	 let val f = if !O_OUTPUT_FILE="-" then TextIO.stdOut
		     else TextIO.openOut (!O_OUTPUT_FILE)
	     val f1 = outLines(f,["graph: {",
				  "  layoutalgorithm: tree",
				  "  smanhattanedges: yes",
				  "  spreadlevel    : 1000",
				  "",
				  "  colorentry "^piColor^": "^piRgb,
				  "  colorentry "^comColor^": "^comRgb,
				  "  colorentry "^txtColor^": "^txtRgb,
				  "  colorentry "^elColor^": "^elRgb,
				  "  colorentry "^valColor^": "^valRgb,
				  "  colorentry "^attColor^": "^attRgb,
				  "  colorentry "^misColor^": "^misRgb,
				  "  colorentry "^typColor^": "^typRgb,
				  "",
				  "  infoname 1: \"Source Position\"",
				  "  infoname 2: \"Attribute Type\"",
				  "  infoname 3: \"\"",
				  "",
				  "  edge.arrowstyle   : none",
				  "  foldedge.thickness: 1"
				  ])
	 in 
	    (dtd,f1,(0,"",nil,nil),nil)
	 end

      fun hookFinish (_,f,(_,_,titles,_),_) = 
	 let 
	    val f1 = outRoot(f,titles)
	    val f2 = outLine(f1,"}")
	 in 
	    if !O_OUTPUT_FILE="-" then () else TextIO.closeOut f2
	 end
   end

functor VizHooks_ ( structure Dtd : Dtd ) = VizHooks ( structure Dtd = Dtd ) : Hooks
