signature CopyOutput =
   sig
      type Dtd
      
      include CopyEncode

      val startDtd     : UniChar.Data
      val endDecl      : UniChar.Data

      val printError   : Errors.Position * Errors.Error -> unit
      val printWarning : Errors.Position * Errors.Warning -> unit

      val putDecl      : File * Dtd * HookData.DeclInfo -> File
      val putXmlDecl   : File * HookData.XmlDecl -> File
      val putProcInst  : File * HookData.ProcInstInfo -> File
      val putComment   : File * HookData.CommentInfo -> File
      val putStartTag  : File * Dtd * HookData.StartTagInfo -> File
      val putEndTag    : File * Dtd * HookData.EndTagInfo -> File
      val putCData     : File * UniChar.Vector -> File
      val putExtId     : File * Base.ExternalId -> File
   end

functor CopyOutput (structure ParserOptions : ParserOptions) : CopyOutput =
   struct
      structure CopyEncode = CopyEncode (structure ParserOptions = ParserOptions)

      open 
	 Base DfaData Dtd Errors ParserOptions UniChar Uri
	 CopyEncode CopyOptions HookData 

      fun printError(pos,err) = if !O_SILENT then () else TextIO.output
	 (!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
	  (Position2String pos
           ::(if isFatalError err then "Fatal error:" else "Error:")
           ::errorMessage err))
      fun printWarning(pos,warn) = if !O_SILENT then () else TextIO.output
	 (!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
	  (Position2String pos^" Warning:"::warningMessage warn))

      val ANY = String2Data "ANY"
      val ATTLIST = String2Data "<!ATTLIST "
      val CDATA = String2Data "CDATA"
      val CDATA_S = String2Data "<![CDATA["
      val ELEMENT = String2Data "<!ELEMENT "
      val EMPTY = String2Data "EMPTY"
      val ENCODING = String2Data " encoding="
      val ENTITY = String2Data "ENTITY"
      val ENTITY_G = String2Data "<!ENTITY "
      val ENTITY_P = String2Data "<!ENTITY % "
      val ENTITIES = String2Data "ENTITIES"
      val FIXED = String2Data "#FIXED "
      val ID = String2Data "ID"
      val IDREF = String2Data "IDREF"
      val IDREFS = String2Data "IDREFS"
      val IMPLIED = String2Data "#IMPLIED"
      val NMTOKEN = String2Data "NMTOKEN"
      val NMTOKENS = String2Data "NMTOKENS"
      val NO = String2Data "no"
      val NOTATION_D = String2Data "<!NOTATION "
      val NOTATION_L = String2Data "NOTATION ("
      val ONE = String2Data "1.0"
      val PCDATA = String2Data "(#PCDATA)"
      val PCDATA_L = String2Data "(#PCDATA"
      val PUBLIC = String2Data "PUBLIC "
      val REQUIRED = String2Data "#REQUIRED"
      val STANDALONE = String2Data " standalone="
      val SYSTEM = String2Data "SYSTEM "
      val XMLVERS = String2Data "<?xml version="
      val YES = String2Data "yes"
	 
      val endDecl   = String2Data ">" 
      val endCom    = String2Data "-->" 
      val endPi     = String2Data "?>" 
      val endSection= String2Data "]]>"
      val preAttDef = String2Data "\n    " 
      val startCom  = String2Data "<!--" 
      val startDtd  = String2Data "<!DOCTYPE "
      val startPi   = String2Data "<?" 

      fun putAttVal(f,lit,cv) = 
	 if !O_EXPAND_ATT_VAL then putAttValue(f,cv,Vector.sub(lit,0))
	 else putAttVector(f,lit)
      fun putEntVal(f,lit,cv) = 
	 if !O_EXPAND_ENT_VAL then putEntValue true (f,cv,Vector.sub(lit,0))
	 else putEntVector(f,lit)

      fun putAttDef(f,dtd,(aidx,at,ad)) = 
	 let 
	    val f1 = putData(f,preAttDef)
	    val f2 = putData(f1,Index2AttNot dtd aidx)
	    val f3 = putBlank f2
	    val f4 = case at
		      of AT_CDATA => putData(f3,CDATA)
		       | AT_NMTOKEN => putData(f3,NMTOKEN)
		       | AT_NMTOKENS => putData(f3,NMTOKENS)
		       | AT_ENTITY => putData(f3,ENTITY)
		       | AT_ENTITIES => putData(f3,ENTITIES)
		       | AT_ID => putData(f3,ID)
		       | AT_IDREF => putData(f3,IDREF)
		       | AT_IDREFS => putData(f3,IDREFS)
		       | AT_GROUP is => let val toks = map (Index2AttNot dtd) is
					    val f4 = putChar(f3,0wx28) (* #"(" *)
					    val f5 = putData(f4,hd toks)
					    val f6 = foldl 
					       (fn (tok,f) => (* " | " *)
						putData(putData(f,[0wx20,0wx7C,0wx20]),tok))
					       f5 (tl toks)
					in putChar(f6,0wx29) (* #")" *)
					end
		       | AT_NOTATION is => let val toks = map (Index2AttNot dtd) is
					       val f4 = putData(f3,NOTATION_L)
					       val f5 = putData(f4,hd toks)
					       val f6 = foldl 
						  (fn (tok,f) =>  (* " | " *)
						   putData(putData(f,[0wx20,0wx7C,0wx20]),tok))
						  f5 (tl toks)
					   in putChar(f6,0wx29) (* #")" *)
					   end
	    val f5 = putBlank f4
	 in case ad
	      of AD_DEFAULT((lit,cv,_),_) => putAttVal(f5,lit,cv)
	       | AD_FIXED((lit,cv,_),_) => let val f6 = putData(f5,FIXED)
				       in putAttVal(f6,lit,cv)
				       end
	       | AD_IMPLIED => putData(f5,IMPLIED)
	       | AD_REQUIRED => putData(f5,REQUIRED)
	 end

      fun putAttListDecl(f,dtd,(eidx,defs,ext)) = 
	 let 
	    val f1 = putData(f,ATTLIST)
	    val f2 = putData(f1,Index2Element dtd eidx)
	    val f3 = foldl (fn (ad,f) => putAttDef(f,dtd,ad)) f2 defs
	 in putData(f3,endDecl)
	 end

      fun putContentModel(f,dtd,cm) = 
	 let 
	    fun putCMs(f,sep,cms) = 
	       let 
		  val f1 = putChar(f,0wx28) (* #"(" *)
		  val f2 = putCM(f1,hd cms)
		  val f3 = foldl (fn (cm,f) => putCM(putData(f,sep),cm)) f2 (tl cms)
	       in putChar(f3,0wx29) (* #")" *)
	       end
	    and putCM(f,cm) =
	       case cm 
		 of CM_ELEM eidx => putData(f,Index2Element dtd eidx)
		  | CM_OPT cm => putChar(putCM(f,cm),0wx3F) (* #"?" *) 
		  | CM_REP cm => putChar(putCM(f,cm),0wx2A) (* #"*" *) 
		  | CM_PLUS cm => putChar(putCM(f,cm),0wx2B) (* #"+" *) 
		  | CM_ALT cms => putCMs(f,[0wx20,0wx7C,0wx20],cms) (* " | " *)
		  | CM_SEQ cms => putCMs(f,[0wx2C,0wx20],cms) (* ", " *)
	 in putCM(f,cm)
	 end

      fun putElemDecl(f,dtd,(eidx,cont,ext)) = 
	 let 
	    val f1 = putData(f,ELEMENT)
	    val f2 = putData(f1,Index2Element dtd eidx)
	    val f3 = putBlank f2
	    val f4 = case cont
		       of CT_ANY => putData(f3,ANY)
			| CT_EMPTY => putData(f3,EMPTY)
			| CT_MIXED nil => putData(f3,PCDATA)
			| CT_MIXED is => 
			  let 
			     val f4 = putData(f3,PCDATA_L)
			     val f5 = foldl 
				(fn (elem,f) => let val f1 = putData(f,[0wx20,0wx7C,0wx20])
						in putData(f1,elem)
						end)
 				f4 (map (Index2Element dtd) is)
			  in putData(f5,[0wx29,0wx2A])
			  end
			| CT_ELEMENT(cm,_) => putContentModel(f3,dtd,cm)
	 in putData(f4,endDecl)
	 end

      fun putExtId(f,EXTID(pub,sys)) =
	 case pub 
	   of SOME(x,q) => let val f1 = putData(f,PUBLIC)
			       val f2 = putChar(f1,q)
			       val f3 = putString(f2,x)
			       val f4 = putChar(f3,q)
			   in case sys
				of NONE => f4
				 | SOME (_,x,q) => let val f5 = putData(f4,[0wx20,q])
						       val f6 = putString(f5,Uri2String x)
						   in putChar(f6,q)
						   end
			   end
	    | NONE => case sys
			of NONE => f
			 | SOME (_,x,q) => let val f1 = putData(f,SYSTEM)
					       val f2 = putChar(f1,q)
					       val f3 = putString(f2,Uri2String x)
					   in putChar(f3,q)
					   end

      fun putGenEntDecl(f,dtd,(idx,ge,ext)) = 
	 let 
	    val f1 = putData(f,ENTITY_G)
	    val f2 = putData(f1,Index2GenEnt dtd idx)
	    val f3 = putBlank f2
	    val f4 = case ge
		       of GE_NULL => f3
			| GE_INTERN (lit,rep) => putEntVal(f3,lit,rep)
			| GE_EXTERN extId => putExtId(f3,extId)
			| GE_UNPARSED (extId,idx,_) => let val f4 = putExtId(f3,extId)
							   val f5 = putBlank f4
						       in putData(f5,Index2AttNot dtd idx)
						       end
	 in putData(f4,endDecl)
	 end

      fun putParEntDecl(f,dtd,(idx,pe,ext)) = 
	 let 
	    val f1 = putData(f,ENTITY_P)
	    val f2 = putData(f1,Index2ParEnt dtd idx)
	    val f3 = putBlank f2
	    val f4 = case pe
		       of PE_NULL => f3
			| PE_INTERN (lit,rep) => putEntVal(f3,lit,rep)
			| PE_EXTERN extId => putExtId(f3,extId)
	 in putData(f4,endDecl)
	 end

      fun putNotationDecl(f,dtd,(idx,extId,ext)) =
	 let 
	    val f1 = putData(f,NOTATION_D)
	    val f2 = putData(f1,Index2AttNot dtd idx)
	    val f3 = putBlank f2
	    val f4 = putExtId(f3,extId)
	 in putData(f4,endDecl)
	 end

      fun putDecl(f,dtd,(_,decl)) =
	 case decl
	   of DEC_ATTLIST x => putAttListDecl(f,dtd,x)
	    | DEC_ELEMENT x => putElemDecl(f,dtd,x)
	    | DEC_GEN_ENT x => putGenEntDecl(f,dtd,x)
	    | DEC_PAR_ENT x => putParEntDecl(f,dtd,x)
	    | DEC_NOTATION x => putNotationDecl(f,dtd,x)

      fun putAttSpec(f,dtd,(aidx,ap,spOpt)) = 
	 case ap 
	   of AP_IMPLIED => f
	    | AP_MISSING => f
	    | AP_DEFAULT _ => f 
	    | AP_PRESENT(lit,cv,_) => let val (sp,eq) = case spOpt
							  of NONE => ([0wx20],[0wx3D])
							   | SOME speq => speq
					  val f1 = putData(f,sp)
					  val f2 = putData(f1,Index2AttNot dtd aidx)
					  val f3 = putData(f2,eq)
					  val f4 = putAttVal(f3,lit,cv)
				      in f4
				      end
				   
      fun putStartTag (f,dtd,(_,eidx,atts,space,mt)) = 
	 let 
	    val f1 = putChar(f,0wx3C) (* #"<" *)
	    val f2 = putData(f1,Index2Element dtd eidx)
	    val f3 = foldl (fn (spec,f) => putAttSpec(f,dtd,spec)) f2 atts
	    val f4 = putData(f3,space)
	 in if mt then putData(f4,[0wx2F,0wx3E]) (* "/>" *) 
	    else putChar(f4,0wx3E) (* #">" *)
	 end

      fun putEndTag (f,dtd,(_,eidx,elsp)) = 
	 let 
	    val f1 = putData(f,[0wx3C,0wx2F]) (* "</" *)
	    val f2 = putData(f1,Index2Element dtd eidx)
	    val f3 = case elsp 
		       of NONE => f2
			| SOME(_,space) => putData(f2,space)
	 in putChar(f3,0wx3E)
	 end

      fun putXmlDecl(f,(v,e,s)) =
	 let 
	    val f1 = putData(f,XMLVERS)
	    val f2 = putChar(f1,0wx22)
	    val f3 = case v
		       of SOME version => putString(f2,version)
			| _ => putData(f2,ONE)
	    val f4 = putChar(f3,0wx22)
	    val f5 = case e
		       of SOME enc => let val f5 = putData(f4,ENCODING)
					  val f6 = putChar(f5,0wx22)
					  val f7 = putString(f6,enc)
				      in putChar(f7,0wx22)
				      end
			| _ => f4   
	    val f6 = case s 
		       of SOME stnd => let val f6 = putData(f5,STANDALONE)
					   val f7 = putChar(f6,0wx22)
					   val f8 = putData(f7,if stnd then YES else NO)
				       in putChar(f8,0wx22)
				       end
			| _ => f5
	    val f7 = putData(f6,endPi)
	 in putNl f7
	 end

      fun putProcInst(f,(_,target,_,text)) = 
	 let 
	    val f1 = putData(f,startPi)
	    val f2 = putData(f1,target)
	    val f3 = if Vector.length text=0 then f2
		     else putVector(putBlank f2,text)
	 in putData(f3,endPi)
	 end

      fun putComment(f,(_,text)) =
	 let 
	    val f1 = putData(f,startCom)
	    val f2 = putVector(f1,text)
	 in putData(f2,endCom)
	 end

      fun putCData(f,text) = 
	 let 
	    val f1 = putData(f,CDATA_S)
	    val f2 = putVector(f1,text)
	 in putData(f2,endSection)
	 end

   end
