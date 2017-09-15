signature CopyOptions =
   sig
      val O_SILENT             : bool ref
      val O_ERROR_DEVICE       : TextIO.outstream ref
      val O_ERROR_LINEWIDTH    : int ref

      val O_EXPAND_CONT_CHAR   : bool ref 
      val O_EXPAND_CONT_EXT    : bool ref 
      val O_EXPAND_CONT_INT    : bool ref 
					  
      val O_EXPAND_SUBSET_EXT  : bool ref 
      val O_EXPAND_SUBSET_INT  : bool ref 
      val O_INCLUDE_EXT_SUBSET : bool ref 
					  
      val O_EXPAND_ENT_VAL     : bool ref 
      val O_EXPAND_ATT_VAL     : bool ref

      val O_OUTPUT_ENCODING    : string option ref
      val O_OUTPUT_FILE        : string ref 

      val setCopyDefaults : unit -> unit
      val setCopyOptions  : Options.Option list * (string -> unit) 
	 -> bool * bool * string option * string option

      val copyUsage : Options.Usage
   end

structure CopyOptions : CopyOptions = 
   struct
      open Options UtilList 

      val O_SILENT             = ref false
      val O_ERROR_DEVICE       = ref TextIO.stdErr
      val O_ERROR_LINEWIDTH    = ref 80 

      val O_EXPAND_CONT_CHAR   = ref false
      val O_EXPAND_CONT_EXT    = ref false
      val O_EXPAND_CONT_INT    = ref false

      val O_EXPAND_SUBSET_EXT  = ref false
      val O_EXPAND_SUBSET_INT  = ref false
      val O_INCLUDE_EXT_SUBSET = ref false

      val O_EXPAND_ENT_VAL     = ref false
      val O_EXPAND_ATT_VAL     = ref false

      val O_OUTPUT_ENCODING    = ref NONE : string option ref
      val O_OUTPUT_FILE        = ref "-"   

      fun setCopyDefaults () = 
	 let 
            val _ = O_SILENT             := false
            val _ = O_ERROR_DEVICE       := TextIO.stdErr

	    val _ = O_EXPAND_CONT_CHAR   := false 
	    val _ = O_EXPAND_CONT_EXT    := false 
	    val _ = O_EXPAND_CONT_INT    := false 
	       
	    val _ = O_EXPAND_SUBSET_EXT  := false 
	    val _ = O_EXPAND_SUBSET_INT  := false 
	    val _ = O_INCLUDE_EXT_SUBSET := false 
	       
	    val _ = O_EXPAND_ENT_VAL     := false 
	    val _ = O_EXPAND_ATT_VAL     := false 
	       
	    val _ = O_OUTPUT_ENCODING    := NONE
	    val _ = O_OUTPUT_FILE        := "-"   
	 in ()
	 end
      
      val copyUsage = 
         [U_ITEM(["-o <file>","--output=<file>"],"Write output to file (stdout)"),
          U_ITEM(["-s","--silent"],"Suppress reporting of errors and warnings"),
          U_ITEM(["-e <file>","--error-output=<file>"],"Redirect errors to file (stderr)"),
	  U_SEP,
	  U_ITEM(["--expand-refs-content[=(yes,no,<key list>]"],
                 "Controls expansion of entity references in content (no)"),
	  U_ITEM(["--expand-refs-subset[=(no|yes|int|ext)]"],
                 "Controls expansion of entity references in the DTD (no)"),
	  U_ITEM(["--expand-ext-subset[=(yes|no)]"],"Expand external subset (no)"), 
	  U_ITEM(["--expand-att-vals[=(yes|no)]"],"Expand and normalize attribute values (no)"), 
	  U_ITEM(["--expand-ent-vals[=(yes|no)]"],"Expand entity values (no)"),
	  U_SEP,
	  U_ITEM(["--expand=yes"],"Expand everything"),
	  U_ITEM(["--expand=no"],"Expand nothing"),
          U_ITEM(["--expand=int"],"Expand internal and character entities"),
          U_ITEM(["--expand=ext"],"Expand external entities and subset"),
          U_SEP,
          U_ITEM(["--version"],"Print the version number and exit"),
          U_ITEM(["-?","--help"],"Print this text and exit"),
          U_ITEM(["--"],"Do not recognize remaining arguments as options")
          ]

      fun setCopyOptions (opts,optError) =
	 let
	    exception Failed
	    
	    val yesNoList= "'yes', 'no' or a list of 'char', 'ext' and 'int'"
	    val yesNoExtInt = "'yes', 'no', 'ext' or 'int'"
	    val yesNo = "'yes' or 'no'"
	       
            fun onlyOne what = "at most one "^what^" may be specified"
            fun unknown pre opt = String.concat ["unknown option ",pre,opt]
            fun hasNoArg pre key = String.concat ["option ",pre,key," expects no argument"]
            fun mustHave pre key = String.concat ["option ",pre,key," must have an argument"]
	    fun errorMustBe(key,what) = String.concat ["the argument to --",key," must be ",what]
	       
            fun check_noarg(key,valOpt) = 
               if isSome valOpt then optError (hasNoArg "--" key) else () 

	    fun do_long (pars as (v,h,e,f)) (key,valOpt) =
	       case key 
		 of "help" => (v,true,e,f) before check_noarg(key,valOpt)
		  | "version" => (true,h,e,f) before check_noarg(key,valOpt)
                  | "silent" => pars before O_SILENT := true before check_noarg(key,valOpt)
		  | "output" => 
		    (case valOpt
		       of NONE => pars before optError (mustHave "--" key)
			| SOME s => pars before O_OUTPUT_FILE := s)
		  | "output-encoding" => 
	            (case valOpt
		       of NONE => pars before optError (mustHave "--" key)
			| SOME s => pars before O_OUTPUT_ENCODING := SOME s)
		  | "error-output" => 
		    (case valOpt
		       of NONE => pars before optError (mustHave "--" key)
			| SOME s => (v,h,SOME s,f))
		  | "expand-refs-content" =>
		    let 
		       datatype what = CHAR | EXT | INT
		       fun setFlags whats = 
			  let 
			     val _ = O_EXPAND_CONT_CHAR := member CHAR whats
			     val _ = O_EXPAND_CONT_EXT := member EXT whats
			     val _ = O_EXPAND_CONT_INT := member INT whats
			  in ()
			  end
		       val _ = case valOpt 
				 of NONE => setFlags [CHAR,EXT,INT]
				  | SOME "yes" => setFlags [CHAR,EXT,INT]
				  | SOME "no" => setFlags nil
				  | SOME s => let val fields = String.fields (fn c => #","=c) s
						  val whats = foldr 
						     (fn(f,yet)=> 
						      case f
							of "char" => CHAR::yet
							 | "ext" => EXT::yet
							 | "int" => INT::yet
							 | _ => yet before optError 
							   (errorMustBe(key,yesNoList)))
						     nil fields
					      in setFlags whats
					      end
		    in pars
		    end
		  | "expand-refs-subset" =>
		    (let val (ext,int) = case valOpt 
					   of NONE => (true,true)
					    | SOME "yes" => (true,true)
					    | SOME "no" => (false,false)
					    | SOME "ext" => (true,false)
					    | SOME "int" => (false,true)
					    | SOME _ => raise Failed
			 val _ = O_EXPAND_SUBSET_EXT := ext
			 val _ = O_EXPAND_SUBSET_INT := int
			 val _ = if ext then (O_EXPAND_ENT_VAL := true) else ()
		     in pars
		     end
			handle Failed => pars before optError (errorMustBe(key,yesNoExtInt)) )
		  | "expand-ent-vals" =>
		    (let val exp = case valOpt 
				     of NONE => true
				      | SOME "yes" => true
				      | SOME "no" => false
				      | SOME _ => raise Failed
			 val _ = O_EXPAND_ENT_VAL := exp
		     in pars
		     end
			handle Failed => pars before optError (errorMustBe(key,yesNo)) )
		  | "expand-att-vals" =>
		    (let val exp = case valOpt 
				     of NONE => true
				      | SOME "yes" => true
				      | SOME "no" => false
				      | SOME _ => raise Failed 
			 val _ = O_EXPAND_ATT_VAL := exp
		     in pars
		     end
			handle Failed => pars before optError (errorMustBe(key,yesNo)) )
		  | "expand-ext-subset" =>
		    (let val exp = case valOpt 
				     of NONE => true
				      | SOME "yes" => true
				      | SOME "no" => false
				      | SOME _ => raise Failed
			 val _ = O_INCLUDE_EXT_SUBSET := exp
			 val _ = if exp then (O_EXPAND_ENT_VAL := true) else ()
		     in pars
		     end
			handle Failed => pars before optError (errorMustBe(key,yesNo)) )
		  | "expand" =>
		    let 
		       datatype what = ALL | NO | EXT | INT
		       val ext = [O_EXPAND_CONT_EXT,O_EXPAND_SUBSET_EXT, 
				  O_INCLUDE_EXT_SUBSET,O_EXPAND_ENT_VAL]
		       val int = [O_EXPAND_CONT_CHAR,O_EXPAND_CONT_INT,  
				  O_EXPAND_SUBSET_INT,O_EXPAND_ATT_VAL]
		       val all = ext@int
		       fun setFlags what = 
			  let val (yes,no) = 
			     case what 
			       of ALL => (all,nil)
				| NO => (nil,all)
				| EXT => (ext,int)
				| INT => (int,ext)
			      val _ = app (fn x => x := true) yes
			      val _ = app (fn x => x := false) no
			  in ()
			  end
		    val _ = case valOpt
			      of NONE => setFlags ALL
			       | SOME "yes" => setFlags ALL
			       | SOME "no" => setFlags NO
			       | SOME "ext" => setFlags EXT
			       | SOME "int" => setFlags INT
			       | SOME _ => optError (errorMustBe(key,yesNoExtInt))
		    in pars
		    end  
		  | _ => pars before optError(unknown "--" key)
		    
	    fun do_short (pars as (v,h,e,f)) (cs,opts) =
	       case cs 
		 of nil => doit pars opts
		  | [#"o"] => (case opts 
				 of OPT_STRING s::opts1 => (O_OUTPUT_FILE := s;
							    doit pars opts1)
				  | _ => (optError (mustHave "-" "o"); doit pars opts))
		  | [#"e"] => (case opts 
				 of OPT_STRING s::opts1 => doit (v,h,SOME s,f) opts1
				  | _ => (optError (mustHave "-" "e"); doit pars opts))
  		  | cs => doit (foldr
				(fn (c,pars) 
				 => case c
				      of #"s" => pars before O_SILENT := true
				       | #"o" => pars before optError (mustHave "-" "o")
				       | #"e" => pars before optError (mustHave "-" "e")
				       | #"?" => (v,true,e,f)
				       | c => pars before 
					 optError(unknown "-" (String.implode [c])))
				pars cs) opts
		    
	    and doit pars nil = pars
	      | doit (pars as (v,h,e,f)) (opt::opts) =
	       case opt 
		 of OPT_LONG(key,valOpt) => doit (do_long pars (key,valOpt)) opts
		  | OPT_SHORT cs => do_short pars (cs,opts)
		  | OPT_STRING s => if isSome f 
				       then let val _ = optError(onlyOne "input file") 
					    in doit pars opts
					    end
				    else doit (v,h,e,SOME s) opts
		  | OPT_NOOPT => doit pars opts
		  | OPT_NEG cs => let val _ = if null cs then ()
					      else app (fn c => optError
							(unknown "-n" (String.implode[c]))) cs
				  in doit pars opts
				  end
	 in doit (false,false,NONE,NONE) opts
	 end
   end
