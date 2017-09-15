signature VizOptions =
   sig
      val O_SILENT          : bool ref
      val O_ERROR_DEVICE    : TextIO.outstream ref
      val O_ERROR_LINEWIDTH : int ref
      val O_OUTPUT_FILE     : string ref   

      val setVizDefaults : unit -> unit
      val setVizOptions  : Options.Option list * (string -> unit) 
	 -> bool * bool * string option * string option

      val vizUsage : Options.Usage
   end

structure VizOptions : VizOptions = 
   struct
      open Options

      val O_SILENT          = ref false
      val O_ERROR_DEVICE    = ref TextIO.stdErr
      val O_ERROR_LINEWIDTH = ref 80 
      val O_OUTPUT_FILE     = ref "-"   

      fun setVizDefaults () = 
	 let 
            val _ = O_SILENT       := false
            val _ = O_ERROR_DEVICE := TextIO.stdErr
	    val _ = O_OUTPUT_FILE  := "-"
	 in ()
	 end

      val vizUsage = 
         [U_ITEM(["-o <file>","--output=<file>"],"Write output to file (stdout)"),
          U_ITEM(["-s","--silent"],"Suppress reporting of errors and warnings"),
          U_ITEM(["-e <file>","--error-output=<file>"],"Redirect errors to file (stderr)"),
          U_SEP,
          U_ITEM(["--version"],"Print the version number and exit"),
          U_ITEM(["-?","--help"],"Print this text and exit"),
          U_ITEM(["--"],"Do not recognize remaining arguments as options")
          ]

      fun setVizOptions (opts,optError) =
	 let
            fun onlyOne what = "at most one "^what^" may be specified"
            fun unknown pre opt = String.concat ["unknown option ",pre,opt]
            fun hasNoArg pre key = String.concat ["option ",pre,key," expects no argument"]
            fun mustHave pre key = String.concat ["option ",pre,key," must have an argument"]

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
		  | "error-output" => 
		    (case valOpt
		       of NONE => pars before optError (mustHave "--" key)
			| SOME s => (v,h,SOME s,f))
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
