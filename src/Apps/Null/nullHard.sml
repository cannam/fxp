




















(*
structure NullHard =
   struct
      fun parseNull uri = NullParse.parseDocument uri NONE NullHooks.nullStart

      open 
	 NullCatOptions NullOptions Options NullParserOptions Uri

      val usage = List.concat [parserUsage,[("","")],catalogUsage,[("","")],nullUsage]

      exception Exit of OS.Process.status 
	 
      fun null(prog,args) = 
	 let 
	    val prog = "fxp"
	    val hadError = ref false
	       
	    fun optError msg = 
	       let val _ = TextIO.output(TextIO.stdErr,msg^".\n")
	       in hadError := true
	       end
	    fun exitError msg = 
	       let val _ = TextIO.output(TextIO.stdErr,msg^".\n")
	       in raise Exit OS.Process.failure 
	       end
	    fun exitHelp prog = 
	       let val _ = printUsage TextIO.stdOut prog usage
	       in raise Exit OS.Process.success 
	       end
	    fun exitVersion prog = 
	       let val _ = app print [prog," version ",Version.FXP_VERSION,"\n"]
	       in raise Exit OS.Process.success 
	       end
	    
	    fun summOpt prog = "For a summary of options type "^prog^" --help"
	    fun noFile(f,cause) = "can't open file '"^f^"': "^exnMessage cause
	       
	    val opts = parseOptions args
	    val _ = setParserDefaults()
	    val opts1 = setParserOptions (opts,optError)
	    val _ = setCatalogDefaults()
	    val opts2 = setCatalogOptions (opts1,optError)
	    val _ = setNullDefaults()
	    val (vers,help,err,file) = setNullOptions (opts2,optError)
	    val _ = if !hadError then exitError (summOpt prog) else ()
	    val _ = if vers then exitVersion prog else ()
	    val _ = if help then exitHelp prog else ()
	    val _ = case err 
		      of SOME "-" => O_ERROR_DEVICE := TextIO.stdErr
		       | SOME f => (O_ERROR_DEVICE := TextIO.openOut f
				    handle IO.Io {cause,...} => exitError(noFile(f,cause)))
		       | NONE => ()
	    val f = valOf file handle Option => "-"
	    val uri = if f="-" then NONE else SOME(String2Uri f)
	    val status = parseNull uri
	    val _ = if isSome err then TextIO.closeOut (!O_ERROR_DEVICE) else ()
	 in status
	 end
      handle Exit status => status 
	   | exn => 
	 let val _ = TextIO.output 
	    (TextIO.stdErr,prog^": Unexpected exception: "^exnMessage exn^".\n")
	 in OS.Process.failure 
	 end
   end
*)
structure NullHard = struct end
