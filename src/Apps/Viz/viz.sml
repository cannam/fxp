signature Viz =
   sig
      val viz : string * string list -> OS.Process.status
   end

structure Viz =
   struct
      structure ParserOptions = ParserOptions ()
      structure CatOptions = CatOptions ()
      structure CatParams =
	 struct
	    open CatError CatOptions VizOptions Uri UtilError 

	    fun catError(pos,err) = if !O_SILENT then () else TextIO.output
	       (!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
		(Position2String pos^" Error in catalog:"::catMessage err))
	 end
      structure Resolve  = ResolveCatalog (structure Params = CatParams)
      structure VizHooks = VizHooks (structure Dtd = Dtd)
      structure ParseViz = Parse (structure Dtd = Dtd
				  structure Hooks = VizHooks
				  structure Resolve = Resolve
				  structure ParserOptions = ParserOptions)

      open 
	 CatOptions VizHooks VizOptions Options ParserOptions Uri

      val usage = List.concat [parserUsage,[U_SEP],catalogUsage,[U_SEP],vizUsage]

      exception Exit of OS.Process.status 
	 
      fun viz(prog,args) = 
	 let 
	    val prog = "fxviz"
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
	    val _ = setVizDefaults()
	    val (vers,help,err,file) = setVizOptions (opts2,optError)
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
	    val dtd = initDtdTables()
	    val _ = ParseViz.parseDocument uri (SOME dtd) (vizStart dtd)
	    val _ = if isSome err then TextIO.closeOut (!O_ERROR_DEVICE) else ()
	 in OS.Process.success
	 end
      handle Exit status => status 
	   | exn => 
	 let val _ = TextIO.output 
	    (TextIO.stdErr,prog^": Unexpected exception: "^exnMessage exn^".\n")
	 in OS.Process.failure
	 end
   end

val _ = Viz.viz(CommandLine.name (), CommandLine.arguments ())
