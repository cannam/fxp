structure NullHooks =
   struct
      open Errors IgnoreHooks NullOptions

      type AppData = OS.Process.status
      type AppFinal = AppData
      val nullStart = OS.Process.success

      fun printError(pos,err) = if !O_SILENT then () else TextIO.output
	 (!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
	  (Position2String pos
           ::(if isFatalError err then "Fatal error:" else "Error:")
           ::errorMessage err))
      fun printWarning(pos,warn) = if !O_SILENT then () else TextIO.output
	 (!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
	  (Position2String pos^" Warning:"::warningMessage warn))

      fun hookError   (_,pe) = OS.Process.failure before printError pe
      fun hookWarning (status,pw) = status before printWarning pw 
   end
