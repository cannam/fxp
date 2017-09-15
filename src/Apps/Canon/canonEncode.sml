














signature CanonEncode = 
   sig   
      type File

      val noFile    : File
      val openFile  : unit -> File
      val closeFile : File -> unit

      val putBlank      : File -> File
      val putChar       : File * UniChar.Char -> File
      val putData       : File * UniChar.Data -> File
      val putVector     : File * UniChar.Vector -> File
      val putDataChar   : File * UniChar.Char -> File
      val putDataVector : File * UniChar.Vector -> File
   end

structure CanonEncode : CanonEncode = 
   struct
      open 
	 CanonOptions Encode UniChar UniClasses UtilError  

      fun decodeError err = if !O_SILENT then () else 
	 TextIO.output(!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
		       ("Encoding error:"::encodeMessage err))

      type File = EncFile 
      val noFile = encNoFile
      fun openFile() = encOpenFile (!O_OUTPUT_FILE,Encoding.UTF8,"UTF-8") 
	 handle NoSuchFile (f,msg) => noFile before 
	    TextIO.output(!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
			  ["Cannot open file '"^f^"' for writing. ("^msg^")"])
      val closeFile = encCloseFile 
      val validChar = encValidChar

      fun putChar(enc,c) = encPutChar(enc,c) 
	 handle EncodeError(f,msg) => encAdapt(enc,f) before decodeError msg

      fun putData(enc,cs) = foldl (fn (c,enc) => putChar(enc,c)) enc cs
      fun putVector(enc,cv) = Vector.foldl (fn (c,enc) => putChar(enc,c)) enc cv
	 
      fun putBlank f = putChar(f,0wx20)
      fun putDataChar (f,c) = 
	 case c 
	   of 0wx09 => putData(f,[0wx26,0wx23,0wx39,0wx3b]) (* "&#9;" *)
	    | 0wx0A => putData(f,[0wx26,0wx23,0wx31,0wx30,0wx3b]) (* "&#10;" *)
	    | 0wx0D => putData(f,[0wx26,0wx23,0wx31,0wx33,0wx3b]) (* "&#13;" *)
	    | 0wx22 => putData(f,[0wx26,0wx71,0wx75,0wx6f,0wx74,0wx3b]) (* "&quot;" *)
	    | 0wx26 => putData(f,[0wx26,0wx61,0wx6d,0wx70,0wx3b]) (* "&amp;" *)
	    | 0wx3C => putData(f,[0wx26,0wx6c,0wx74,0wx3b]) (* "&lt;" *)
	    | 0wx3E => putData(f,[0wx26,0wx67,0wx74,0wx3b]) (* "&gt;" *)
	    | _ => putChar(f,c)

      fun putDataVector (f,cv) = 
	 Vector.foldl (fn (c,f) => putDataChar(f,c)) f cv
   end
