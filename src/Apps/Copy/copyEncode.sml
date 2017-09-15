signature CopyEncode = 
   sig   
      type File 
	 
      val noFile        : File
      val openFile      : string * Encoding.Encoding * string -> File
      val closeFile     : File -> unit

      val putBlank      : File -> File
      val putNl         : File -> File

      val putChar       : File * UniChar.Char -> File
      val putAttChar    : File * UniChar.Char -> File
      val putDataChar   : File * UniChar.Char -> File

      val putData       : File * UniChar.Data -> File
      val putAttData    : File * UniChar.Data -> File
      val putDataData   : File * UniChar.Data -> File

      val putVector     : File * UniChar.Vector -> File
      val putAttVector  : File * UniChar.Vector -> File
      val putDataVector : File * UniChar.Vector -> File
      val putEntVector  : File * UniChar.Vector -> File

      val putAttValue   : File * UniChar.Vector * UniChar.Char -> File
      val putEntValue   : bool -> File * UniChar.Vector * UniChar.Char -> File
      val putString     : File * string -> File

      val putCharRef    : File * UniChar.Char -> File
      val putGenRef     : File * UniChar.Data -> File
      val putParRef     : File * UniChar.Data -> File
   end

functor CopyEncode (structure ParserOptions : ParserOptions) : CopyEncode = 
   struct
      open 
	 CopyOptions Encode ParserOptions UniChar UniClasses UtilError  

      fun encodeError err = if !O_SILENT then () else 
	 TextIO.output(!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
		       ("Encoding error:"::encodeMessage err))

      type File = EncFile 
      val noFile = encNoFile
      fun openFile fe = encOpenFile fe 
	 handle NoSuchFile (f,msg) => noFile before 
	    TextIO.output(!O_ERROR_DEVICE,formatMessage (4,!O_ERROR_LINEWIDTH) 
			  ["Cannot open file '"^f^"' for writing. ("^msg^")"])
      val closeFile = encCloseFile 
      val validChar = encValidChar

      fun putChar(enc,c) = encPutChar(enc,c) 
	 handle EncodeError(f,msg) => encAdapt(enc,f) before encodeError msg

      fun putData(enc,cs) = foldl (fn (c,enc) => putChar(enc,c)) enc cs
      fun putVector(enc,cv) = Vector.foldl (fn (c,enc) => putChar(enc,c)) enc cv
	 
      fun putNl f = putChar(f,0wx0A)
      fun putBlank f = putChar(f,0wx20)

      val hexDigits = Vector.tabulate(16,fn i => Chars.fromInt((if i<10 then 48 else 55)+i))
      fun hexDigit n = Vector.sub(hexDigits,Chars.toInt n)
      fun charRefSeq c  =
	 if c=0wx00 then [0wx26,0wx23,0wx78,0wx30,0wx3b] (* "&#x0;" *)
	 else let fun mk_hex yet n = if n=(0w0:Char) then yet
				     else mk_hex (hexDigit(n mod 0w16)::yet) (n div 0w16)
	      in 0wx26::0wx23::0wx78::mk_hex [0wx3b] c
	      end
      fun putCharRef(f,c) = putData(f,charRefSeq c)
      fun putGenRef(f,ent) = let val f1 = putChar(f,0wx26)
				 val f2 = putData(f1,ent)
				 val f3 = putChar(f2,0wx3b)
			     in f3
			     end
      fun putParRef(f,ent) = let val f1 = putChar(f,0wx25)
				 val f2 = putData(f1,ent)
				 val f3 = putChar(f2,0wx3b)
			     in f3
			     end
	 
      fun putAttChar(f,c) = 
	 case c 
	   of 0wx26 => putData(f,[0wx26,0wx61,0wx6d,0wx70,0wx3b]) (* "&amp;" *)
	    | 0wx3C => putData(f,[0wx26,0wx6c,0wx74,0wx3b]) (* "&lt;" *)
	    | _ => if validChar(f,c) then putChar(f,c) else putCharRef(f,c)
      fun putDataChar(f,c) = 
	 case c 
	   of 0wx26 => putData(f,[0wx26,0wx61,0wx6d,0wx70,0wx3b]) (* "&amp;" *)
	    | 0wx3C => putData(f,[0wx26,0wx6c,0wx74,0wx3b]) (* "&lt;" *)
	    | 0wx3E => if !O_COMPATIBILITY 
			  then putData(f,[0wx26,0wx67,0wx74,0wx3b]) (* "&gt;" *)
		       else putChar(f,c)
	    | _ => if validChar(f,c) then putChar(f,c) else putCharRef(f,c)
		      
      fun putAttData(f,cs) =
	 foldl (fn (c,f) => putAttChar(f,c)) f cs
      fun putDataData(f,cs) =
	 foldl (fn (c,f) => putDataChar(f,c)) f cs

      fun putAttVector(f,cv) =
	 Vector.foldl (fn (c,f) => if validChar(f,c) then putChar(f,c) else putCharRef(f,c)) f cv
      fun putDataVector(f,cv) =
	 Vector.foldl (fn (c,f) => putDataChar(f,c)) f cv
      fun putEntVector (f,cv) = 
	 Vector.foldl (fn (c,f) => if validChar(f,c) then putChar(f,c) else putCharRef(f,c)) f cv

      fun putAttValue (f,cv,q) = 
	 let 
	    fun putOne(c,f) = 
	       case c 
		 of 0wx26 => putData(f,[0wx26,0wx61,0wx6d,0wx70,0wx3b]) (* "&amp;" *)
		  | 0wx3C => putData(f,[0wx26,0wx6c,0wx74,0wx3b]) (* "&lt;" *)
		  | _ => if c<>q andalso validChar(f,c) then putChar(f,c) else putCharRef(f,c)
	    val f1 = putChar(f,q)
	    val f2 = Vector.foldl putOne f1 cv
	    val f3 = putChar(f2,q)
	 in f3
	 end
      fun putEntValue escapeParRef (f,cv,q) = 
	 let 
	    fun putOne(i,c,f) =
	       case c 
		 of 0wx25 => if escapeParRef then putCharRef(f,c) else putChar(f,c)
		  | 0wx26 => if i+1<Vector.length cv andalso isNms(Vector.sub(cv,i+1))
				then putChar(f,c) else putCharRef(f,c)
		  | _ => if c<>q andalso validChar(f,c) then putChar(f,c) else putCharRef(f,c)
				   
	    val f1 = putChar(f,q)
            val f2 = Vector.foldli putOne f1 cv
	    val f3 = putChar(f2,q)
	 in f3
	 end

      fun putString(f,str) = putData(f,String2Data str)
   end
