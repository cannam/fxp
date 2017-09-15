




structure EsisData = 
   struct
      type Stag = int * (int * HookData.AttPresent) list
	 
      datatype Item = 
	 PI of UniChar.Data * UniChar.Vector
       | ELEM of Stag * Item list
       | DATA of UniChar.Vector
	 
      type Content  = Item list
   end
