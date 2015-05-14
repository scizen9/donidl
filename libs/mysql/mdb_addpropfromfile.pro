;+
; NAME:  
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-05-18 17:17:00 by Marshall Perrin 
;-


PRO mdb_addpropfromfile,filename

	tab=string(09b)
	readcol,filename,delim=tab,name,property,value,value_error,reference,$
		format="A,A,A,A,A"

	n = n_elements(name)
	cmt = "ADDPROPFROMFILE: filename "+filename+" ; "+systime()
	for i=0L,n-1 do begin
		
		err = value_error[i]
		if err eq '-' then delvarx,err

		ref = reference[i]
		year = stregex(ref,"[1-2][0-9][0-9][0-9]",/extract)
        if year eq "" then year = '2004'
        date = year+"-01-01" 
	
		mdb_addproperty,name[i],property[i],value[i],value_err=err,ref,date=date,comment=cmt

		mdb_sed,name[i],/prop
		;stop

	endfor 

end
