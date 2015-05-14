;+
; NAME: mdb_getmag 
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-06-21 19:38:39 by Marshall Perrin 
;-

function mdb_getmag,name,band

common mysql, SQLhandle
     mysqlcheck,SQLhandle

	 
	q = "Select magnitude from photometry where name = "+quote(name)+" and band ="+quote(band)+" order by date;"
	mysqlquery,sqlhandle,q,mag,ngood=count

	if count lt 1 then begin
		message,"Couldn't find "+band+" for "+name+"!" ,/info
		return, !values.f_nan
	endif else return,mag[0]
	
	

end
