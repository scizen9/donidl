;+
; NAME:  
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-07-23 12:35:52 by Marshall Perrin 
;-

PRO mdb_listobs,name

	nn = mdb_nameresolve(name)

	q = "select date,instrument,band,exptime from observations where name='"+nn+"' order by date;"

	common mysql, SQLhandle
	mysqlcheck,SQLhandle
	mysqlquery,SQLhandle,q,date,inst,band,exptime

	forprint,date,inst,band,exptime,format="(A15,A15,A15,A15)",/textout

end
