;+
; NAME: mdb_addiras 
;
; 	Add iras fluxes to the database.
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2005-10-12 03:56:19 by Marshall Perrin 
;2006-04-20 14:13:04 split from mdb_add2mass
;-

PRO mdb_addiras,name=name,simname=simname


	common mysql, SQLhandle
	mysqlcheck,SQLhandle


	if ~(keyword_set(name)) then begin
		q = "select distinct name from sources;"
		mysqlquery,SQLhandle,q,names
	endif else begin
		names=[name]
	endelse

	for i=0L,n_elements(names)-1 do begin
		myname = mdb_nameresolve(names[i])
		cmt = quote("MDB_ADDiras.pro, using name='"+myname+"'.  "+systime())
		
		if keyword_set(simname) then query_iras,simname,i12,i25,i60,i100,/auto,status=status,iras=catalog $
		else		query_iras,names[i],i12,i25,i60,i100,/auto,status=status,iras=catalog
		if status eq 0 then return
		c=","
		q2 = "insert ignore into photometry (name,band,flux,flux_error,reference,comment) values ("+quote(myname)+', "IRAS 12",'+strc(i12,/nan)+',NULL,"IRAS PSC",'+cmt+');'
		mysqlcmd,SQLhandle,q2
		q2 = "insert ignore into photometry (name,band,flux,flux_error,reference,comment) values ("+quote(myname)+', "IRAS 25",'+strc(i25,/nan)+',NULL,"IRAS PSC",'+cmt+');'
		mysqlcmd,SQLhandle,q2
		q2 = "insert ignore into photometry (name,band,flux,flux_error,reference,comment) values ("+quote(myname)+', "IRAS 60",'+strc(i60,/nan)+',NULL,"IRAS PSC",'+cmt+');'
		mysqlcmd,SQLhandle,q2
		q2 = "insert ignore into photometry (name,band,flux,flux_error,reference,comment) values ("+quote(myname)+', "IRAS 100",'+strc(i100,/nan)+',NULL,"IRAS PSC",'+cmt+');'
		mysqlcmd,SQLhandle,q2

		; add an alias for the name.
		mdb_rename,catalog,name,/alt,/auto 

	
	endfor 


end
