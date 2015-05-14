;+
; NAME: mdb_add2mass 
;
; 	Add 2MASS fluxes to the database.
;
; INPUTS:
; 	name	object name
; 	id		2mass object ID
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2005-10-12 03:56:19 by Marshall Perrin 
;-

PRO mdb_add2mass,name=name,id=id,simname=simname,_extra=_extra


	common mysql, SQLhandle
	mysqlcheck,SQLhandle

	if ~(keyword_set(name)) then begin
		
		q = "select distinct name from sources;"
		mysqlquery,SQLhandle,q,names
	endif else begin
		names=[name]
	endelse

	nan = !values.f_nan
	if keyword_set(id) then begin
		cmt = quote("MDB_ADD2MASS.pro, using id='2MASS J"+id+"'.  "+systime()) 
	endif else begin
		cmt = quote("MDB_ADD2MASS.pro, using name='"+name+"'.  "+systime())
	endelse

	
	for i=0L,n_elements(names)-1 do begin
		if keyword_set(simname) then query_2mass,simname,id=id,/auto,count=count,result=r,_extra=_extra $
		else query_2mass,names[i],id=id, /auto,count=count,result=r,_extra=_extra
		if count lt 1 then message,"Couldn't find a match for "+name

		q = "delete from photometry where name="+quote(name)+" and reference like '2MASS%';"
		mysqlcmd,sqlhandle,q

		mdb_addflux,name,'2MASS J',r.Jmag,r.e_Jmag,nan,nan,'2MASS',date=quote(r.date),comment=cmt
		mdb_addflux,name,'2MASS H',r.Hmag,r.e_Hmag,nan,nan,'2MASS',date=quote(r.date),comment=cmt
		mdb_addflux,name,'2MASS Ks',r.Kmag,r.e_Kmag,nan,nan,'2MASS',date=quote(r.date),comment=cmt
	endfor 


	; add an alias for the name.
	mdb_rename,'2MASS J'+r._2MASS,name,/alt,/auto 

end
