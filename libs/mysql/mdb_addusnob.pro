;+
; NAME: mdb_addusnob 
;
; 	Add usnob fluxes to the database.
;
; INPUTS:
; 	name	object name
; 	id		usnob object ID
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2005-10-12 03:56:19 by Marshall Perrin 
;-

PRO mdb_addusnob,name=name,id=id,simname=simname,_extra=_extra


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
		cmt = quote("MDB_ADDusnob.pro, using id='USNOB"+id+"'.  "+systime()) 
	endif else begin
		cmt = quote("MDB_ADDusnob.pro, using name='"+name+"'.  "+systime())
	endelse

	
	for i=0L,n_elements(names)-1 do begin
		if keyword_set(simname) then query_usnob,simname,Bmag,Rmag,Imag,id=id,/auto,count=count,result=r,_extra=_extra $
		else query_usnob,names[i],Bmag,Rmag,Imag,id=id, /auto,count=count,result=r,_extra=_extra
		if count lt 1 then message,"Couldn't find a match for "+name

		q = "delete from photometry where name="+quote(name)+" and reference like 'usnob%';"
		mysqlcmd,sqlhandle,q

		mdb_addflux,name,'B',Bmag,nan,nan,nan,'USNO B1',comment=cmt
		mdb_addflux,name,'R',Rmag,nan,nan,nan,'USNO B1',comment=cmt
		mdb_addflux,name,'I',Imag,nan,nan,nan,'USNO B1',comment=cmt
	endfor 


	; add an alias for the name.
	mdb_rename,'USNOB '+r.usno_b1_0,name,/alt,/auto 

end
