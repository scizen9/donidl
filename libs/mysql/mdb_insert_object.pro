;+
; NAME: insert_object
;
; 	Insert an object into the database.
;
; 	Optionally will query simbad for coords and get the
; 	USNO/2MASS/IRAS magnitudes (if available).
;
; 	Warning! matching of objects against those catalogs is just based
; 	on closest object, and is not necessarily perfect.
;
; USAGE:
; 	mdb_insert_object, "FX Tau",/auto
;
; INPUTS:
; 	name		name of the object (should be simbad-resolvable, hopefully)
; 	ra,dec		can specify RA and Dec instead of trying to get from SIMBAD
; KEYWORDS:
; 	/AUTO		automatically get coords and magnitudes.
; 	
; OUTPUTS:
;
; HISTORY:
; 	Began 2005-10-05 18:24:52 by Marshall Perrin 
;-

PRO mdb_insert_object,name,objtype,ra,dec,auto=auto
	

	common mysql, SQLhandle
	mysqlcheck,SQLhandle
	

	if keyword_set(auto) then begin
		if ~(keyword_set(ra)) then ra=''
		if ~(keyword_set(dec)) then dec=''
	endif
	
	c = ", "

	cmd = "insert ignore into sources (name, ra2000,dec2000,objecttype) values ("+$
		quote(name)+c+quote(ra)+c+quote(dec)+c+quote(objtype)+");"

	print,cmd

	mysqlcmd,SQLhandle,cmd,answer,nlines


;	query = "Select name, ra2000, dec2000 from objects;"
;	mysqlquery,SQLhandle,query,n,r,d

			 
if keyword_set(auto) then begin 
	mdb_addusnob,name=name
	mdb_add2mass,name=name
	mdb_addiras,name=name
	mdb_simbadcoords,name=name
	mdb_sed,name
end

end
