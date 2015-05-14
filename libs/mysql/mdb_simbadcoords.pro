;+
; NAME: mdb_add2mass 
;
; 	Add 2MASS fluxes to the database.
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2005-10-12 03:56:19 by Marshall Perrin 
;-

PRO mdb_simbadcoords,name=name,simbadname=simbadname


	common mysql, SQLhandle
	mysqlcheck,SQLhandle

	if ~(keyword_set(name)) then begin
		q = "select distinct name from sources where sources.ra2000 ='' and sources.dec2000 = '';"
		mysqlquery,SQLhandle,q,names
	endif else begin
		names=[name]
	endelse

	for i=0L,n_elements(names)-1 do begin
		if keyword_set(simbadname) then n = simbadname else n = names[i]
		querysimbad,n,ra,dec,id
		if ~(keyword_set(ra)) or ~(keyword_set(dec)) then begin
			message," Couldn't resolve "+names[i],/info
			stop
			continue
		endif

		ras =  quote((sixty_string(ra/15))[0])
		decs = quote((adstring(dec))[0])
		ref = quote('SIMBAD, id '+id)



		q = "update sources  set ra2000="+ras+", dec2000="+decs+", reference_radec="+ref+" where name="+quote(names[i])+";"
		print,q
		mysqlcmd,SQLhandle,q
	endfor 


end
