;+
; NAME: mdb_getcoords 
;
;	Find coordinates for an object. 
;	First tries looking up the object in the database.
;	Then tries resolving it from SIMBAD.
;
; INPUTS:
; 	simbadname	use this name for lookups in simbad, instead of name
; KEYWORDS:
; 	/ten	return DECIMAL DEGREES (instead of hexagesimal HHMMSS+-DDMMSS)
; 	/print	print output
; 	simbadname=		override default name for simbad query
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-05-10 00:58:13 by Marshall Perrin 
;-

PRO mdb_getcoords,name0,ra,dec,print=print,simbadname=simbadname,ten=ten
	common mysql, SQLhandle
	mysqlcheck,SQLhandle

	name = mdb_nameresolve(name0)

	q = "SELECT ra2000,dec2000 from sources where name="+quote(name)+";"
	mysqlquery,SQLhandle,q,ra,dec,format="A,A",ngood=count

	if count eq 0 then begin
		message,/info,"That object not in database; querying SIMBAD instead!"
		querysimbad,name,ra,dec,found=count
		if count then goto,matchfound
			
		; if we have failed thus far to find the coords, try looking it up in
		; SIMBAD.  
		; If the user explicitly provided a simbad name, use that.
		if keyword_set(simbadname) then begin
			name=simbadname
			querysimbad,name,ra,dec,found=count
			if count then goto,matchfound
		endif
						
		while count eq 0 do begin
			print, "Try entering a new name for simbad: "
			print, "set name= [NEW VALUE] then .cont"
			stop
			querysimbad,name,ra,dec,found=count
			if count then goto,matchfound
		endwhile
	endif

	if count ne 1 then message,"Got an odd number of results for that query... ?"
	if keyword_set(print) then print,"RESULT: "+ra+" "+dec

	matchfound:

ras = ra
decs = dec

	ra = ra[0]
	dec=dec[0]

	; convert to decimal degrees
	if keyword_set(ten) then begin
		ra = ten_string(ra)/15
		dec = ten_string(dec)
	endif

		; not necessary - database is in sexagesimal
		;ra = sixty_string(ra/15)
		;dec = sixty_string(dec)
	return
	
end
