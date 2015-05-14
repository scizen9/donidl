;+
; NAME: query_usnob 
;
; 	Requires the 'queryvizier' procedure from Goddard IDLAstro, and others.
;
; USAGE:
;   query_usnob,name,Bmag,Rmag,Imag,radius=radius,stop=stop,auto=auto
;
;
; INPUTS:
; 	name	Simbad-resolvable object name or coordinates
; KEYWORDS:
; 	id=		2MASS object ID (used instead of name)
; 	radius=	Radius around that object to search; same as on the
; 			Vizier query page. In units of arcseconds.
; 	/stop	Stop at a breakpoint after doing the query
; 	/auto	Return after an error. By default, it stops after an error.
; OUTPUTS:
;
; HISTORY:
; 	Began 2005-10-07 11:31:32 by Marshall Perrin 
; 			based on Keck's findttref.pro
;-



PRO query_usnob,name,radius=radius,B,R,I,$
	ra=ra,dec=dec,id=id,results=results,$
	count=count,auto=auto

	;,status=status
	if ~(keyword_set(radius)) then radius=30. ; arcsec

	if ~(keyword_set(ra)) and ~(keyword_set(dec)) then begin
		; figure out what and where the object is
		querysimbad,name,RA,DEC,idName,found=found
		if found eq 0 then message,"SIMBAD doesn't know that target name!"
	endif else  begin
		if size(ra,/tname) eq "STRING" then ra = ten_string(ra)*15
		if size(dec,/tname) eq "STRING" then dec = ten_string(dec)

	endelse
	print, "   "+name+" : "+ adstring(ra,dec,1)

	; find 2mass things within the cutoff. 
	results = queryvizier('USNO-B1',name,radius/60.)
	nres = n_elements(results)
	count=nres
	print,"Found "+strc(nres)+" matches."
	if ((size(results))[0] eq 0) then begin
		message,"No sources found within that radius!",info=auto
		if (keyword_set(auto)) then return
	endif

	; figure out the distances from the 2mass results to the SIMBAD location
	dist = sphdist(RA,DEC,results.raj2000,results.dej2000,/degrees)*3600.
	; This appears to be exactly equivalent to the above. 
	;  GCIRC,1,ra/15,dec, results.raj2000/15,results.dej2000,sep
	
	
	; Average the two USNO color measurements  
     usno_rmag = (results.r1mag + results.r2mag)/2.      ; R magnitude (R1 or R2)
      usno_bmag = (results.b1mag + results.b2mag)/2.      ; R magnitude (B1 or B2)
      w1 = WHERE(FINITE(results.r2mag) ne 1)
      if w1[0] ne -1 then usno_rmag[w1] = results[w1].r1mag
      w2 = WHERE(FINITE(results.r1mag) ne 1)
      if w2[0] ne -1 then usno_rmag[w2] = results[w2].r2mag
      
      w3 = WHERE(FINITE(results.b2mag) ne 1)
      if w3[0] ne -1 then usno_bmag[w3] = results[w3].b1mag
      w4 = WHERE(FINITE(results.b1mag) ne 1)
      if w4[0] ne -1 then usno_bmag[w4] = results[w4].b2mag


	whereismin,dist,imin

	print,"Min. distance is "+strc(dist[imin])+" arcsec"
	print," Values: "
	;r = results[imin]
	print,"b: ",usno_bmag[imin]
	print,"r: ",usno_rmag[imin]
	print,"i: ",results[imin].imag
	b = usno_bmag[imin]
	r = usno_rmag[imin]
	i = results[imin].imag

	results = results[imin]

	if keyword_set(stop) then stop

end
