;+
; NAME: query_2mass 
;
; 	Given a name, query the 2MASS catalog on Vizier for its
; 	J, H, and Ks magnitudes (and their errors).
;
; 	Requires the 'queryvizier' procedure from Goddard IDLAstro, and others.
;
; USAGE:
;   query_2mass,name,J,H,Ks,Jerr,Herr,Kserr,radius=radius,stop=stop,auto=auto
;
; INPUTS:
; 	name	Simbad-resolvable object name or coordinates
; KEYWORDS:
; 	radius=	Radius around that object to search; same as on the
; 			Vizier query page. In units of arcseconds.
; 	/stop	Stop at a breakpoint after doing the query
; 	/auto	Return after an error. By default, it stops after an error.
; OUTPUTS:
; 	J,H,Ks				Magnitudes, from 2MASS, for the object with 
; 						the closest location to the requested name.
; 	Jerr,Herr,Kserr		Errors in Magnitudes
;
; HISTORY:
; 	Began 2005-10-07 04:00:19 by Marshall Perrin 
; 		Based on Keck's 'findttref.pro'
; 		2006-06-26	Added checks for flux## instead of fnu_## because
; 			sometimes (for no apparent reason) that's what gets returned.
; 			And for "IRAS" versus "NAME" for the IRAS ID.
;-

PRO query_iras,name,radius=radius,iras12,iras25,iras60,iras100,stop=stop,auto=auto,status=status,$
	iras=iras

	status=0
	if ~(keyword_set(radius)) then radius=120. ; arcsec

		iras12=!values.f_nan
		iras25=!values.f_nan
		iras60=!values.f_nan
		iras100=!values.f_nan
		iras12err=!values.f_nan
		iras25err=!values.f_nan
		iras60err=!values.f_nan
		iras100err=!values.f_nan


	; figure out what and where the object is
	querysimbad,name,RA,DEC,idName,found=found
	if found eq 0 then begin
		message,"SIMBAD doesn't know that target name!",info=auto
		if (keyword_set(auto)) then return
	endif
	print, "   "+name+" : "+ adstring(ra,dec,1)


	; find iras things within the cutoff. 
	results = queryvizier('IRAS',name,radius/60.)
	nres = n_elements(results)
	print,"Found "+strc(nres)+" matches."
	if ((size(results))[0] eq 0) then begin
		message,"No sources found within that radius!",info=auto
		if (keyword_set(auto)) then return
	endif

	; figure out the distances from the 2mass results to the SIMBAD location
	ra1950 = ten_string(results.ra1950)*15
	dec1950 = ten_string(results.de1950)
	jprecess,ra1950,dec1950,raj2000,dej2000
	dist = sphdist(RA,DEC,raj2000,dej2000,/degrees)*3600.

	; This appears to be exactly equivalent to the above. 
	;  GCIRC,1,ra/15,dec, results.raj2000/15,results.dej2000,sep
			        

	whereismin,dist,imin

	print,"Min. distance is "+strc(dist[imin])+" arcsec"
	print," Values: "
	r = results[imin]

	t = tag_names(r)
	wn = where(t eq "FNU_12",ct)
	if ct gt 0 then iras12 = r.fnu_12
	wn = where(t eq "FLUX12",ct)
	if ct gt 0 then iras12 = r.flux12
	wn = where(t eq "FNU_25",ct)
	if ct gt 0 then iras25 = r.fnu_25
	wn = where(t eq "FLUX25",ct)
	if ct gt 0 then iras25 = r.flux25
	wn = where(t eq "FNU_60",ct)
	if ct gt 0 then iras60 = r.fnu_60
	wn = where(t eq "FLUX60",ct)
	if ct gt 0 then iras60 = r.flux60
	wn = where(t eq "FNU_100",ct)
	if ct gt 0 then iras100 = r.fnu_100
	wn = where(t eq "FLUX100",ct)
	if ct gt 0 then iras100 = r.flux100
				
	print,"Fnu_12: ",iras12
	print,"Fnu_25: ",iras25
	print,"Fnu_60: ",iras60
	print,"Fnu_100: ",iras100

	wn = where(t eq "IRAS",ct)
	if ct gt 0 then iras ="IRAS "+r.iras
	wn = where(t eq "NAME",ct)
	if ct gt 0 then iras = "IRAS "+r.name
	
	
	status=1
	if keyword_set(stop) then stop

end
