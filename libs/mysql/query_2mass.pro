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
; 	id=		2MASS object ID (used instead of name)
; 	radius=	Radius around that object to search; same as on the
; 			Vizier query page. In units of arcseconds.
; 	/stop	Stop at a breakpoint after doing the query
; 	/auto	Return after an error. By default, it stops after an error.
; OUTPUTS:
; 	J,H,Ks				Magnitudes, from 2MASS, for the object with 
; 						the closest location to the requested name.
; 	Jerr,Herr,Kserr		Errors in Magnitudes
; 	results=			keyword to return the full 2mass results structure
;
; HISTORY:
; 	Began 2005-10-07 04:00:19 by Marshall Perrin 
; 		Based on Keck's 'findttref.pro'
;-

PRO query_2mass,name,radius=radius,J,H,Ks,Jerr,Herr,Kserr,stop=stop,auto=auto,id=id,results=r,$
		count=count

	if ~(keyword_set(radius)) then radius=10. ; arcsec

		J=!values.f_nan
		H=!values.f_nan
		Ks=!values.f_nan
		Jerr=!values.f_nan
		Herr=!values.f_nan
		Kserr=!values.f_nan
		
		
	if ~(keyword_set(id)) then begin
		; figure out what and where the object is
		querysimbad,name,RA,DEC,idName,found=found
		if found eq 0 then begin
			message,"SIMBAD doesn't know that target name!",info=auto
			if (keyword_set(auto)) then return
		endif
		print, "   "+name+" : "+ adstring(ra,dec,1)

		; find 2mass things within the cutoff. 
		results = queryvizier('2MASS-PSC',name,radius/60.,/all)
	endif else begin
		webid = strepex(id,"\+","%02B")
		results = queryvizier('2MASS-PSC',name,radius/60.,constraint='2MASS='+webid,/all)
		stop
	endelse
	
	nres = n_elements(results)
	count=nres
	print,"Found "+strc(nres)+" matches."
	if ((size(results))[0] eq 0) then begin
		message,"No sources found within that radius!",info=auto
		if (keyword_set(auto)) then return
	endif

	if keyword_set(id) then begin
		; select one based on 2MASS id
		w = where(results._2MASS eq id,wcnt)
		if wcnt ne 1 then message,"Can't find a match for ID = "+id
		r = results[w[0]]
	endif else begin
		; select the closest
		
		; figure out the distances from the 2mass results to the SIMBAD location
		dist = sphdist(RA,DEC,results.raj2000,results.dej2000,/degrees)*3600.

		; This appears to be exactly equivalent to the above. 
		;  GCIRC,1,ra/15,dec, results.raj2000/15,results.dej2000,sep
			        
		whereismin,dist,imin

		print,"Min. distance is "+strc(dist[imin])+" arcsec"
		print," Values: "
		r = results[imin]
	endelse

	print,"J: ",r.jmag,r.e_jmag
	print,"H: ",r.hmag,r.e_hmag
	print,"Ks:",r.Kmag,r.e_Kmag
	J = r.jmag
	Jerr = r.e_jmag
	H = r.hmag
	Herr = r.e_hmag
	ks = r.kmag
	kserr = r.e_kmag

	if keyword_set(stop) then stop

end
