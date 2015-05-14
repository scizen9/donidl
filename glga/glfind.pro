function glfind,gal,count=count,ra=ra,dec=dec,srad=srad,status=status, $
	silent=silent
;+
;	glfind - find item in the glga_info struct glgadat
;
; INPUTS:
;	gal	- galaxy name
;
; KEYWORDS:
;	count	- number of entries found (integer)
;	ra,dec	- coords to use if name not found (decimal degrees, J2000)
;	srad	- search radius for coords (arcsec)
;	status	- flag for results: 0 name found, 1+ coord match, -1 not found
;	silent	- supress output
;
; HISTORY:
;	14-FEB-2013	- Initial Revision, JDN
;-
common glgadb_info
status = -1
;
; search on given name first
ind=where(strcmp(glgadat.id,strtrim(gal,2)) eq 1, n)
;
; got one and only one name match
if n eq 1 then begin
	status = 0
;
; got no name matches
endif else if n le 0 then begin
	;
	; check coords
	if keyword_set(ra) and keyword_set(dec) then $
	if ra ge 0. and dec ge -90. then begin
		if keyword_set(srad) then $
			rlim = srad $
		else	rlim = 10.
		gcirc,2,ra,dec,glgadat.ra,glgadat.dec,r
		ind=where(r le rlim, n)
		status = n
		;
		; check matches
		if n ge 1 then begin
			;
			; more than one: get closest
			if n gt 1 then begin
				r=r[ind]
				s=sort(r)
				ind=ind[s[0]]
			endif
			n = 1
		endif	else status = -1	; none found
	endif
;
; multiple name matches
endif else if n gt 1 then begin
	;
	; check coords
	if keyword_set(ra) and keyword_set(dec) then begin
		if ra ge 0. and dec ge -90. then begin
			gcirc,2,ra,dec,glgadat[ind].ra,glgadat[ind].dec,r
			s=sort(r)
			ind = ind[s[0]]
		endif else begin
			ind = ind[0]
		endelse
	;
	; no coords, pick one
	endif else begin
		ind = ind[0]
	endelse
	status = n
	n = 1
endif
;
; no coords, then not found
if n le 0 then if not keyword_set(silent) then print,gal,' not found.'
;
count=n
return,ind
end
