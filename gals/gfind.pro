function gfind,gal,count=count,ra=ra,dec=dec,srad=srad,items=items, $
	status=status, silent=silent
;+
;	gfind - find item in the galdb_info struct galdat
;
; INPUTS:
;	gal	- galaxy name
;
; KEYWORDS:
;	count	- number of entries found (integer)
;	ra,dec	- coords to use if name not found (decimal degrees, J2000)
;	srad	- search radius for coords (arcsec)
;	items	- items to print from galdat, (string array)
;	status	- flag for results: 0 name found, 1+ coord match, -1 not found
;	silent	- supress output
;
; HISTORY:
;	20-MAR-2011	- Initial Revision, JDN
;-
common galdb_info
status = -1
;
; search on given name first
ind=where(strcmp(strtrim(galdat.id,2),gal) eq 1, n)
;
; now check alternatives
if n le 0 then $	; altids
	ind=where(strpos(galdat.altids,'|'+gal+'|') ge 0, n)
if n le 0 then $	; hlname
	ind=where(strcmp(strtrim(galdat.id,2),get_hl_name(gal)) eq 1,n)
if n le 0 then $	; alt hlname
	ind=where(strpos(galdat.altids,'|'+get_hl_name(gal)+'|') ge 0,n)
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
		gcirc,2,ra,dec,galdat.ra,galdat.dec,r
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
			gcirc,2,ra,dec,galdat[ind].ra,galdat[ind].dec,r
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
; print items
if n gt 0 then $
	gprint,ind,items=items
;
count=n
return,ind
end
