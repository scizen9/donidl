function hlfind,ra,dec,hldata=hldata,name=name,diam=diam,verbose=verbose
;+
;	Find Hyper LEDA data structure index
;
; INPUTS:
;	ra,dec	- galaxy coords in decimal degrees (j2000)
;
; optional KEYWORDS:
;	hldata	- the hldata structure, passed to save loading for each search
;	name	- use if coords unknown
;	diam	- search radius in arcmin (defaults to 1.)
;	verbose	- print some data
;
;-
; make sure data available
if keyword_set(hldata) then begin
	if n_elements(hldata) lt 0 then $
		restore,!CAT_DATA+'hl_master_structs.sav'
endif else	restore,!CAT_DATA+'hl_master_structs.sav'
;
; default not found
m = -1L
;
; are we doing a name search?
if keyword_set(name) then begin
	host=strtrim(name,2)
	hlhost=get_hl_name(host)
;
; check name
	h=where(strcmp(hldata.objname,hlhost) eq 1, nh)
	if nh eq 1 then begin
		m=h[0]
;
; check alt names
	endif else begin
		h=where(strcmp(hldata.altnames,hlhost) eq 1, nh)
		if nh eq 1 then m=h[0]
	endelse
;
; no, were doing a position search
endif else begin
	if keyword_set(diam) then $
		rlim = 60.*diam/2. $	; radius in arcseconds
	else	rlim = 30.
;
; get hldata
	gcirc,2,ra,dec,hldata.al2000*15.d0,hldata.de2000,r
	w=where(r le rlim, nw)
;
; coord match
	if nw ge 1 then begin
	;
	; more than one match
		if nw gt 1 then begin
		;
		; else get closest in position
			r=r[w]
			s=sort(r)
			w=w[s]
			if keyword_set(verbose) then $
				print,'Host with neighbor: ',host,' ',hlhost
		endif
		m=w[0]
	endif
endelse
if keyword_set(verbose) and m lt 0 then $
	print,'Host not found.'
;
return,m
end
