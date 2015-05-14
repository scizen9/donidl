pro gx_read_xsp,file,id,offset,cnts,errs,ra=ra,dec=dec
;+
; GX_READ_PRI
;
;	gx_read_xsp, xsp_file, specid, photim, respim
;
; INPUTS:
;
;	file	- name of *-[n|f]g-xsp.fits file
;	id	- id of object of interest
;
; OUTPUT:
;
;	offset	- offset in arcsec
;	cnts	- extracted counts per second
;	errs	- errors, 1-sigma in counts
;
; KEYWORDS:
;
;       ra,dec          - coords of object; when supplied, these are used to
;                               select object
;
; HISTORY:
;
;	20sep07 jdn	- intial revision
;-
;
if n_params(0) lt 1 then begin
	print, 'Usage: gx_read_xsp, xsp_file, specid, offset, counts, errs [, ra=ra, dec=dec ]'
	return
endif
;
; read gsp file
gspf = file
strput,gspf,'gsp',strpos(gspf,'xsp')
hgsp = headfits(gspf)
gsp = mrdfits(gspf,1,h,/silent)
;
; get flux fraction (from profile model)
ffrac = sxpar(hgsp,'FLXFRAC')
;
; check if coords are specified
gid = -1
if n_elements(ra) eq 1 and n_elements(dec) eq 1 then begin
	gcirc,2,ra,dec,gsp.alpha_j2000,gsp.delta_j2000,dis
	t=where(dis eq min(dis))
	if dis(t(0)) lt 10. then $
		gid = gsp(t(0)).id
endif else gid = id
;
t=where(gsp.id eq gid, n)
;
if n le 0 then begin
	print,'ID not found: ',gid
	return
endif
if n gt 1 then begin
	print,'Multiple objects with same ID: ',gid
	return
endif
;
; read data
data = mrdfits(file,t(0)+1,h,/silent)
;
; place in arrays
offset = data.offset
cnts   = data.obj / ffrac
errs   = data.objerr
id = gid
;
return
end
