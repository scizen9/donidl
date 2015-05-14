pro gx_read_gsp,file,id,obj,objerr,wav,dga,exptime,xorms,yorms,mdres,visit, $
	subvis, ra=ra,dec=dec
;+
; GX_READ_GSP
;
;	gx_read_gsp, gsp_file, specid, obj, objerr, wav, dga, exptime, xorms,
;		yorms, mdres, ra=ra, dec=dec
;
; INPUTS:
;
;	gsp_file	- name of *-xg-gsp.fits file
;
; INPUT/OUTPUT:
;
;	specid		- id of object of interest on input
;			  if keywords ra and dec are set this is an output
;
; OUTPUT:
;
;	obj		- flux (pho/sec/cm^2/Ang)
;	objerr		- error on the flux
;	wav		- wavelengths (Ang)
;	dga		- detector grism angle (degrees)
;	exptime		- exposure time (seconds)
;	xorms		- x offset rms (arcsec)
;	yorms		- y offset rms (arcsec)
;	mdres		- median residual (arcsec)
;	visit		- visit number
;	subvis		- subvisit number
;
; KEYWORDS:
;
;	ra,dec		- coords of object; when supplied, these are used to
;				select object
;
; HISTORY:
;
;	19sep07 jdn	- initial version
;	20sep07 jdn	- dga keyword
;-
;
if n_params(0) lt 1 then begin
	print, 'Usage: gx_read_gsp, gsp_file, specid, flx, flxerr, wave'
	return
endif
;
; get header
hdr=headfits(file)
;
; read data
data=mrdfits(file,1,h,/silent)
;
; check if coords are specified
gid = -1
if n_elements(ra) eq 1 and n_elements(dec) eq 1 then begin
	gcirc,2,ra,dec,data.alpha_j2000,data.delta_j2000,dis
	t=where(dis eq min(dis))
	if dis(t(0)) lt 10. then begin
		gid = data(t(0)).id
	endif else begin
		print,'No match at coords: ',ra,dec
		id=-1
	endelse
endif else gid = id
;
t=where(data.id eq gid, n)
if n le 0 then begin
	print,'ID not found: ',id
	return
endif
if n gt 1 then begin
	print,'Multiple objects with same ID: ',id
	return
endif
;
t=t(0)
obj = data(t).obj
objerr = data(t).objerr
wav = findgen(n_elements(obj)) * data(t).disp + data(t).zero
dga = sxpar(hdr,'DGA')
exptime = sxpar(hdr,'EXPTIME')
xorms = sxpar(hdr,'NSXORMS')
yorms = sxpar(hdr,'NSYORMS')
mdres = sxpar(hdr,'NSMDNRES')
visit = sxpar(hdr,'VISIT')
subvis = sxpar(hdr,'SUBVIS')
;
id = gid
;
return
end
