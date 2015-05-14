pro gx_read_pri,file,id,phot,resp
;+
; GX_READ_PRI
;
;	gx_read_pri, pri_file, specid, photim, respim
;
; INPUTS:
;
;	pri_file	- name of *-[n|f]g-pri.fits file
;	specid		- id of object of interest
;
; OUTPUT:
;
;	phot		- accumulated photon image
;	resp		- detector response
;
; HISTORY:
;
;	20sep07 jdn	- intial revision
;-
;
if n_params(0) lt 1 then begin
	print, 'Usage: gx_read_pri, pri_file, specid, photim, respim'
	return
endif
;
; init
phot = fltarr(1000,2)
resp = phot
;
; read gsp file
gspf = file
strput,gspf,'gsp',strpos(gspf,'pri')
if file_exist(gspf) then begin
	gsp = mrdfits(gspf,1,hgsp,/silent)
	t=where(gsp.id eq id, n)
endif else begin
	print,'gsp file not found: ',gspf
	return
endelse
;
if n le 0 then begin
	print,'ID not found: ',id
	return
endif
if n gt 1 then begin
	print,'Multiple objects with same ID: ',id
	return
endif
;
; read data
if file_exist(file) then begin
	data = mrdfits(file,t(0)+1,h,/silent)
;
; reform into image
	phot = reform(data.dat,sxpar(h,'PRI_NC'),sxpar(h,'PRI_NR'))
	resp = reform(data.rsp,sxpar(h,'PRI_NC'),sxpar(h,'PRI_NR'))
endif
;
return
end
