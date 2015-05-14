pro sdss_get_driver,lfile,ptslim,nosizelim=nosizelim
;+
; sdss_get_driver - write out foot files for upload to:
;  	http://das.sdss.org/www/html/post_coords.html
;	click on the link under 'Download Selection'
;	select the following data items: 
;		all filters
;		drField
;		corr
;		tsField
;		wget
;	click 'request'
; then cd to raw data directory and type:
;	wget -nc -i <sdss-wget-file.lis>
;		(the -nc prevents re-downloading existing files)
; lfile - galaxy list with one entry per galaxy with the following columns:
;	id
;	ra,dec	- decimal degrees
;	majdiam,mindiam	- arcminutes
;	pa	- position angle in degrees
;	type	- galaxy Hubble type (string)
;
; ptslim - max number of points per foot file (default 2500)
;-
if n_params(0) lt 2 then $
	ptslim = 2500L
; read input
readcol,lfile,id,ra,dec,majdiam,mindiam,pa,format='a,d,d,f,f,f'
nobj = n_elements(ra)
name = strtrim(id,2)
;
; check size limit
if keyword_set(nosizelim) then $
	szlim = 1.e9 $
else	szlim = 26.
;
; convert from arcmin to degrees: 3 arcmin is min and szlim is max size
r=((majdiam*4.)>3.<szlim) / 60.
;
;
objid=findgen(nobj)
cover_points,ra,dec,r,objid,allobj,allcoo
npts=n_elements(allobj)
;
ptsw = 0L	; number of points written
nfil = 1L	; number of files created
for j=0L,nobj-1L do begin
	if ptsw eq 0L then begin
		ofil = !GLGA_SDSS_DATA + 'work/foot' + strn(nfil) + '.list'
		filestamp,ofil
		if nfil gt 1 then free_lun,ol
		openw,ol,ofil,/get_lun
		printf,ol,'ra,dec'
		nfil = nfil + 1L
	endif
	t=where(allobj eq float(j), nt)
	if nt gt 0 then begin
	    for i=0L,nt-1 do $
		printf,ol,allcoo[t[i],0],allcoo[t[i],1],format='(F8.4,",",F8.4)'
	    ptsw = ptsw + nt
;	    if ptsw gt 25 then ptsw = 0L	; for stripe 82
	    if ptsw gt ptslim then ptsw = 0L
	endif
endfor
free_lun,ol
;
ofil = !GLGA_SDSS_DATA + 'work/objmultipos.list'
filestamp,ofil
openw,ol,ofil,/get_lun
printf,ol,'id,ra,dec,name'
for i=0L,npts-1 do $
	printf,ol,allobj[i],allcoo[i,0],allcoo[i,1],name[allobj[i]], $
		format='(I5," ",F8.4," ",F8.4," ",A)'
free_lun,ol
;
return
end
