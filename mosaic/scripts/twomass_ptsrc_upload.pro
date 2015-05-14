pro twomass_ptsrc_upload,lfile
;+
; twomass_ptsrc_upload - write out search params for GATOR cat search:
;  	http://irsa.ipac.caltech.edu/cgi-bin/Gator/nph-scan?submit=Select&projshort=2MASS
;
; lfile - galaxy list with one entry per galaxy with the following columns:
;	id
;	ra,dec	- decimal degrees
;	majdiam,mindiam	- arcminutes
;	pa	- position angle in degrees
;
; writes out file with same root as lfile, but with .gator extension
;-
; read input
readcol,lfile,id,ra,dec,majdiam,mindiam,pa,format='a,d,d,f,f,f'
;
; convert from arcmin to arcsec, 1 deg is largest size
srad=((majdiam*2.0)<60.) * 60.
;
; this ratio is min/maj diam
rat = mindiam/majdiam
;
; check for bad pa's
b=where(pa lt 0, nb)
if nb gt 0 then pa[b]=0.
;
name = strtrim(id,2)
nobj = n_elements(ra)
;
rute=strmid(lfile,0,strpos(lfile,'.',/reverse_search))
ofile=rute+'.gator'
filestamp,ofile
openw,ol,ofile,/get_lun
printf,ol,'ra,dec,object,major,ratio,angle'
;
c=','
for j=0L,nobj-1L do begin
	printf,ol,strtrim(string(ra[j],form='(f13.8)'),2)+c+ $
		  strtrim(string(dec[j],form='(f13.8)'),2)+c+name[j]+c+ $
		  strtrim(string(srad[j],form='(f9.3)'),2)+c+ $
		  strtrim(string(rat[j],form='(f9.3)'),2)+c+ $
		  strtrim(string(pa[j],form='(f9.3)'),2)
endfor
free_lun,ol
;
;
return
end
