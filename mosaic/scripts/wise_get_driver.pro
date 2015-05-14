pro wise_get_driver,lfile
;+
; wise_get_driver - write out irsa *.tbl files
;-
; read input
readcol,lfile,id,ra,dec,majdiam,format='a,d,d,f'
;
; convert from arcmin to arcsec: 3 arcmin is min size
r=(majdiam*4.0)>3.
r=r * 60.
;
name = strtrim(id,2)
nobj = n_elements(ra)
;
; output file
temp = lfile
rute = gettok(temp,'.')
ofil = rute + '.tbl'
filestamp,ofil
openw,ol,ofil,/get_lun
;
; header
printf,ol,'| object          | ra          | dec        | subsize   |'
printf,ol,'| char            | double      | double     | double    |'
printf,ol,'|                 | degree      | degree     | arcsecond |'
;
for i=0L,nobj-1L do $
	printf,ol,id[i],ra[i],dec[i],r[i], $
		form='(1x,a-17,f11.6,3x,f11.6,2x,f8.0)'
free_lun,ol
;
;
return
end
