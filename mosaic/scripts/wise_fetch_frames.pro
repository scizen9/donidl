pro wise_fetch_frames,lfile,nosizelim=nosizelim
;+
; wise_fetch_frames - get frames direct from ipac
;
; lfile - galaxy list with one entry per galaxy with the following columns:
;	id
;	ra,dec	- decimal degrees
;	majdiam,mindiam	- arcminutes
;	pa	- position angle in degrees
;	type	- galaxy Hubble type (string)
;
;-
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
; directories
sortdir=!GLGA_WISE_DATA+'data/sort/'
;
; script file
temp = lfile
rute = gettok(temp,'.')
sfil = rute + '_fetch.csh'
openw,sl,sfil,/get_lun
printf,sl,'# WISE_FETCH_FRAMES: '+systime(0)
;
for i=0L,nobj-1L do begin
	; check for existing directory
	if file_test(sortdir+name[i]) ne 0 then $
		printf,sl,'mv '+sortdir+name[i]+' '+sortdir+name[i]+'_old'
	printf,sl,'getframesAllSky -ra ' + $
		strtrim(string(ra[i],form='(f13.6)'),2) + ' -dec ' + $
		strtrim(string(dec[i],form='(f13.6)'),2) + ' -sx ' + $
		strtrim(string(r[i],form='(f9.3)'),2) + ' -sy ' + $
		strtrim(string(r[i],form='(f9.3)'),2) + ' -outd ' + $
		sortdir+name[i] + ' -bands 1,2,3,4 -moo 25 -ant 2000 -log ' + $
		name[i] + '_fetch.log'
endfor
free_lun,sl
;
return
end
