pro glga_montage,lfile,sdss=sdss,twomass=twomass,pxscl=pxscl,clean=clean, $
	silent=silent
;+
; glga_montage - call Montage routines to build a mosaic of galaxies in lfile
;
; lfile - list of objects with one line per object with these columns:
;	id
;	ra,dec	- degrees
;	majdiam,mindiam	- arcmin
;	pa	- degrees
;	type	- galaxy type
;
; keywords:
;	sdss,twomass - survey to gather images from
;	pxscl - set to force pixel scale (arcseconds), else uses defaults
;	clean - set to delete intermediate files
;	silent	- supress output
;-
;
; set defaults and check keywords
ddir='sdss/'
fpx='sdss'
dpsc=0.5	; default pixel scale
nproc = 5	; number that should be processed
if keyword_set(sdss) then begin
	ddir='sdss/'
	fpx='sdss'
	dpsc=0.5
	nproc = 5
endif
if keyword_set(twomass) then begin
	ddir='2mass/'
	fpx='2mass'
	dpsc=1.0
	nproc = 3
endif
;
; are we using a fixed pixel scale?
if keyword_set(pxscl) then dpsc = pxscl
;
; should we clean?
if keyword_set(clean) then $
	docln = ' clean' $
else	docln = ''
;
; read in sample data
readcol,lfile, id, ra, dec, majdiam, mindiam, pa, type, $
	format='a,d,d,f,f,f,a', /silent
;
; get output filenames
temp = lfile
rute = gettok(temp,'.')
mfile = rute + '_made.log'
filestamp,mfile,/arch,/verbose
openw,ml,mfile,/get_lun
printf,ml,'# GLGA_MONTAGE: '+systime(0)
printf,ml,'# IMAGES MADE'
printf,ml,'# Input list  : '+lfile
printf,ml,'# Input survey: '+strupcase(fpx)
nfile = rute + '_notmade.log'
filestamp,nfile,/arch,/verbose
openw,nl,nfile,/get_lun
printf,nl,'# GLGA_MONTAGE: '+systime(0)
printf,nl,'# IMAGES NOT MADE'
printf,nl,'# Input list  : '+lfile
printf,nl,'# Input survey: '+strupcase(fpx)
;
; 3 arcmin is minimum size
msize=(majdiam*4.0)>3.
bg = where(majdiam ge 50., nbg)
if nbg gt 0 then msize[bg] = majdiam[bg] * 1.5
bg = where(majdiam lt 50. and majdiam ge 30., nbg)
if nbg gt 0 then msize[bg] = majdiam[bg] * 2.0
bg = where(majdiam lt 30 and majdiam ge 20., nbg)
if nbg gt 0 then msize[bg] = majdiam[bg] * 2.5
bg = where(majdiam lt 20 and majdiam ge 10., nbg)
if nbg gt 0 then msize[bg] = majdiam[bg] * 3.0
;
; glga data directory
deg = string(floor(ra), format='(i3.3)')+'D'
;
; used to generate file names
id = strcompress(id,/rem)

dirbase=!GLGA_ROOT+'data/'+deg+'/'+ddir+'fits/'
;
; number of galaxies
nloop = n_elements(id)
;
; loop over object list
for i=0L, nloop-1 do begin
	;
	; set default pixel scale in arcseconds
	psc = dpsc
	;
	; if fixed pixel scale not requested
	; calculate for each image
	if not keyword_set(pxscl) then begin
		asize = (msize[i]*60. / psc)
		;
		; be sure image is less than 6k x 6k
		while asize gt 6000. do begin
			;
			; increment by 0.5 arcsec
			psc = psc + 0.5
			asize = (msize[i]*60. / psc)
		endwhile
	endif
	;
	; print status
	if not keyword_set(silent) then $
		print,i+1,'/',nloop,id[i],deg[i],psc, $
		format = '(i6,a1,i6,2x,a-25,a5,f7.3)'
	;
	; spawn command
	cmd = 'cd ' + dirbase[i] + ' ; ' + fpx+'all ' + id[i] + ' ' + $
		strtrim(string(ra[i],form='(f13.8)'),2) + ' ' + $
		strtrim(string(dec[i],form='(f13.8)'),2) + ' ' + $
		strtrim(string(msize[i]/60.,form='(f9.6)'),2) + ' ' + $
		strtrim(string(psc,form='(f5.2)'),2) + docln
	spawn,cmd
	;
	; check results
	plist = file_search(dirbase[i] + id[i] + '_*.fits.gz', count=np)
	if np gt 0 then begin
		if np eq nproc then $
			otyp = type[i] $
		else	otyp = type[i]+'probs'
		printf,ml,id[i],ra[i],dec[i],majdiam[i],mindiam[i],pa[i],otyp, $
			format='(a-25,2f13.8,3f9.3,2x,a)'
	endif else $
		printf,nl,id[i],ra[i],dec[i],majdiam[i],mindiam[i],pa[i], $
			type[i], format='(a-25,2f13.8,3f9.3,2x,a)'
	flush,ml,nl
;
endfor	; loop over object list
free_lun,ml,nl
;
return
;
end
