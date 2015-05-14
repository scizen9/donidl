pro fitsed_zpeg, galid, glgaout=glgaout, test=test, yuan13=yuan13, $
	update=update, verbose=verbose, area=area
;+
;	fitsed_zpeg - fit the SED of a galaxy with zpeg
;
; CALLING SEQUENCE:
;	fitsed_zpeg,galid,/glgaout,test=<int>,/yuan13,update,/verbose
;
; INPUTS:
;	galid	- galaxy id as specified in galdat struct
;
; KEYWORDS:
;	glgaout	- put in/output files in !GLGA_ROOT degree dir
;	test	- integer minimum number of valid bandpasses required to fit
;	yuan13	- use extinction coeff's of Yuan et al. 2013
;	update	- only fit if GLGA photometry is newer than GLGA outputs
;	verbose	- extra output to screen
;
; CALLS:
;	get_bestfluxes to get fluxes to fit
;	plot_zspec if /glgaout is set
;
; HISTORY:
; $Id: fitsed_zpeg.pro,v 1.9 2013/09/03 15:36:23 neill Exp $
;	06-JUN-2013, jdn - Initial Revision
;	17-JUL-2013, jdn - Implemented update keyword
;-
common galdb_info
;
; get galaxy
if n_params(0) lt 1 then begin
	gal=''
	read,'Enter Galaxy id: ',gal
endif else gal = galid
;
; get fluxes
get_bestfluxes,gal,flux,flxer,waves,bwid,src,gphts, $
	yuan13=yuan13,status=stat,/silent
;
; check results
if stat ne 0 then begin
	print,'Not in gal and/or glga db: ',gal
	return
endif
;
; filter list for zpeg (only fits out to H-band)
zpegflts = [ $
	'GALEX_FUV.fil', 'GALEX_NUV.fil', $
	'UX_B90.fil','B_B90.fil','V_B90.fil', $
	'u_prime.fil','g_prime.fil','r_prime.fil','i_prime.fil','z_prime.fil', $
	'J.fil','H.fil' ]
nflt = n_elements(zpegflts)
;
; trim mags for zpeg
flux = flux[0:(nflt-1)]
flxer= flxer[0:(nflt-1)]
waves= waves[0:(nflt-1)]
;
; check good mags
good = where(flux gt 0., ngood)
if keyword_set(test) then $
	ntst = test $
else	ntst = 5
if ngood lt ntst then begin
	print,'Not enough good points: ',ngood
	return
endif
;
; get galdat index
g = gfind(gal)
;
; get directories
glga_dir = !GLGA_ROOT + 'data/'+glga_degdir(galdat[g].ra)+'/seds/zpeg/'
;
; get version string
version = '$Revision: 1.9 $'
junk = gettok(version,' ')
junk = gettok(version,' ')
version = junk
;
; area, default is all-sky
if keyword_set(area) then $
	sarea = area $
else	sarea = 1.
;
; output directory, tail
if keyword_set(glgaout) then begin
	odir = glga_dir
	tail = ''
endif else begin
	odir = './'
	tail = '_'+timestr()
endelse
id = gal+tail
;
; file names
cfil = odir+id+ '_zpeg.dat'
ofil = odir+id
pfil = odir+id+'.par.tmp'
lfil = odir+id+'.log'
;
; check for update only
if keyword_set(update) then begin
	;
	; output timestamp
	fi = file_info(ofil)
	;
	; if photometry is older, don't fit
	if gphts le fi.mtime then begin
		if keyword_set(verbose) then $
			print,'FIT up-to-date: ',id
		return
	endif
endif
;
; open phot file
openw,cl,cfil,/get_lun
;
; header
printf,cl,'## FITSED_ZPEG v'+version+': '+systime(0)
printf,cl,'## Testing Mags: at least '+strn(ntst)+' must be >= 0'
printf,cl,'# SURVEY_AREA '+strtrim(string(sarea),2)
printf,cl,'# FILTER_COMPLETENESS 10'
printf,cl,'# COMPLETENESS_LIMIT 22.0'				; arbitrary
;
; filters and their calibrations
flist = '# FILTERS '
calst = '# CALIBTYPES '
for i=0,nflt-1 do begin
	flist = flist + zpegflts[i]+', '
	calst = calst + 'FNU_JY, '
endfor
printf,cl,flist
printf,cl,calst
;
; object info prefix
fmt = '(a-'+strn(strlen(id)>25)+')'
mglst = string(gal,form=fmt)+' '+ $
	string((galdat[g].cz/!phys_c)>0.005,form='(f7.4)') + $
	string(galdat[g].tyn,form='(f12.2)') + $
	string(galdat[g].ra,form='(f16.8)') + $
	string(galdat[g].dec,form='(f16.8)') + '   '
;
; photometric data
for i=0,nflt-1 do $
    mglst = mglst + string(flux[i], form='(g15.3)') + '  ' + $
		    string(flxer[i],form='(g15.3)') + '  '
printf,cl,mglst

free_lun,cl
;
; run zpeg
cmd='zpeg -V '+cfil+' -o '+ofil+' -p '+!GALS_DATA+'zpeg/gals_zpegin.par -t '+ $
		 pfil+' |& tee '+lfil
print,cmd
spawn,cmd
;
; clean up
cmd='rm '+pfil
spawn,cmd
;
; check for glgaout
if keyword_set(glgaout) then $
	plot_zspec,gal,/glgaout,verbose=verbose
;
return
end
