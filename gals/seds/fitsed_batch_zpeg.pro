pro fitsed_batch_zpeg, yuan13=yuan13, verbose=verbose
;+
;	fitsed_batch_zpeg - fit the SED of all galaxies with zpeg
;
; CALLING SEQUENCE:
;	fitsed_batch_zpeg,/yuan13,/verbose
;
; INPUTS:
;	None.
;
; KEYWORDS:
;	yuan13	- use extinction coeff's of Yuan et al. 2013
;	verbose	- extra output to screen
;
; CALLS:
;	get_bestfluxes to get fluxes to fit
;
; HISTORY:
; $Id: fitsed_batch_zpeg.pro,v 1.6 2013/07/19 16:37:27 neill Exp $
;	19-JUL-2013, jdn - Initial Revision
;-
common galdb_info
common glgadb_info
;
; galaxy list
ngal = n_elements(glgadat)
;
; filter list for zpeg (only fits out to H-band)
zpegflts = [ $
	'GALEX_FUV.fil', 'GALEX_NUV.fil', $
	'UX_B90.fil','B_B90.fil','V_B90.fil', $
	'u_prime.fil','g_prime.fil','r_prime.fil','i_prime.fil','z_prime.fil', $
	'J.fil','H.fil' ]
nflt = n_elements(zpegflts)
ntst = 5
;
; file names
cfil = 'glga_zpeg.dat'
ofil = 'glga'
pfil = 'glga.par.tmp'
lfil = 'glga.log'
;
; area, default is all-sky
sarea = 1.
;
; get version string
version = '$Revision: 1.6 $ $Date: 2013/07/19 16:37:27 $'
junk = gettok(version,' ')
junk = gettok(version,' ')
version = junk
;
; open phot file
openw,cl,cfil,/get_lun
;
; header
printf,cl,'## FITSED_BATCH_ZPEG v'+version+': '+systime(0)
printf,cl,'## Number of galaxies examined: '+strtrim(string(ngal),2)
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
; record number of galaxies
nfit = 0L
;
; loop over galaxies
for i=0,ngal-1 do begin
	;
	; get galaxy name
	gal = glgadat[i].id
	;
	; get galdat index
	g = gfind(gal)
	;
	; get data directory
	if g lt 0 then begin
		gddir = glga_degdir(glgadat[i].ra)
		tystr = string(-99.,form='(f12.2)')
		czstr = string(0.005, form='(f7.4)')
	endif else begin
		gddir = glga_degdir(galdat[g].ra)
		tystr = string(galdat[g].tyn,form='(f12.2)')
		czstr = string((galdat[g].cz/!phys_c)>0.005, form='(f7.4)')
	endelse
	;
	; print report
	print,i+1,'/',ngal,gal,gddir,format='(i7,a1,i7,2x,a-25,2x,a)'
	;
	; get fluxes
	get_bestfluxes,gal,flux,flxer,waves,bwid,src,gphts, $
		yuan13=yuan13,status=stat,verbose=verbose
	;
	; check results
	if stat eq 0 then begin
		;
		; trim mags for zpeg
		flux = flux[0:(nflt-1)]
		flxer= flxer[0:(nflt-1)]
		waves= waves[0:(nflt-1)]
		;
		; check good mags
		good = where(flux gt 0., ngood)
		if ngood ge ntst then begin
			;
			; get glga directory
			glga_dir = !GLGA_ROOT + 'data/'+gddir+'/seds/zpeg/'
			;
			; test file
			tfil = glga_dir + gal
			;
			; do we have new photometry relative to current fit?
			fi = file_info(tfil)
			if gphts gt fi.mtime or gphts eq 0 then begin
				;
				; object info prefix
				fmt = '(a-'+strn(strlen(gal)>25)+')'
				mglst = string(gal,form=fmt)+' '+czstr+tystr + $
					string(glgadat[i].ra,form='(f16.8)') + $
					string(glgadat[i].dec,form='(f16.8)')+ $
					'   '
				;
				; photometric data
				for j=0,nflt-1 do $
    					mglst = mglst + string(flux[j], $
						form='(g15.3)') + '  ' + $
	    				string(flxer[j],form='(g15.3)') + '  '
				printf,cl,mglst
				nfit += 1L
			endif else print,'Already up-to-date: ',gal
		endif else print,'Not enough good points: ',gal,' ',ngood
	endif else print,'Not in gal and/or glga db: ',gal
	flush,cl
endfor	; loop over galaxies

free_lun,cl
;
; report
print,'Fitting this many galaxies: ',nfit
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
return
end
