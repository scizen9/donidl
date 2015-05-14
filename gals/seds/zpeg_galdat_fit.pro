pro zpeg_galdat_fit,indices,silent=silent,samname=samname, $
	minfilts=minfilts, area=area, filtnums=filtnums, sne=sne, $
	make_plots=make_plots
;+
;	snpegfits - write out a catalog and fit SED with zpeg
;
; INPUTS:
;	indices	- the galdat indices for the galaxies to fit
;
; KEYWORDS:
;	silent	- keep it quiet
;	samname	- a root name for the output, defaults to 'gals'
;	minfilts- minimum number of filters for SED fits, defaults to 4
;	area	- survey area in degrees, defaults to 1 degree
;	filtnums- filters to use, see below
;	sne	- append SNe to id
;	make_plots - set to make postscript plot of all fits
;
; HISTORY:
;	09-NOV-2010 - Initial version, jdn
;-
common filter_info
common galdb_info
;
; minimum nuber of bandpasses
if keyword_set(minfilts) then $
	minflts = minfilts $
else	minflts = 4
;
; check filtnums
if keyword_set(filtnums) then begin
	if n_elements(filtnums) lt minflts then begin
		print,'ZPEG_GALDAT_FIT: Error - size of filtnums must be >= minfilts'
		return
	endif
endif
;
; area, default is all-sky
if keyword_set(area) then $
	sarea = area $
else	sarea = 1.
;
; get gals
if n_params(0) lt 1 then begin
	print,'ZPEG_GALDAT_FIT: Usage - zpeg_galdat_fit,galdat_indices'
	return
endif
ngal = n_elements(indices)
;
; set up constants
flam2Jy = 3.33564095d4	; times lamda^2 in Angstroms
iFUV=0
iNUV=1
isu=2
isg=3
isr=4
isi=5
isz=6
iJu=7
iJb=8
iJv=9
i2J=10
i2H=11
nflt=12		; ZPEG only fits out to the H-band
if keyword_set(filtnums) then begin
	if max(filtnums) ge nflt or min(filtnums) lt 0 then begin
		print,'ZPEG_GALDAT_FIT: Error - filtnums indices must be in the range 0 - '+strn(nflt-1)
		return
	endif
	ufilt = intarr(nflt)
	ufilt[filtnums] = 1
endif else ufilt = intarr(nflt) + 1
waves=fltarr(nflt)		; Angstroms
bwid =fltarr(nflt)		; Angstroms
zpts =fltarr(nflt)+3630.78	; 0 mag (Jy)
zpegflts = [ $
	'GALEX_FUV.fil', 'GALEX_NUV.fil', $
	'u_prime.fil','g_prime.fil','r_prime.fil','i_prime.fil','z_prime.fil', $
	'UX_B90.fil','B_B90.fil','V_B90.fil', $
	'J.fil','H.fil' ]
;
; GALEX
waves(iFUV)= 1531.6
bwid(iFUV) = 442.4
waves(iNUV)= 2256.6
bwid(iNUV) = 1059.3
;
; SDSS
waves(isu) = 3543.
bwid(isu)  = 634.
waves(isg) = 4770.
bwid(isg)  = 1409.
waves(isr) = 6231.
bwid(isr)  = 1388.
waves(isi) = 7625.
bwid(isi)  = 1535.
waves(isz) = 9134.
bwid(isz)  = 1409.
;
; Johnson
waves(iJu) = master_filter(65).mean_wave
bwid(iJu)  = 524.
zpts(iJu)  = zpts(iJu) * 10.^(-0.4*master_filter(65).ab_offset)
waves(iJb) = master_filter(66).mean_wave
bwid(iJb)  = 1008.
zpts(iJb)  = zpts(iJb) * 10.^(-0.4*master_filter(66).ab_offset)
waves(iJv) = master_filter(67).mean_wave
bwid(iJv)  = 826.
zpts(iJv)  = zpts(iJv) * 10.^(-0.4*master_filter(67).ab_offset)
;
; 2MASS
waves(i2J) = master_filter(41).mean_wave
bwid(i2J)  = 0.162*1.e4
zpts(i2J)  = zpts(i2J) * 10.^(-0.4*master_filter(41).ab_offset)
waves(i2H) = master_filter(42).mean_wave
bwid(i2H)  = 0.251*1.e4
zpts(i2H)  = zpts(i2H) * 10.^(-0.4*master_filter(42).ab_offset)
;
if keyword_set(samname) then $
	snam = samname $
else	snam = 'gals'
odir = timestr()
file_mkdir,odir
cd,odir
cfil = 'zpeg_pht_'+odir+'.dat'
openw,cl,cfil,/get_lun
printf,cl,'## ZPEG_GALDAT_FIT: run on '+systime(0)
printf,cl,'## '+snam+' initially containing '+strtrim(string(ngal),2)+' Galaxies'
printf,cl,'# SURVEY_AREA '+strtrim(string(sarea),2)
printf,cl,'# FILTER_COMPLETENESS '+strtrim(string(iJv+1),2)
printf,cl,'# COMPLETENESS_LIMIT 22.0'				; arbitrary
flist = '# FILTERS '
calst = '# CALIBTYPES '
for i=0,nflt-1 do begin
	if ufilt[i] eq 1 then begin
		flist = flist + zpegflts[i]+', '
		calst = calst + 'FNU_JY, '
	endif
endfor
printf,cl,flist
printf,cl,calst
;
; loop over indices
nwrt = 0L
for j=0L,ngal-1L do begin
  flux =fltarr(nflt)-99.	; Jy
  flxer=fltarr(nflt)-1.		; Jy
  s = indices[j]

  if not keyword_set(silent) then $
	print,galdat(s).id
;
; GALEX FUV-band flux
    if galdat(s).fuv_int_mag gt 0. then begin
	flux(iFUV) = zpts(iFUV)*10.^(-0.4*(galdat(s).fuv_int_mag))
	if galdat(s).fuv_int_magerr ge 0. then $
		flxer(iFUV)= flux(iFUV)*(galdat(s).fuv_int_magerr>0.001)/1.0857362d0 $
	else	flxer(iFUV)= -1.
	if not keyword_set(silent) then $
		print,'GALEX FUV flux   : ',flux(iFUV),' +- ',flxer(iFUV)
    endif
;
; GALEX NUV-band flux
    if galdat(s).nuv_int_mag gt 0. then begin
	flux(iNUV) = zpts(iNUV)*10.^(-0.4*(galdat(s).nuv_int_mag))
	if galdat(s).nuv_int_magerr ge 0. then $
		flxer(iNUV)= flux(iNUV)*(galdat(s).nuv_int_magerr>0.001)/1.0857362d0 $
	else	flxer(iNUV)= -1.
	if not keyword_set(silent) then $
		print,'GALEX NUV flux   : ',flux(iNUV),' +- ',flxer(iNUV)
    endif
;
; SDSS u-band flux
    if galdat(s).u_int_mag gt 0. then begin
	flux(isu) = zpts(isu)*10.^(-0.4*(galdat(s).u_int_mag))
	if galdat(s).u_int_magerr ge 0. then $
		flxer(isu)= flux(isu)*(galdat(s).u_int_magerr>0.001)/1.0857362d0 $
	else	flxer(isu)= -1.
	if not keyword_set(silent) then $
		print,'SDSS u-band flux : ',flux(isu),' +- ',flxer(isu)
    endif
;
; SDSS g-band flux
    if galdat(s).g_int_mag gt 0. then begin
	flux(isg) = zpts(isg)*10.^(-0.4*(galdat(s).g_int_mag))
	if galdat(s).g_int_magerr ge 0. then $
		flxer(isg)= flux(isg)*(galdat(s).g_int_magerr>0.001)/1.0857362d0 $
	else	flxer(isg)= -1.
	if not keyword_set(silent) then $
		print,'SDSS g-band flux : ',flux(isg),' +- ',flxer(isg)
    endif
;
; SDSS r-band flux
    if galdat(s).r_int_mag gt 0. then begin
	flux(isr) = zpts(isr)*10.^(-0.4*(galdat(s).r_int_mag))
	if galdat(s).r_int_magerr ge 0. then $
		flxer(isr)= flux(isr)*(galdat(s).r_int_magerr>0.001)/1.0857362d0 $
	else	flxer(isr)= -1.
	if not keyword_set(silent) then $
		print,'SDSS r-band flux : ',flux(isr),' +- ',flxer(isr)
    endif
;
; SDSS i-band flux
    if galdat(s).i_int_mag gt 0. then begin
	flux(isi) = zpts(isi)*10.^(-0.4*(galdat(s).i_int_mag))
	if galdat(s).i_int_magerr ge 0. then $
		flxer(isi)= flux(isi)*(galdat(s).i_int_magerr>0.001)/1.0857362d0 $
	else	flxer(isi)= -1.
	if not keyword_set(silent) then $
		print,'SDSS i-band flux : ',flux(isi),' +- ',flxer(isi)
    endif
;
; SDSS z-band flux
    if galdat(s).z_int_mag gt 0. then begin
	flux(isz) = zpts(isz)*10.^(-0.4*(galdat(s).z_int_mag))
	if galdat(s).z_int_magerr ge 0. then $
		flxer(isz)= flux(isz)*(galdat(s).z_int_magerr>0.001)/1.0857362d0 $
	else	flxer(isz)= -1.
	if not keyword_set(silent) then $
		print,'SDSS z-band flux : ',flux(isz),' +- ',flxer(isz)
    endif
;
; Johnson U-band flux
  if galdat(s).uJ_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'U'))
	flux(iJu) = flux_to_apparent_mag(galdat(s).uJ_int_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=(galdat(s).uJ_int_magerr>0.001), $
		/vegasys, /reverse) * flam2Jy * waves(iJu)^2
	flxer(iJu) = fxe * flam2Jy * waves(iJu)^2
	if not keyword_set(silent) then $
		print,'Johnson U-band flux : ',flux(iJu),' +- ',flxer(iJu)
  endif
;
; Johnson B-band flux
  if galdat(s).bJ_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'B'))
	flux(iJb) = flux_to_apparent_mag(galdat(s).bJ_int_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=(galdat(s).bJ_int_magerr>0.001), $
		/vegasys, /reverse) * flam2Jy * waves(iJb)^2
	flxer(iJb) = fxe * flam2Jy * waves(iJb)^2
	if not keyword_set(silent) then $
		print,'Johnson B-band flux : ',flux(iJb),' +- ',flxer(iJb)
  endif
;
; Johnson V-band flux
  if galdat(s).vJ_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'V'))
	flux(iJv) = flux_to_apparent_mag(galdat(s).vJ_int_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=(galdat(s).vJ_int_magerr>0.001), $
		/vegasys, /reverse) * flam2Jy * waves(iJv)^2
	flxer(iJv) = fxe * flam2Jy * waves(iJv)^2
	if not keyword_set(silent) then $
		print,'Johnson V-band flux : ',flux(iJv),' +- ',flxer(iJv)
  endif
;
; 2MASS J-band flux
    if galdat(s).J_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'J-2mass'))
	flux(i2J) = flux_to_apparent_mag(galdat(s).J_int_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=(galdat(s).J_int_magerr>0.001), $
		/vegasys, /reverse) * flam2Jy * waves(i2J)^2
	flxer(i2J) = fxe * flam2Jy * waves(i2J)^2
	if not keyword_set(silent) then $
		print,'2MASS J-band flux: ',flux(i2J),' +- ',flxer(i2J)
    endif
;
; 2MASS H-band flux
    if galdat(s).H_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'H-2mass'))
	flux(i2H) = flux_to_apparent_mag(galdat(s).H_int_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=(galdat(s).H_int_magerr>0.001), $
		/vegasys, /reverse) * flam2Jy * waves(i2H)^2
	flxer(i2H) = fxe * flam2Jy * waves(i2H)^2
	if not keyword_set(silent) then $
		print,'2MASS H-band flux: ',flux(i2H),' +- ',flxer(i2H)
    endif
;
; only output if we have at least minflts bandpasses
  t = where(ufilt eq 1)
  tstflx = flux[t]
  tstfle = flxer[t]
  g=where(tstflx ge 0. and tstfle ge 0., ng)
  if ng ge minflts then begin
    id = strtrim(galdat[s].id,2)
    if keyword_set(sne) then $
	    id = id + ':' + strtrim(galdat[s].sne,2)
    fmt = '(a-'+strn(strlen(id)>25)+')'
    mglst = string(id,form=fmt)+' '+ $
	    string((galdat[s].cz/!phys_c)>0.005,form='(f7.4)') + $
	    string(galdat[s].tyn,form='(f12.2)') + $
	    string(galdat[s].ra,form='(f16.8)') + $
	    string(galdat[s].dec,form='(f16.8)') + '   '
    for i=0,nflt-1 do $
	if ufilt[i] eq 1 then $
	    mglst = mglst + string(flux[i], form='(g15.3)') + '  ' + $
			    string(flxer[i],form='(g15.3)') + '  '
    printf,cl,mglst
    nwrt = nwrt + 1L
  endif

endfor	 ; loop over indices
printf,cl,''
free_lun,cl
print,'Wrote ',nwrt,' Galaxies to catalog: ',cfil
if nwrt le 0 then return
;
; run zpeg
cmd='zpeg -V '+cfil+' -o '+snam+' -p '+!ZPEG_ROOT+'data/mark_zpegin.par -t '+ $
		 snam+'.par.tmp |& tee '+snam+'.log'
print,cmd
spawn,cmd
;
; make plots
if keyword_set(make_plots) then $
	plot_zpegfits,snam,/zpeg_scen,xr=[750.,32000.],xsty=1,/xlog,/fnu
;
cd,'..'
;
return
end
