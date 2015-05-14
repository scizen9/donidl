pro snhpegfits,indices,site=site,silent=silent,samname=samname, $
	minfilts=minfilts, area=area
;+
;	snpegfits - write out a catalog and fit SED with zpeg
;
; INPUTS:
;	indices	- the sndat indices for the host to fit
;
; KEYWORDS:
;	site - use site photometry instead of (default) integrated photometry
;	silent - keep it quiet
;	samname - a root name for the output, defaults to 'snhosts'
;	minfilts - minimum number of filters for SED fits, defaults to 4
;	area - survey area in degrees, defaults to 1 degree
;
; HISTORY:
;	09-NOV-2010 - Initial version, jdn
;-
common filter_info
common lowz_sne_info
;
; minimum nuber of bandpasses
if keyword_set(minfilts) then $
	minflts = minfilts $
else	minflts = 4
;
; area, default is all-sky
if keyword_set(area) then $
	sarea = area $
else	sarea = 1.
;
; get sn
if n_params(0) lt 1 then begin
	print,'SNHPEGFITS: Usage - snhpegfits,sndat_indices'
	return
endif
nsn = n_elements(indices)
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
i2K=12
i12m=13
i25m=14
i60m=15
i100m=16
nflt=17
lstflt=iJv
waves=fltarr(nflt)		; Angstroms
bwid =fltarr(nflt)		; Angstroms
zpts =fltarr(nflt)+3630.78	; 0 mag (Jy)
zpegflts = [ $
	'GALEX_FUV.fil', 'GALEX_NUV.fil', $
	'u_prime.fil','g_prime.fil','r_prime.fil','i_prime.fil','z_prime.fil', $
	'UX_B90.fil','B_B90.fil','V_B90.fil', $
	'J.fil','H.fil','K.fil', $
	'IRAS12.fil','IRAS25.fil','IRAS60.fil','IRAS100.fil']
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
waves(i2K) = master_filter(43).mean_wave
bwid(i2K)  = 0.262*1.e4
zpts(i2K)  = zpts(i2K) * 10.^(-0.4*master_filter(43).ab_offset)
;
; IRAS
waves(i12m)= 12.*1.e4
waves(i25m)= 25.*1.e4
waves(i60m)= 60.*1.e4
waves(i100m)= 100.*1.e4
;
if keyword_set(samname) then $
	snam = samname $
else	snam = 'snhosts'
cfil = snam+'_pht.dat'
filestamp,cfil
openw,cl,cfil,/get_lun
printf,cl,'## SNHPEGFIT: run on '+systime(0)
printf,cl,'## '+snam+' initially containing '+strtrim(string(nsn),2)+' SNe'
printf,cl,'# SURVEY_AREA '+strtrim(string(sarea),2)
printf,cl,'# FILTER_COMPLETENESS '+strtrim(string(iJv+1),2)	; Johnson V
printf,cl,'# COMPLETENESS_LIMIT 22.0'				; arbitrary
flist = '# FILTERS '
calst = '# CALIBTYPES '
for i=0,lstflt do begin
	p = i
	flist = flist + zpegflts[p]+', '
	calst = calst + 'FNU_JY, '
endfor
printf,cl,flist
printf,cl,calst
;
; loop over indices
nwrt = 0L
for j=0,nsn-1 do begin
  flux =fltarr(nflt)-99.	; Jy
  flxer=fltarr(nflt)-1.		; Jy
  s = indices[j]

  if not keyword_set(silent) then $
	print,sndat(s).hlname
;
; GALEX FUV-band flux
  if keyword_set(site) then begin
    if sndat(s).hfuv_1kpc_mag gt 0. then begin
	flux(iFUV) = zpts(iFUV)*10.^(-0.4*(sndat(s).hfuv_1kpc_mag))
	if sndat(s).hfuv_1kpc_magerr ge 0. then $
		flxer(iFUV)= flux(iFUV)*(sndat(s).hfuv_1kpc_magerr>0.001)/1.0857362d0 $
	else	flxer(iFUV)= -1.
	if not keyword_set(silent) then $
		print,'GALEX FUV flux   : ',flux(iFUV),' +- ',flxer(iFUV)
    endif
  endif else begin
    if sndat(s).hfuv_int_mag gt 0. then begin
	flux(iFUV) = zpts(iFUV)*10.^(-0.4*(sndat(s).hfuv_int_mag))
	if sndat(s).hfuv_int_magerr ge 0. then $
		flxer(iFUV)= flux(iFUV)*(sndat(s).hfuv_int_magerr>0.001)/1.0857362d0 $
	else	flxer(iFUV)= -1.
	if not keyword_set(silent) then $
		print,'GALEX FUV flux   : ',flux(iFUV),' +- ',flxer(iFUV)
    endif
  endelse
;
; GALEX NUV-band flux
  if keyword_set(site) then begin
    if sndat(s).hnuv_1kpc_mag gt 0. then begin
	flux(iNUV) = zpts(iNUV)*10.^(-0.4*(sndat(s).hnuv_1kpc_mag))
	if sndat(s).hnuv_1kpc_magerr ge 0. then $
		flxer(iNUV)= flux(iNUV)*(sndat(s).hnuv_1kpc_magerr>0.001)/1.0857362d0 $
	else	flxer(iNUV)= -1.
	if not keyword_set(silent) then $
		print,'GALEX NUV flux   : ',flux(iNUV),' +- ',flxer(iNUV)
    endif
  endif else begin
    if sndat(s).hnuv_int_mag gt 0. then begin
	flux(iNUV) = zpts(iNUV)*10.^(-0.4*(sndat(s).hnuv_int_mag))
	if sndat(s).hnuv_int_magerr ge 0. then $
		flxer(iNUV)= flux(iNUV)*(sndat(s).hnuv_int_magerr>0.001)/1.0857362d0 $
	else	flxer(iNUV)= -1.
	if not keyword_set(silent) then $
		print,'GALEX NUV flux   : ',flux(iNUV),' +- ',flxer(iNUV)
    endif
  endelse
;
; SDSS u-band flux
  if keyword_set(site) then begin
    if sndat(s).hu_1kpc_mag gt 0. then begin
	flux(isu) = zpts(isu)*10.^(-0.4*(sndat(s).hu_1kpc_mag))
	if sndat(s).hu_1kpc_magerr ge 0. then $
		flxer(isu)= flux(isu)*(sndat(s).hu_1kpc_magerr>0.001)/1.0857362d0 $
	else	flxer(isu)= -1.
	if not keyword_set(silent) then $
		print,'SDSS u-band flux : ',flux(isu),' +- ',flxer(isu)
    endif
  endif else begin
    if sndat(s).hu_int_mag gt 0. then begin
	flux(isu) = zpts(isu)*10.^(-0.4*(sndat(s).hu_int_mag))
	if sndat(s).hu_int_magerr ge 0. then $
		flxer(isu)= flux(isu)*(sndat(s).hu_int_magerr>0.001)/1.0857362d0 $
	else	flxer(isu)= -1.
	if not keyword_set(silent) then $
		print,'SDSS u-band flux : ',flux(isu),' +- ',flxer(isu)
    endif
  endelse
;
; SDSS g-band flux
  if keyword_set(site) then begin
    if sndat(s).hg_1kpc_mag gt 0. then begin
	flux(isg) = zpts(isg)*10.^(-0.4*(sndat(s).hg_1kpc_mag))
	if sndat(s).hg_1kpc_magerr ge 0. then $
		flxer(isg)= flux(isg)*(sndat(s).hg_1kpc_magerr>0.001)/1.0857362d0 $
	else	flxer(isg)= -1.
	if not keyword_set(silent) then $
		print,'SDSS g-band flux : ',flux(isg),' +- ',flxer(isg)
    endif
  endif else begin
    if sndat(s).hg_int_mag gt 0. then begin
	flux(isg) = zpts(isg)*10.^(-0.4*(sndat(s).hg_int_mag))
	if sndat(s).hg_int_magerr ge 0. then $
		flxer(isg)= flux(isg)*(sndat(s).hg_int_magerr>0.001)/1.0857362d0 $
	else	flxer(isg)= -1.
	if not keyword_set(silent) then $
		print,'SDSS g-band flux : ',flux(isg),' +- ',flxer(isg)
    endif
  endelse
;
; SDSS r-band flux
  if keyword_set(site) then begin
    if sndat(s).hr_1kpc_mag gt 0. then begin
	flux(isr) = zpts(isr)*10.^(-0.4*(sndat(s).hr_1kpc_mag))
	if sndat(s).hr_1kpc_magerr ge 0. then $
		flxer(isr)= flux(isr)*(sndat(s).hr_1kpc_magerr>0.001)/1.0857362d0 $
	else	flxer(isr)= -1.
	if not keyword_set(silent) then $
		print,'SDSS r-band flux : ',flux(isr),' +- ',flxer(isr)
    endif
  endif else begin
    if sndat(s).hr_int_mag gt 0. then begin
	flux(isr) = zpts(isr)*10.^(-0.4*(sndat(s).hr_int_mag))
	if sndat(s).hr_int_magerr ge 0. then $
		flxer(isr)= flux(isr)*(sndat(s).hr_int_magerr>0.001)/1.0857362d0 $
	else	flxer(isr)= -1.
	if not keyword_set(silent) then $
		print,'SDSS r-band flux : ',flux(isr),' +- ',flxer(isr)
    endif
  endelse
;
; SDSS i-band flux
  if keyword_set(site) then begin
    if sndat(s).hi_1kpc_mag gt 0. then begin
	flux(isi) = zpts(isi)*10.^(-0.4*(sndat(s).hi_1kpc_mag))
	if sndat(s).hi_1kpc_magerr ge 0. then $
		flxer(isi)= flux(isi)*(sndat(s).hi_1kpc_magerr>0.001)/1.0857362d0 $
	else	flxer(isi)= -1.
	if not keyword_set(silent) then $
		print,'SDSS i-band flux : ',flux(isi),' +- ',flxer(isi)
    endif
  endif else begin
    if sndat(s).hi_int_mag gt 0. then begin
	flux(isi) = zpts(isi)*10.^(-0.4*(sndat(s).hi_int_mag))
	if sndat(s).hi_int_magerr ge 0. then $
		flxer(isi)= flux(isi)*(sndat(s).hi_int_magerr>0.001)/1.0857362d0 $
	else	flxer(isi)= -1.
	if not keyword_set(silent) then $
		print,'SDSS i-band flux : ',flux(isi),' +- ',flxer(isi)
    endif
  endelse
;
; SDSS z-band flux
  if keyword_set(site) then begin
    if sndat(s).hz_1kpc_mag gt 0. then begin
	flux(isz) = zpts(isz)*10.^(-0.4*(sndat(s).hz_1kpc_mag))
	if sndat(s).hz_1kpc_magerr ge 0. then $
		flxer(isz)= flux(isz)*(sndat(s).hz_1kpc_magerr>0.001)/1.0857362d0 $
	else	flxer(isz)= -1.
	if not keyword_set(silent) then $
		print,'SDSS z-band flux : ',flux(isz),' +- ',flxer(isz)
    endif
  endif else begin
    if sndat(s).hz_int_mag gt 0. then begin
	flux(isz) = zpts(isz)*10.^(-0.4*(sndat(s).hz_int_mag))
	if sndat(s).hz_int_magerr ge 0. then $
		flxer(isz)= flux(isz)*(sndat(s).hz_int_magerr>0.001)/1.0857362d0 $
	else	flxer(isz)= -1.
	if not keyword_set(silent) then $
		print,'SDSS z-band flux : ',flux(isz),' +- ',flxer(isz)
    endif
  endelse
;
; Johnson U-band flux
  if sndat(s).huJ_int_mag gt 0. and not keyword_set(site) then begin
	f=where(strcmp(master_filter.shortid,'U'))
	flux(iJu) = flux_to_apparent_mag(sndat(s).huJ_int_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=(sndat(s).huJ_int_magerr>0.001), $
		/vegasys, /reverse) * flam2Jy * waves(iJu)^2
	flxer(iJu) = fxe * flam2Jy * waves(iJu)^2
	if not keyword_set(silent) then $
		print,'Johnson U-band flux : ',flux(iJu),' +- ',flxer(iJu)
  endif
;
; Johnson B-band flux
  if sndat(s).hbJ_int_mag gt 0. and not keyword_set(site) then begin
	f=where(strcmp(master_filter.shortid,'B'))
	flux(iJb) = flux_to_apparent_mag(sndat(s).hbJ_int_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=(sndat(s).hbJ_int_magerr>0.001), $
		/vegasys, /reverse) * flam2Jy * waves(iJb)^2
	flxer(iJb) = fxe * flam2Jy * waves(iJb)^2
	if not keyword_set(silent) then $
		print,'Johnson B-band flux : ',flux(iJb),' +- ',flxer(iJb)
  endif
;
; Johnson V-band flux
  if sndat(s).hvJ_int_mag gt 0. and not keyword_set(site) then begin
	f=where(strcmp(master_filter.shortid,'V'))
	flux(iJv) = flux_to_apparent_mag(sndat(s).hvJ_int_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=(sndat(s).hvJ_int_magerr>0.001), $
		/vegasys, /reverse) * flam2Jy * waves(iJv)^2
	flxer(iJv) = fxe * flam2Jy * waves(iJv)^2
	if not keyword_set(silent) then $
		print,'Johnson V-band flux : ',flux(iJv),' +- ',flxer(iJv)
  endif
;
; 2MASS J-band flux
  if keyword_set(site) then begin
    if sndat(s).hJ_1kpc_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'J-2mass'))
	flux(i2J) = flux_to_apparent_mag(sndat(s).hJ_1kpc_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=(sndat(s).hJ_1kpc_magerr>0.001), $
		/vegasys, /reverse) * flam2Jy * waves(i2J)^2
	flxer(i2J) = fxe * flam2Jy * waves(i2J)^2
	if not keyword_set(silent) then $
		print,'2MASS J-band flux: ',flux(i2J),' +- ',flxer(i2J)
    endif
  endif else begin
    if sndat(s).hJ_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'J-2mass'))
	flux(i2J) = flux_to_apparent_mag(sndat(s).hJ_int_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=(sndat(s).hJ_int_magerr>0.001), $
		/vegasys, /reverse) * flam2Jy * waves(i2J)^2
	flxer(i2J) = fxe * flam2Jy * waves(i2J)^2
	if not keyword_set(silent) then $
		print,'2MASS J-band flux: ',flux(i2J),' +- ',flxer(i2J)
    endif
  endelse
;
; 2MASS H-band flux
  if keyword_set(site) then begin
    if sndat(s).hH_1kpc_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'H-2mass'))
	flux(i2H) = flux_to_apparent_mag(sndat(s).hH_1kpc_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=(sndat(s).hH_1kpc_magerr>0.001), $
		/vegasys, /reverse) * flam2Jy * waves(i2H)^2
	flxer(i2H) = fxe * flam2Jy * waves(i2H)^2
	if not keyword_set(silent) then $
		print,'2MASS H-band flux: ',flux(i2H),' +- ',flxer(i2H)
    endif
  endif else begin
    if sndat(s).hH_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'H-2mass'))
	flux(i2H) = flux_to_apparent_mag(sndat(s).hH_int_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=(sndat(s).hH_int_magerr>0.001), $
		/vegasys, /reverse) * flam2Jy * waves(i2H)^2
	flxer(i2H) = fxe * flam2Jy * waves(i2H)^2
	if not keyword_set(silent) then $
		print,'2MASS H-band flux: ',flux(i2H),' +- ',flxer(i2H)
    endif
  endelse
;
; 2MASS K-band flux
  if keyword_set(site) then begin
    if sndat(s).hK_1kpc_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'Ks-2mass'))
	flux(i2K) = flux_to_apparent_mag(sndat(s).hK_1kpc_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=(sndat(s).hK_1kpc_magerr>0.001), $
		/vegasys, /reverse) * flam2Jy * waves(i2K)^2
	flxer(i2K) = fxe * flam2Jy * waves(i2K)^2
	if not keyword_set(silent) then $
		print,'2MASS K-band flux: ',flux(i2K),' +- ',flxer(i2K)
    endif
  endif else begin
    if sndat(s).hK_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'Ks-2mass'))
	flux(i2K) = flux_to_apparent_mag(sndat(s).hK_int_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=(sndat(s).hK_int_magerr>0.001), $
		/vegasys, /reverse) * flam2Jy * waves(i2K)^2
	flxer(i2K) = fxe * flam2Jy * waves(i2K)^2
	if not keyword_set(silent) then $
		print,'2MASS K-band flux: ',flux(i2K),' +- ',flxer(i2K)
    endif
  endelse
;
; IRAS 12-mu flux
  if sndat(s).h12m_int_mag gt 0. and not keyword_set(site) then begin
	flux(i12m) = zpts(i12m)*10.^(-0.4*(sndat(s).h12m_int_mag))
	flxer(i12m)= flux(i12m)*(sndat(s).h12m_int_magerr>0.001)/1.0857362d0
	if not keyword_set(silent) then $
		print,'IRAS 12-mu   flux: ',flux(i12m),' +- ',flxer(i12m)
  endif
;
; IRAS 25-mu flux
  if sndat(s).h25m_int_mag gt 0. and not keyword_set(site) then begin
	flux(i25m) = zpts(i25m)*10.^(-0.4*(sndat(s).h25m_int_mag))
	flxer(i25m)= flux(i25m)*(sndat(s).h25m_int_magerr>0.001)/1.0857362d0
	if not keyword_set(silent) then $
		print,'IRAS 25-mu   flux: ',flux(i25m),' +- ',flxer(i25m)
  endif
;
; IRAS 60-mu flux
  if sndat(s).h60m_int_mag gt 0. and not keyword_set(site) then begin
	flux(i60m) = zpts(i60m)*10.^(-0.4*(sndat(s).h60m_int_mag))
	flxer(i60m)= flux(i60m)*(sndat(s).h60m_int_magerr>0.001)/1.0857362d0
	if not keyword_set(silent) then $
		print,'IRAS 60-mu   flux: ',flux(i60m),' +- ',flxer(i60m)
  endif
;
; IRAS 100-mu flux
  if sndat(s).h100m_int_mag gt 0. and not keyword_set(site) then begin
	flux(i100m) = zpts(i100m)*10.^(-0.4*(sndat(s).h100m_int_mag))
	flxer(i100m)= flux(i100m)*(sndat(s).h100m_int_magerr>0.001)/1.0857362d0
	if not keyword_set(silent) then $
		print,'IRAS 100-mu   flux: ',flux(i100m),' +- ',flxer(i100m)
  endif

;
; only output if we have at least 4 bandpasses
  tstflx = flux[0:lstflt]
  tstfle = flxer[0:lstflt]
  g=where(tstflx ge 0. and tstfle ge 0., ng)
  if ng ge minflts then begin
    mglst = sndat[s].id+' '+string((sndat[s].cz/!phys_c)>0.005,form='(f7.4)') + $
	string(sndat[s].htyn,form='(f12.2)') + $
	string(sndat[s].hra,form='(f16.8)') + $
	string(sndat[s].hdec,form='(f16.8)') + '   '
    for i=0,lstflt do $
	mglst = mglst + string(flux[i],form='(g15.3)') + '  ' + $
			string(flxer[i],form='(g15.3)') + '  '
    printf,cl,mglst
    nwrt = nwrt + 1L
  endif

endfor	 ; loop over indices
printf,cl,''
free_lun,cl
print,'Wrote ',nwrt,' SN hosts to catalog: ',cfil
if nwrt le 0 then return
;
; run zpeg
;cmd = 'zpeg -V '+cfil+' -o '+snam+' -p '+!ZPEG_ROOT+'data/snhost.par -t '+ $
cmd = 'zpeg -V '+cfil+' -o '+snam+' -p '+!ZPEG_ROOT+'data/mark_zpegin.par -t '+ $
		 snam+'.par.tmp |& tee '+snam+'.log'
print,cmd
spawn,cmd
;
; make plots
plot_zpegfits,snam,/zpeg_scen,xr=[500.,10000.],xsty=1,/fnu
;
return
end
