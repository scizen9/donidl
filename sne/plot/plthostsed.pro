pro plthostsed,snid,silent=silent,ps=ps,site=site
;+
; plot the SN host SED
;-
common filter_info
common lowz_sne_info
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
waves=fltarr(17)		; Angstroms
bwid =fltarr(17)		; Angstroms
zpts =fltarr(17)+3630.78	; 0 mag (Jy)
flux =fltarr(17)-99.		; Jy
flxer=fltarr(17)		; Jy
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
; get sn
if n_params(0) lt 1 then begin
	sn=''
	read,'Enter SN id: ',sn
endif else sn = snid
s = snfind(sn)
if s lt 0 then begin
	print,'Not found: ',sn
	return
endif
if not keyword_set(silent) then $
	print,sndat(s).hlname
;
; GALEX FUV-band flux
if keyword_set(site) then begin
    if sndat(s).hfuv_1kpc_mag gt 0. then begin
	flux(iFUV) = zpts(iFUV)*10.^(-0.4*(sndat(s).hfuv_1kpc_mag))
	if sndat(s).hfuv_1kpc_magerr ge 0. then $
		flxer(iFUV)= flux(iFUV)*sndat(s).hfuv_1kpc_magerr/1.0857362d0 $
	else	flxer(iFUV)= -1.
	if not keyword_set(silent) then $
		print,'GALEX FUV flux   : ',flux(iFUV),' +- ',flxer(iFUV)
    endif
endif else begin
    if sndat(s).hfuv_int_mag gt 0. then begin
	flux(iFUV) = zpts(iFUV)*10.^(-0.4*(sndat(s).hfuv_int_mag))
	if sndat(s).hfuv_int_magerr ge 0. then $
		flxer(iFUV)= flux(iFUV)*sndat(s).hfuv_int_magerr/1.0857362d0 $
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
		flxer(iNUV)= flux(iNUV)*sndat(s).hnuv_1kpc_magerr/1.0857362d0 $
	else	flxer(iNUV)= -1.
	if not keyword_set(silent) then $
		print,'GALEX NUV flux   : ',flux(iNUV),' +- ',flxer(iNUV)
    endif
endif else begin
    if sndat(s).hnuv_int_mag gt 0. then begin
	flux(iNUV) = zpts(iNUV)*10.^(-0.4*(sndat(s).hnuv_int_mag))
	if sndat(s).hnuv_int_magerr ge 0. then $
		flxer(iNUV)= flux(iNUV)*sndat(s).hnuv_int_magerr/1.0857362d0 $
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
		flxer(isu)= flux(isu)*sndat(s).hu_1kpc_magerr/1.0857362d0 $
	else	flxer(isu)= -1.
	if not keyword_set(silent) then $
		print,'SDSS u-band flux : ',flux(isu),' +- ',flxer(isu)
    endif
endif else begin
    if sndat(s).hu_int_mag gt 0. then begin
	flux(isu) = zpts(isu)*10.^(-0.4*(sndat(s).hu_int_mag))
	if sndat(s).hu_int_magerr ge 0. then $
		flxer(isu)= flux(isu)*sndat(s).hu_int_magerr/1.0857362d0 $
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
		flxer(isg)= flux(isg)*sndat(s).hg_1kpc_magerr/1.0857362d0 $
	else	flxer(isg)= -1.
	if not keyword_set(silent) then $
		print,'SDSS g-band flux : ',flux(isg),' +- ',flxer(isg)
    endif
endif else begin
    if sndat(s).hg_int_mag gt 0. then begin
	flux(isg) = zpts(isg)*10.^(-0.4*(sndat(s).hg_int_mag))
	if sndat(s).hg_int_magerr ge 0. then $
		flxer(isg)= flux(isg)*sndat(s).hg_int_magerr/1.0857362d0 $
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
		flxer(isr)= flux(isr)*sndat(s).hr_1kpc_magerr/1.0857362d0 $
	else	flxer(isr)= -1.
	if not keyword_set(silent) then $
		print,'SDSS r-band flux : ',flux(isr),' +- ',flxer(isr)
    endif
endif else begin
    if sndat(s).hr_int_mag gt 0. then begin
	flux(isr) = zpts(isr)*10.^(-0.4*(sndat(s).hr_int_mag))
	if sndat(s).hr_int_magerr ge 0. then $
		flxer(isr)= flux(isr)*sndat(s).hr_int_magerr/1.0857362d0 $
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
		flxer(isi)= flux(isi)*sndat(s).hi_1kpc_magerr/1.0857362d0 $
	else	flxer(isi)= -1.
	if not keyword_set(silent) then $
		print,'SDSS i-band flux : ',flux(isi),' +- ',flxer(isi)
    endif
endif else begin
    if sndat(s).hi_int_mag gt 0. then begin
	flux(isi) = zpts(isi)*10.^(-0.4*(sndat(s).hi_int_mag))
	if sndat(s).hi_int_magerr ge 0. then $
		flxer(isi)= flux(isi)*sndat(s).hi_int_magerr/1.0857362d0 $
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
		flxer(isz)= flux(isz)*sndat(s).hz_1kpc_magerr/1.0857362d0 $
	else	flxer(isz)= -1.
	if not keyword_set(silent) then $
		print,'SDSS z-band flux : ',flux(isz),' +- ',flxer(isz)
    endif
endif else begin
    if sndat(s).hz_int_mag gt 0. then begin
	flux(isz) = zpts(isz)*10.^(-0.4*(sndat(s).hz_int_mag))
	if sndat(s).hz_int_magerr ge 0. then $
		flxer(isz)= flux(isz)*sndat(s).hz_int_magerr/1.0857362d0 $
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
		apparent_mag_err=fxe, flux_error=sndat(s).huJ_int_magerr, $
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
		apparent_mag_err=fxe, flux_error=sndat(s).hbJ_int_magerr, $
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
		apparent_mag_err=fxe, flux_error=sndat(s).hvJ_int_magerr, $
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
		apparent_mag_err=fxe, flux_error=sndat(s).hJ_1kpc_magerr, $
		/vegasys, /reverse) * flam2Jy * waves(i2J)^2
	flxer(i2J) = fxe * flam2Jy * waves(i2J)^2
	if not keyword_set(silent) then $
		print,'2MASS J-band flux: ',flux(i2J),' +- ',flxer(i2J)
    endif
endif else begin
    if sndat(s).hJ_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'J-2mass'))
	flux(i2J) = flux_to_apparent_mag(sndat(s).hJ_int_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=sndat(s).hJ_int_magerr, $
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
		apparent_mag_err=fxe, flux_error=sndat(s).hH_1kpc_magerr, $
		/vegasys, /reverse) * flam2Jy * waves(i2H)^2
	flxer(i2H) = fxe * flam2Jy * waves(i2H)^2
	if not keyword_set(silent) then $
		print,'2MASS H-band flux: ',flux(i2H),' +- ',flxer(i2H)
    endif
endif else begin
    if sndat(s).hH_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'H-2mass'))
	flux(i2H) = flux_to_apparent_mag(sndat(s).hH_int_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=sndat(s).hH_int_magerr, $
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
		apparent_mag_err=fxe, flux_error=sndat(s).hK_1kpc_magerr, $
		/vegasys, /reverse) * flam2Jy * waves(i2K)^2
	flxer(i2K) = fxe * flam2Jy * waves(i2K)^2
	if not keyword_set(silent) then $
		print,'2MASS K-band flux: ',flux(i2K),' +- ',flxer(i2K)
    endif
endif else begin
    if sndat(s).hK_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'Ks-2mass'))
	flux(i2K) = flux_to_apparent_mag(sndat(s).hK_int_mag,ifilter=f(0), $
		apparent_mag_err=fxe, flux_error=sndat(s).hK_int_magerr, $
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
	flxer(i12m)= flux(i12m)*sndat(s).h12m_int_magerr/1.0857362d0
	if not keyword_set(silent) then $
		print,'IRAS 12-mu   flux: ',flux(i12m),' +- ',flxer(i12m)
endif
;
; IRAS 25-mu flux
if sndat(s).h25m_int_mag gt 0. and not keyword_set(site) then begin
	flux(i25m) = zpts(i25m)*10.^(-0.4*(sndat(s).h25m_int_mag))
	flxer(i25m)= flux(i25m)*sndat(s).h25m_int_magerr/1.0857362d0
	if not keyword_set(silent) then $
		print,'IRAS 25-mu   flux: ',flux(i25m),' +- ',flxer(i25m)
endif
;
; IRAS 60-mu flux
if sndat(s).h60m_int_mag gt 0. and not keyword_set(site) then begin
	flux(i60m) = zpts(i60m)*10.^(-0.4*(sndat(s).h60m_int_mag))
	flxer(i60m)= flux(i60m)*sndat(s).h60m_int_magerr/1.0857362d0
	if not keyword_set(silent) then $
		print,'IRAS 60-mu   flux: ',flux(i60m),' +- ',flxer(i60m)
endif
;
; IRAS 100-mu flux
if sndat(s).h100m_int_mag gt 0. and not keyword_set(site) then begin
	flux(i100m) = zpts(i100m)*10.^(-0.4*(sndat(s).h100m_int_mag))
	flxer(i100m)= flux(i100m)*sndat(s).h100m_int_magerr/1.0857362d0
	if not keyword_set(silent) then $
		print,'IRAS 100-mu   flux: ',flux(i100m),' +- ',flxer(i100m)
endif
g=where(flux ge 0., ng)
if ng gt 0 then begin
	y0 = min(flux(g))
	y1 = max(flux(g))
	yrng = [y0,y1*10.]
endif else yrng = [1.e-1,1.e1]
g=where(flux ge 0. and flxer ge 0, ng)
;
; plot
font_store=!p.font
if keyword_set(ps) then begin
	th=5
	si=1.8
	if keyword_set(site) then $
		psf=sn+'_sSED' $
	else	psf=sn+'_hSED'
	psfile,psf
	print,'Plotting to: '+psf+'.ps'
	!p.font=1
endif else begin
	th=3
	si=2.0
endelse
if keyword_set(site) then begin
	xrng = [1000.,3.d4]
	ylab = 'Site Flux (Jy)'
	xl = 9000.
endif else begin
	xrng = [800.,1.2e6]
	ylab = 'Host Flux (Jy)'
	xl = 8.0e4 
endelse
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
a=[findgen(40)*(!pi*2/40.),0.]
usersym,cos(a)*si,sin(a)*si,thick=th
plot,waves,flux,psym=8,thick=th,xthick=th,ythick=th,charthi=th,$
		charsi=si,title=snid+'  '+sndat(s).type+'  '+sndat(s).hlname, $
		xtitle='Wavelength (Angstroms)',xran=xrng,xsty=1, $
		ytitle=ylab,yran=yrng,/xlog,/ylog,/nodata
if ng gt 0 then $
	oploterror,waves(g),flux(g),bwid(g)/2.,flxer(g),psym=8
;
; get limits
	u=where(flux gt 0. and flxer lt 0., nu)
	if nu gt 0 then begin
		oplot,waves(u),flux(u),psym=8
		olimplot,waves(u),flux(u),scale=2.0
	endif
;
ci=lonarr(5)
ci(0) = colordex('P')
ci(1) = colordex('B')
ci(2) = colordex('Q')
ci(3) = colordex('O')
ci(4) = colordex('R')
usersym,cos(a)*si,sin(a)*si,thick=th,/fill
sd=[isu,isg,isr,isi,isz]
wvsd=waves(sd)
fxsd=flux(sd)
oplot,wvsd,fxsd,psym=8,color=ci(1)
gd=[iFUV,iNUV]
wvgx=waves(gd)
fxgx=flux(gd)
oplot,wvgx,fxgx,psym=8,color=ci(0)
jd=[iJu,iJb,iJv]
wvjn=waves(jd)
fxjn=flux(jd)
oplot,wvjn,fxjn,psym=8,color=ci(2)
td=[i2J,i2H,i2K]
wv2m=waves(td)
fx2m=flux(td)
oplot,wv2m,fx2m,psym=8,color=ci(3)
id=[i12m,i25m,i60m,i100m]
wvir=waves(id)
fxir=flux(id)
oplot,wvir,fxir,psym=8,color=ci(4)
b=where(flux le 0., nb)
if nb gt 0 then begin
	fbad=fltarr(nb)+2.0*10.^(!y.crange(0))
	oplot,waves(b),fbad,psym=1,thick=th,symsi=si
	b=where(fxsd le 0., nb)
	if nb gt 0 then begin
		fbad=fltarr(nb)+2.0*10.^(!y.crange(0))
		oplot,wvsd(b),fbad,psym=1,color=ci(1),thick=th,symsi=si
	endif
	b=where(fxgx le 0., nb)
	if nb gt 0 then begin
		fbad=fltarr(nb)+2.0*10.^(!y.crange(0))
		oplot,wvgx(b),fbad,psym=1,color=ci(0),thick=th,symsi=si
	endif
	b=where(fxjn le 0., nb)
	if nb gt 0 then begin
		fbad=fltarr(nb)+2.0*10.^(!y.crange(0))
		oplot,wvjn(b),fbad,psym=1,color=ci(2),thick=th,symsi=si
	endif
	b=where(fx2m le 0., nb)
	if nb gt 0 then begin
		fbad=fltarr(nb)+2.0*10.^(!y.crange(0))
		oplot,wv2m(b),fbad,psym=1,color=ci(3),thick=th,symsi=si
	endif
	b=where(fxir le 0., nb)
	if nb gt 0 then begin
		fbad=fltarr(nb)+2.0*10.^(!y.crange(0))
		oplot,wvir(b),fbad,psym=1,color=ci(4),thick=th,symsi=si
	endif
endif
;
; legend
lem=strarr(4)
lem(0) = 'cz = '+string(sndat(s).cz,format='(f9.1)')
lem(1) = 'Str ='+string(sndat(s).smpl_str,format='(f5.2)') + $
	' B-V ='+string(sndat(s).smpl_clr,format='(f5.2)')
lem[2] = 'E(B-V)!DMW!N ='+string(sndat(s).hmwebmv,format='(f5.3)')
;if sndat(s).hnga_gal then lem(3) = lem(3) + 'NGA '
;if sndat(s).hglga_gal then lem(3) = lem(3) + 'GLGA '
;if sndat(s).hnyu_gal then lem(3) = lem(3) + 'NYU '
;if sndat(s).hlga_gal then lem(3) = lem(3) + 'LGA '
;if sndat(s).hxsc_gal then lem(3) = lem(3) + 'XSC '
;legend,lem,charsi=si,charthi=th,box=0,pos=[xl,0.8*10.^(!y.crange(1))]
;
lem=strarr(5)
lem(0) = 'GALEX'
lem(1) = 'SDSS'
lem(2) = 'RC3'
lem(3) = '2MASS'
lem(4) = 'IRAS'
usersym,cos(a),sin(a),thick=th,/fill
legend,lem,psym=[8,8,8,8,8],color=ci,charthi=th,charsi=si,box=0
;
if keyword_set(ps) then psclose
!p.font=font_store
;
return
end
