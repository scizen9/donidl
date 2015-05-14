pro get_bestfluxes, galid, flux, fluxerr, wave, bwidth, source, phot_ts, $
	yuan13=yuan13, status=status, verbose=verbose, silent=silent, $
	galex_indices=galex_indices, sdss_indices=sdss_indices, $
	rc3_indices=rc3_indices, twomass_indices=twomass_indices, $
	wise_indices=wise_indices, iras_indices=iras_indices
;+
;	get_bestfluxes - get the best extinction-corrected fluxes
;
; CALLING SEQUENCE:
;	get_bestfluxes, galid, flux, fluxerr, wave, bwidth, source
;
; INPUTS:
;	galid	- galaxy id as specified in galdat struct
;
; OUTPUTS:
;	flux	- extinction corrected flux in Jy (F-lambda)
;	fluxerr	- errors on the fluxes
;	wave	- wavelengths of bandpasses in Angstroms
;	bwidth	- width of filter bandpasses in Angstroms
;	source	- 1 if GLGA, 2 if Literature (NED)
;	phot_ts	- latest timestamp of photometry (unix seconds)
;
; KEYWORDS:
;	yuan13	- use extinction coeff's of Yuan et al. 2013
;	status	- 0, galid found, -1, galid not in galdat and/or glgadat
;	verbose	- extra output to screen
;	silent	- silence output
;	*_indices	- wave indices for each survey
;
; NOTE:
;	If all photometry is from literature sources, the time-stamp
;	will be 0 (zero).
;
; HISTORY:
; $Id: get_bestfluxes.pro,v 1.20 2014/02/24 17:14:48 neill Exp $
;	03-JUN-2013, jdn - Initial Revision
;	24-FEB-2014, jdn - GLGA 2MASS mags now in AB
;-
common filter_info
common galdb_info
common glgadb_info
;
status = 0
;
; check inputs
;
; get galaxy
if n_params(0) lt 1 then begin
	gal=''
	read,'Enter Galaxy id: ',gal
endif else gal = galid
g = gfind(gal)
gl = glfind(gal,count=ngl)
if g lt 0 then begin
	print,'Not in gal db: ',gal
	status = -1
	return
endif
if ngl le 0 then begin
	print,'Not in GLGA db: ',gal
	status = -1
	return
endif
;
; get GLGA directory
glga_dir = !GLGA_ROOT + 'data/'+glga_degdir(galdat[g].ra)+'/'
;
; get MW extinction
ebmv = galdat[g].mwebmv 
;
if keyword_set(verbose) then $
	print,galdat(g).id
;
; GLGA phot path
ppath = glga_dir+'photometry/' 
;
; photometry bands
bands = ['FUV','NUV','uJ','bJ','vJ','u','g','r','i','z', $
	 'j','h','k','w1','w2','w3','w4','i12m','i25m','i60m','i100m']
nbands= n_elements(bands)
opfmt = '(a13,f9.1,1x,f8.1,i4,g10.3,1x,a2,1x,g10.3)'
if not keyword_set(silent) then $
	print,'       BAND       WAVE     BWID  SRC     FLUX         ERROR'
;
; set up constants
flam2Jy	= 3.33564095d4	; times lamda^2 in Angstroms
iFUV	= 0
iNUV	= 1
iJu	= 2
iJb	= 3
iJv	= 4
isu	= 5
isg	= 6
isr	= 7
isi	= 8
isz	= 9
i2J	= 10
i2H	= 11
i2K	= 12
iw1	= 13
iw2	= 14
iw3	= 15
iw4	= 16
i12m	= 17
i25m	= 18
i60m	= 19
i100m	= 20
nwavs	= 21
wave	= fltarr(nwavs)		; Angstroms
bwidth	= fltarr(nwavs)		; Angstroms
zpts	= fltarr(nwavs)+3630.78	; 0 mag (Jy)
source	= intarr(nwavs)		; 2 - literature, 1 - GLGA, 0 - none
flux	= fltarr(nwavs)-99.	; Jy
fluxerr	= fltarr(nwavs)		; Jy
;
; GALEX
wave[iFUV]= 1531.6
bwidth[iFUV] = 442.4
wave[iNUV]= 2256.6
bwidth[iNUV] = 1059.3
galex_indices = [iFUV, iNUV]
;
; Johnson
wave[iJu] = master_filter[65].mean_wave
bwidth[iJu]  = 524.
zpts[iJu]  = zpts[iJu] * 10.^(-0.4*master_filter[65].ab_offset)
wave[iJb] = master_filter[66].mean_wave
bwidth[iJb]  = 1008.
zpts[iJb]  = zpts[iJb] * 10.^(-0.4*master_filter[66].ab_offset)
wave[iJv] = master_filter[67].mean_wave
bwidth[iJv]  = 826.
zpts[iJv]  = zpts[iJv] * 10.^(-0.4*master_filter[67].ab_offset)
rc3_indices = [iJu, iJb, iJv]
;
; SDSS
wave[isu] = 3543.
bwidth[isu]  = 634.
wave[isg] = 4770.
bwidth[isg]  = 1409.
wave[isr] = 6231.
bwidth[isr]  = 1388.
wave[isi] = 7625.
bwidth[isi]  = 1535.
wave[isz] = 9134.
bwidth[isz]  = 1409.
sdss_indices = [isu, isg, isr, isi, isz]
;
; 2MASS
; NOTE: GLGA 2MASS mags now in AB system (jdn, 24-FEB-2014)
wave[i2J] = master_filter[41].mean_wave
bwidth[i2J]  = 0.162*1.e4
;zpts[i2J]  = zpts[i2J] * 10.^(-0.4*master_filter[41].ab_offset)
wave[i2H] = master_filter[42].mean_wave
bwidth[i2H]  = 0.251*1.e4
;zpts[i2H]  = zpts[i2H] * 10.^(-0.4*master_filter[42].ab_offset)
wave[i2K] = master_filter[43].mean_wave
bwidth[i2K]  = 0.262*1.e4
;zpts[i2K]  = zpts[i2K] * 10.^(-0.4*master_filter[43].ab_offset)
twomass_indices = [i2J, i2H, i2K]
;
; WISE
wave[iw1] = 3.4003*1.e4
bwidth[iw1]  = 0.6358*1.e4
wave[iw2] = 4.6520*1.e4
bwidth[iw2]  = 1.1073*1.e4
wave[iw3] = 12.8109*1.e4
bwidth[iw3]  = 6.2758*1.e4
wave[iw4] = 22.3753*1.e4
bwidth[iw4]  = 4.7397*1.e4
wise_indices = [iw1, iw2, iw3, iw4]
;
; IRAS
wave[i12m]= 11.5980*1.e4
bwidth[i12m] = 6.9307*1.e4
wave[i25m]= 23.8789*1.e4
bwidth[i25m] = 11.2543*1.e4
wave[i60m]= 61.4850*1.e4
bwidth[i60m] = 32.7605*1.e4
wave[i100m]= 101.9803*1.e4
bwidth[i100m] = 32.2250*1.e4
iras_indices = [i12m, i25m, i60m, i100m]
;
; get latest timestamp
phot_ts = 0LL
;
; get GALEX GLGA QA status
galex_qa = glga_read_qa_stat(glga_dir+'galex/fits/'+gal+'_qa.txt', $
	stat=galex_qa_stat)
;
; GALEX FUV-band flux
;
; check GLGA first
if galex_qa_stat eq 1 then begin
    read_radprof,gal,'FUV',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprof=ppath, /silent, $
			phot_ts=pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	nap_mag = nap_mag[0] - glga_getextin(ebmv,'FUV',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		nap_magerr = nap_magerr[0] > 0.02 $
	else	nap_magerr = nap_magerr[0]	; preserve upper limits
	if nap_mag gt 0. then $
		flux[iFUV] = zpts[iFUV]*10.^(-0.4*nap_mag) $
	else	flux[iFUV] = -99.999		; prevent non-finite fluxes
	if nap_magerr ge 0. then $
		fluxerr[iFUV] = flux[iFUV]*nap_magerr / 1.0857362d0 $
	else	fluxerr[iFUV] = -1.
	source[iFUV] = 1
    endif
endif
;
; Check literature if needed
if flux[iFUV] le 0. and galdat[g].fuv_int_mag gt 0. then begin
	flux[iFUV] = zpts[iFUV]*10.^(-0.4*(galdat[g].fuv_int_mag))
	if galdat[g].fuv_int_magerr ge 0. then $
		fluxerr[iFUV]= flux[iFUV]*galdat[g].fuv_int_magerr/1.0857362d0 $
	else	fluxerr[iFUV]= -1.
	source[iFUV] = 2
endif
if flux[iFUV] gt 0 and not keyword_set(silent) then $
	print,'GALEX FUV :',wave[iFUV],bwidth[iFUV],source[iFUV], $
		flux[iFUV],'+-',fluxerr[iFUV],form=opfmt
;
; GALEX NUV-band flux
;
; check GLGA first
if galex_qa_stat eq 1 then begin
    read_radprof,gal,'NUV',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	nap_mag = nap_mag[0] - glga_getextin(ebmv,'NUV',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		nap_magerr = nap_magerr[0] > 0.02 $
	else	nap_magerr = nap_magerr[0]	; preserve upper limits
	if nap_mag gt 0. then $
		flux[iNUV] = zpts[iNUV]*10.^(-0.4*nap_mag) $
	else	flux[iNUV] = -99.999		; prevent non-finite fluxes
	if nap_magerr ge 0. then $
		fluxerr[iNUV] = flux[iNUV]*nap_magerr / 1.0857362d0 $
	else	fluxerr[iNUV] = -1.
	source[iNUV] = 1
    endif
endif
;
; Check literature if needed
if flux[iNUV] le 0. and galdat[g].nuv_int_mag gt 0. then begin
	flux[iNUV] = zpts[iNUV]*10.^(-0.4*(galdat[g].nuv_int_mag))
	if galdat[g].nuv_int_magerr ge 0. then $
		fluxerr[iNUV]= flux[iNUV]*galdat[g].nuv_int_magerr/1.0857362d0 $
	else	fluxerr[iNUV]= -1.
	source[iNUV] = 2
endif
if flux[iNUV] gt 0 and not keyword_set(silent) then $
	print,'GALEX NUV :',wave[iNUV],bwidth[iNUV],source[iNUV], $
		flux[iNUV],'+-',fluxerr[iNUV],form=opfmt
;
; Johnson U-band flux
if galdat[g].uJ_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'U'))
	flux[iJu] = flux_to_apparent_mag(galdat[g].uJ_int_mag,ifilter=f[0], $
		apparent_mag_err=fxe, flux_error=galdat[g].uJ_int_magerr, $
		/vegasys, /reverse) * flam2Jy * wave[iJu]^2
	fluxerr[iJu] = fxe * flam2Jy * wave[iJu]^2
	source[iJu] = 2
	if not keyword_set(silent) then $
	    print,'Johnson U :',wave[iJu],bwidth[iJu],source[iJu], $
	    	flux[iJu],'+-',fluxerr[iJu],form=opfmt
endif
;
; Johnson B-band flux
if galdat[g].bJ_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'B'))
	flux[iJb] = flux_to_apparent_mag(galdat[g].bJ_int_mag,ifilter=f[0], $
		apparent_mag_err=fxe, flux_error=galdat[g].bJ_int_magerr, $
		/vegasys, /reverse) * flam2Jy * wave[iJb]^2
	fluxerr[iJb] = fxe * flam2Jy * wave[iJb]^2
	source[iJb] = 2
	if not keyword_set(silent) then $
	    print,'Johnson B :',wave[iJb],bwidth[iJb],source[iJb], $
	    	flux[iJb],'+-',fluxerr[iJb],form=opfmt
endif
;
; Johnson V-band flux
if galdat[g].vJ_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'V'))
	flux[iJv] = flux_to_apparent_mag(galdat[g].vJ_int_mag,ifilter=f[0], $
		apparent_mag_err=fxe, flux_error=galdat[g].vJ_int_magerr, $
		/vegasys, /reverse) * flam2Jy * wave[iJv]^2
	fluxerr[iJv] = fxe * flam2Jy * wave[iJv]^2
	source[iJv] = 2
	if not keyword_set(silent) then $
	    print,'Johnson V :',wave[iJv],bwidth[iJv],source[iJv], $
	    	flux[iJv],'+-',fluxerr[iJv],form=opfmt
endif
;
; get SDSS GLGA QA status
sdss_qa = glga_read_qa_stat(glga_dir+'sdss/fits/'+gal+'_qa.txt', $
	stat=sdss_qa_stat)
;
; SDSS u-band flux
;
; check GLGA first
if sdss_qa_stat eq 1 then begin
    read_radprof,gal,'u',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	nap_mag = nap_mag[0] - glga_getextin(ebmv,'u',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		nap_magerr = nap_magerr[0] > 0.02 $
	else	nap_magerr = nap_magerr[0]	; preserve upper limits
	if nap_mag gt 0. then $
		flux[isu] = zpts[isu]*10.^(-0.4*nap_mag) $
	else	flux[isu] = -99.999		; prevent non-finite fluxes
	if nap_magerr ge 0. then $
		fluxerr[isu] = flux[isu]*nap_magerr / 1.0857362d0 $
	else	fluxerr[isu] = -1.
	source[isu] = 1
    endif
endif
;
; Check literature if needed
if flux[isu] le 0. and galdat[g].u_int_mag gt 0. then begin
	flux[isu] = zpts[isu]*10.^(-0.4*(galdat[g].u_int_mag))
	if galdat[g].u_int_magerr ge 0. then $
		fluxerr[isu]= flux[isu]*galdat[g].u_int_magerr/1.0857362d0 $
	else	fluxerr[isu]= -1.
	source[isu] = 2
endif
if flux[isu] gt 0 and not keyword_set(silent) then $
	print,'SDSS u :',wave[isu],bwidth[isu],source[isu], $
		flux[isu],'+-',fluxerr[isu],form=opfmt
;
; SDSS g-band flux
;
; check GLGA first
if sdss_qa_stat eq 1 then begin
    read_radprof,gal,'g',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	nap_mag = nap_mag[0] - glga_getextin(ebmv,'g',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		nap_magerr = nap_magerr[0] > 0.02 $
	else	nap_magerr = nap_magerr[0]	; preserve upper limits
	if nap_mag gt 0. then $
		flux[isg] = zpts[isg]*10.^(-0.4*nap_mag) $
	else	flux[isg] = -99.999		; prevent non-finite fluxes
	if nap_magerr ge 0. then $
		fluxerr[isg] = flux[isg]*nap_magerr / 1.0857362d0 $
	else	fluxerr[isg] = -1.
	source[isg] = 1
    endif
endif
;
; Check literature if needed
if flux[isg] le 0. and galdat[g].g_int_mag gt 0. then begin
	flux[isg] = zpts[isg]*10.^(-0.4*(galdat[g].g_int_mag))
	if galdat[g].g_int_magerr ge 0. then $
		fluxerr[isg]= flux[isg]*galdat[g].g_int_magerr/1.0857362d0 $
	else	fluxerr[isg]= -1.
	source[isg] = 2
endif
if flux[isg] gt 0 and not keyword_set(silent) then $
	print,'SDSS g :',wave[isg],bwidth[isg],source[isg], $
		flux[isg],'+-',fluxerr[isg],form=opfmt
;
; SDSS r-band flux
;
; check GLGA first
if sdss_qa_stat eq 1 then begin
    read_radprof,gal,'r',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	nap_mag = nap_mag[0] - glga_getextin(ebmv,'r',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		nap_magerr = nap_magerr[0] > 0.02 $
	else	nap_magerr = nap_magerr[0]	; preserve upper limits
	if nap_mag gt 0. then $
		flux[isr] = zpts[isr]*10.^(-0.4*nap_mag) $
	else	flux[isr] = -99.999		; prevent non-finite fluxes
	if nap_magerr ge 0. then $
		fluxerr[isr] = flux[isr]*nap_magerr / 1.0857362d0 $
	else	fluxerr[isr] = -1.
	source[isr] = 1
    endif
endif
;
; Check literature if needed
if flux[isr] le 0. and galdat[g].r_int_mag gt 0. then begin
	flux[isr] = zpts[isr]*10.^(-0.4*(galdat[g].r_int_mag))
	if galdat[g].r_int_magerr ge 0. then $
		fluxerr[isr]= flux[isr]*galdat[g].r_int_magerr/1.0857362d0 $
	else	fluxerr[isr]= -1.
	source[isr] = 2
endif
if flux[isr] gt 0 and not keyword_set(silent) then $
	print,'SDSS r :',wave[isr],bwidth[isr],source[isr], $
		flux[isr],'+-',fluxerr[isr],form=opfmt
;
; SDSS i-band flux
;
; check GLGA first
if sdss_qa_stat eq 1 then begin
    read_radprof,gal,'i',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	nap_mag = nap_mag[0] - glga_getextin(ebmv,'i',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		nap_magerr = nap_magerr[0] > 0.02 $
	else	nap_magerr = nap_magerr[0]	; preserve upper limits
	if nap_mag gt 0. then $
		flux[isi] = zpts[isi]*10.^(-0.4*nap_mag) $
	else	flux[isi] = -99.999		; prevent non-finite fluxes
	if nap_magerr ge 0. then $
		fluxerr[isi] = flux[isi]*nap_magerr / 1.0857362d0 $
	else	fluxerr[isi] = -1.
	source[isi] = 1
    endif
endif
;
; Check literature if needed
if flux[isi] le 0. and galdat[g].i_int_mag gt 0. then begin
	flux[isi] = zpts[isi]*10.^(-0.4*(galdat[g].i_int_mag))
	if galdat[g].i_int_magerr ge 0. then $
		fluxerr[isi]= flux[isi]*galdat[g].i_int_magerr/1.0857362d0 $
	else	fluxerr[isi]= -1.
	source[isi] = 2
endif
if flux[isi] gt 0 and not keyword_set(silent) then $
	print,'SDSS i :',wave[isi],bwidth[isi],source[isi], $
		flux[isi],'+-',fluxerr[isi],form=opfmt
;
; SDSS z-band flux
;
; check GLGA first
if sdss_qa_stat eq 1 then begin
    read_radprof,gal,'z',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	nap_mag = nap_mag[0] - glga_getextin(ebmv,'z',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		nap_magerr = nap_magerr[0] > 0.02 $
	else	nap_magerr = nap_magerr[0]	; preserve upper limits
	if nap_mag gt 0. then $
		flux[isz] = zpts[isz]*10.^(-0.4*nap_mag) $
	else	flux[isz] = -99.999		; prevent non-finite fluxes
	if nap_magerr ge 0. then $
		fluxerr[isz] = flux[isz]*nap_magerr / 1.0857362d0 $
	else	fluxerr[isz] = -1.
	source[isz] = 1
    endif
endif
;
; Check literature if needed
if flux[isz] le 0. and galdat[g].z_int_mag gt 0. then begin
	flux[isz] = zpts[isz]*10.^(-0.4*(galdat[g].z_int_mag))
	if galdat[g].z_int_magerr ge 0. then $
		fluxerr[isz]= flux[isz]*galdat[g].z_int_magerr/1.0857362d0 $
	else	fluxerr[isz]= -1.
	source[isz] = 2
endif
if flux[isz] gt 0 and not keyword_set(silent) then $
	print,'SDSS z :',wave[isz],bwidth[isz],source[isz], $
		flux[isz],'+-',fluxerr[isz],form=opfmt
;
; get 2MASS GLGA QA status
twomass_qa = glga_read_qa_stat(glga_dir+'2mass/fits/'+gal+'_qa.txt', $
	stat=twomass_qa_stat)
;
; 2MASS J-band flux
;
; check GLGA first
if twomass_qa_stat eq 1 then begin
    read_radprof,gal,'j',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	nap_mag = nap_mag[0] - glga_getextin(ebmv,'j',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		nap_magerr = nap_magerr[0] > 0.02 $
	else	nap_magerr = nap_magerr[0]	; preserve upper limits
	if nap_mag gt 0. then $
		flux[i2J] = zpts[i2J]*10.^(-0.4*nap_mag) $
	else	flux[i2J] = -99.999		; prevent non-finite fluxes
	if nap_magerr ge 0. then $
		fluxerr[i2J] = flux[i2J]*nap_magerr / 1.0857362d0 $
	else	fluxerr[i2J] = -1.
	source[i2J] = 1
    endif
endif
;
; Check literature if needed
if flux[i2J] le 0. and galdat[g].J_int_mag gt 0. then begin
	cmag = galdat[g].J_int_mag - glga_getextin(ebmv,'j',yuan13=yuan13)
	f=where(strcmp(master_filter.shortid,'J-2mass'))
	flux[i2J] = flux_to_apparent_mag(cmag,ifilter=f[0], $
		apparent_mag_err=fxe, flux_error=galdat[g].J_int_magerr, $
		/vegasys, /reverse) * flam2Jy * wave[i2J]^2
	fluxerr[i2J] = fxe * flam2Jy * wave[i2J]^2
	source[i2J] = 2
endif
if flux[i2J] gt 0 and not keyword_set(silent) then $
	print,'2MASS J :',wave[i2J],bwidth[i2J],source[i2J], $
		flux[i2J],'+-',fluxerr[i2J],form=opfmt
;
; 2MASS H-band flux
;
; check GLGA first
if twomass_qa_stat eq 1 then begin
    read_radprof,gal,'h',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	nap_mag = nap_mag[0] - glga_getextin(ebmv,'h',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		nap_magerr = nap_magerr[0] > 0.02 $
	else	nap_magerr = nap_magerr[0]	; preserve upper limits
	if nap_mag gt 0. then $
		flux[i2H] = zpts[i2H]*10.^(-0.4*nap_mag) $
	else	flux[i2H] = -99.999		; prevent non-finite fluxes
	if nap_magerr ge 0. then $
		fluxerr[i2H] = flux[i2H]*nap_magerr / 1.0857362d0 $
	else	fluxerr[i2H] = -1.
	source[i2H] = 1
    endif
endif
;
; Check literature if needed
if flux[i2H] le 0. and galdat[g].H_int_mag gt 0. then begin
	cmag = galdat[g].H_int_mag - glga_getextin(ebmv,'h',yuan13=yuan13)
	f=where(strcmp(master_filter.shortid,'H-2mass'))
	flux[i2H] = flux_to_apparent_mag(cmag,ifilter=f[0], $
		apparent_mag_err=fxe, flux_error=galdat[g].H_int_magerr, $
		/vegasys, /reverse) * flam2Jy * wave[i2H]^2
	fluxerr[i2H] = fxe * flam2Jy * wave[i2H]^2
	source[i2H] = 2
endif
if flux[i2H] gt 0 and not keyword_set(silent) then $
	print,'2MASS H :',wave[i2H],bwidth[i2H],source[i2H], $
		flux[i2H],'+-',fluxerr[i2H],form=opfmt
;
; 2MASS K-band flux
;
; check GLGA first
if twomass_qa_stat eq 1 then begin
    read_radprof,gal,'k',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	nap_mag = nap_mag[0] - glga_getextin(ebmv,'k',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		nap_magerr = nap_magerr[0] > 0.02 $
	else	nap_magerr = nap_magerr[0]	; preserve upper limits
	if nap_mag gt 0. then $
		flux[i2K] = zpts[i2K]*10.^(-0.4*nap_mag) $
	else	flux[i2K] = -99.999		; prevent non-finite fluxes
	if nap_magerr ge 0. then $
		fluxerr[i2K] = flux[i2K]*nap_magerr / 1.0857362d0 $
	else	fluxerr[i2K] = -1.
	source[i2K] = 1
    endif
endif
;
; Check literature if needed
if flux[i2K] le 0. and galdat[g].K_int_mag gt 0. then begin
	cmag = galdat[g].K_int_mag - glga_getextin(ebmv,'k',yuan13=yuan13)
	f=where(strcmp(master_filter.shortid,'Ks-2mass'))
	flux[i2K] = flux_to_apparent_mag(cmag,ifilter=f[0], $
		apparent_mag_err=fxe, flux_error=galdat[g].K_int_magerr, $
		/vegasys, /reverse) * flam2Jy * wave[i2K]^2
	fluxerr[i2K] = fxe * flam2Jy * wave[i2K]^2
	source[i2K] = 2
endif
if flux[i2K] gt 0 and not keyword_set(silent) then $
	print,'2MASS K :',wave[i2K],bwidth[i2K],source[i2K], $
		flux[i2K],'+-',fluxerr[i2K],form=opfmt
;
; get WISE GLGA QA status
wise_qa = glga_read_qa_stat(glga_dir+'wise/fits/'+gal+'_qa.txt', $
	stat=wise_qa_stat)
;
; WISE W1 flux
;
; check GLGA first
if wise_qa_stat eq 1 then begin
    read_radprof,gal,'w1',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	nap_mag = nap_mag[0] - glga_getextin(ebmv,'w1',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		nap_magerr = nap_magerr[0] > 0.02 $
	else	nap_magerr = nap_magerr[0]	; preserve upper limits
	if nap_mag gt 0. then $
		flux[iw1] = zpts[iw1]*10.^(-0.4*nap_mag) $
	else	flux[iw1] = -99.999		; prevent non-finite fluxes
	if nap_magerr ge 0. then $
		fluxerr[iw1] = flux[iw1]*nap_magerr / 1.0857362d0 $
	else	fluxerr[iw1] = -1.
	source[iw1] = 1
    endif
endif
;
; Check literature if needed
if flux[iw1] le 0. and galdat[g].w1_int_mag gt 0. then begin
	flux[iw1] = zpts[iw1]*10.^(-0.4*(galdat[g].w1_int_mag))
	if galdat[g].w1_int_magerr ge 0. then $
		fluxerr[iw1]= flux[iw1]*galdat[g].w1_int_magerr/1.0857362d0 $
	else	fluxerr[iw1]= -1.
	source[iw1] = 2
endif
if flux[iw1] gt 0 and not keyword_set(silent) then $
	print,'WISE w1 :',wave[iw1],bwidth[iw1],source[iw1], $
		flux[iw1],'+-',fluxerr[iw1],form=opfmt
;
; WISE W2 flux
;
; check GLGA first
if wise_qa_stat eq 1 then begin
    read_radprof,gal,'w2',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	nap_mag = nap_mag[0] - glga_getextin(ebmv,'w2',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		nap_magerr = nap_magerr[0] > 0.02 $
	else	nap_magerr = nap_magerr[0]	; preserve upper limits
	if nap_mag gt 0. then $
		flux[iw2] = zpts[iw2]*10.^(-0.4*nap_mag) $
	else	flux[iw2] = -99.999		; prevent non-finite fluxes
	if nap_magerr ge 0. then $
		fluxerr[iw2] = flux[iw2]*nap_magerr / 1.0857362d0 $
	else	fluxerr[iw2] = -1.
	source[iw2] = 1
    endif
endif
;
; Check literature if needed
if flux[iw2] le 0. and galdat[g].w2_int_mag gt 0. then begin
	flux[iw2] = zpts[iw2]*10.^(-0.4*(galdat[g].w2_int_mag))
	if galdat[g].w2_int_magerr ge 0. then $
		fluxerr[iw2]= flux[iw2]*galdat[g].w2_int_magerr/1.0857362d0 $
	else	fluxerr[iw2]= -1.
	source[iw2] = 2
endif
if flux[iw2] gt 0 and not keyword_set(silent) then $
	print,'WISE w2 :',wave[iw2],bwidth[iw2],source[iw2], $
		flux[iw2],'+-',fluxerr[iw2],form=opfmt
;
; WISE W3 flux
;
; check GLGA first
if wise_qa_stat eq 1 then begin
    read_radprof,gal,'w3',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	nap_mag = nap_mag[0] - glga_getextin(ebmv,'w3',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		nap_magerr = nap_magerr[0] > 0.02 $
	else	nap_magerr = nap_magerr[0]	; preserve upper limits
	if nap_mag gt 0. then $
		flux[iw3] = zpts[iw3]*10.^(-0.4*nap_mag) $
	else	flux[iw3] = -99.999		; prevent non-finite fluxes
	if nap_magerr ge 0. then $
		fluxerr[iw3] = flux[iw3]*nap_magerr / 1.0857362d0 $
	else	fluxerr[iw3] = -1.
	source[iw3] = 1
    endif
endif
;
; Check literature if needed
if flux[iw3] le 0. and galdat[g].w3_int_mag gt 0. then begin
	flux[iw3] = zpts[iw3]*10.^(-0.4*(galdat[g].w3_int_mag))
	if galdat[g].w3_int_magerr ge 0. then $
		fluxerr[iw3]= flux[iw3]*galdat[g].w3_int_magerr/1.0857362d0 $
	else	fluxerr[iw3]= -1.
	source[iw3] = 2
endif
if flux[iw3] gt 0 and not keyword_set(silent) then $
	print,'WISE w3 :',wave[iw3],bwidth[iw3],source[iw3], $
		flux[iw3],'+-',fluxerr[iw3],form=opfmt
;
; WISE W4 flux
;
; check GLGA first
if wise_qa_stat eq 1 then begin
    read_radprof,gal,'w4',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	nap_mag = nap_mag[0] - glga_getextin(ebmv,'w4',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		nap_magerr = nap_magerr[0] > 0.02 $
	else	nap_magerr = nap_magerr[0]	; preserve upper limits
	if nap_mag gt 0. then $
		flux[iw4] = zpts[iw4]*10.^(-0.4*nap_mag) $
	else	flux[iw4] = -99.999		; prevent non-finite fluxes
	if nap_magerr ge 0. then $
		fluxerr[iw4] = flux[iw4]*nap_magerr / 1.0857362d0 $
	else	fluxerr[iw4] = -1.
	source[iw4] = 1
    endif
endif
;
; Check literature if needed
if flux[iw4] le 0. and galdat[g].w4_int_mag gt 0. then begin
	flux[iw4] = zpts[iw4]*10.^(-0.4*(galdat[g].w4_int_mag))
	if galdat[g].w4_int_magerr ge 0. then $
		fluxerr[iw4]= flux[iw4]*galdat[g].w4_int_magerr/1.0857362d0 $
	else	fluxerr[iw4]= -1.
	source[iw4] = 2
endif
if flux[iw4] gt 0 and not keyword_set(silent) then $
	print,'WISE w4 :',wave[iw4],bwidth[iw4],source[iw4], $
		flux[iw4],'+-',fluxerr[iw4],form=opfmt
;
; IRAS 12-mu flux
if galdat[g].i12m_int_mag gt 0. then begin
	flux[i12m] = zpts[i12m]*10.^(-0.4*(galdat[g].i12m_int_mag))
	fluxerr[i12m]= flux[i12m]*galdat[g].i12m_int_magerr/1.0857362d0
	source[i12m] = 2
	if not keyword_set(silent) then $
	    print,'IRAS 12-mu :',wave[i12m],bwidth[i12m],source[i12m], $
	    	flux[i12m],'+-',fluxerr[i12m],form=opfmt
endif
;
; IRAS 25-mu flux
if galdat[g].i25m_int_mag gt 0. then begin
	flux[i25m] = zpts[i25m]*10.^(-0.4*(galdat[g].i25m_int_mag))
	fluxerr[i25m]= flux[i25m]*galdat[g].i25m_int_magerr/1.0857362d0
	source[i25m] = 2
	if not keyword_set(silent) then $
	    print,'IRAS 25-mu :',wave[i25m],bwidth[i25m],source[i25m], $
	    	flux[i25m],'+-',fluxerr[i25m],form=opfmt
endif
;
; IRAS 60-mu flux
if galdat[g].i60m_int_mag gt 0. then begin
	flux[i60m] = zpts[i60m]*10.^(-0.4*(galdat[g].i60m_int_mag))
	fluxerr[i60m]= flux[i60m]*galdat[g].i60m_int_magerr/1.0857362d0
	source[i60m] = 2
	if not keyword_set(silent) then $
	    print,'IRAS 60-mu :',wave[i60m],bwidth[i60m],source[i60m], $
	   	 flux[i60m],'+-',fluxerr[i60m],form=opfmt
endif
;
; IRAS 100-mu flux
if galdat[g].i100m_int_mag gt 0. then begin
	flux[i100m] = zpts[i100m]*10.^(-0.4*(galdat[g].i100m_int_mag))
	fluxerr[i100m]= flux[i100m]*galdat[g].i100m_int_magerr/1.0857362d0
	source[i100m] = 2
	if not keyword_set(silent) then $
	   print,'IRAS 100-mu :',wave[i100m],bwidth[i100m],source[i100m], $
	   	flux[i100m],'+-',fluxerr[i100m],form=opfmt
endif
;
return
end
