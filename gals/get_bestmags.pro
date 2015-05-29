pro get_bestmags, galid, mags, merrs, wave, bwidth, source, phot_ts, $
	yuan13=yuan13, status=status, verbose=verbose, silent=silent, $
	galex_indices=galex_indices, sdss_indices=sdss_indices, $
	rc3_indices=rc3_indices, twomass_indices=twomass_indices, $
	wise_indices=wise_indices, iras_indices=iras_indices
;+
;	get_bestmags - get the best extinction-corrected AB mags
;
; CALLING SEQUENCE:
;	get_bestmags, galid, mags, merrs, wave, bwidth, source
;
; INPUTS:
;	galid	- galaxy id as specified in galdat struct
;
; OUTPUTS:
;	mags	- AB mags for each filter
;	merrs	- AB mag errs for each filter
;	wave	- wavelengths of filters in Angstroms
;	bwidth	- width of filter filters in Angstroms
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
; HISTORY:
; $Id: get_bestmags.pro,v 1.12 2014/02/24 17:14:48 neill Exp $
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
opfmt = '(a13,f9.1,1x,f8.1,i4,f9.2,1x,a2,1x,f6.2)'
if not keyword_set(silent) then $
	print,'       BAND       WAVE     BWID  SRC  MAG(AB)    ERROR'
;
; set up constants
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
source	= intarr(nwavs)		; 2 - literature, 1 - GLGA, 0 - none
mags	= fltarr(nwavs)-99.	; AB mag
merrs	= fltarr(nwavs)		; AB mag
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
wave[iJb] = master_filter[66].mean_wave
bwidth[iJb]  = 1008.
wave[iJv] = master_filter[67].mean_wave
bwidth[iJv]  = 826.
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
wave[i2J] = master_filter[41].mean_wave
bwidth[i2J]  = 0.162*1.e4
wave[i2H] = master_filter[42].mean_wave
bwidth[i2H]  = 0.251*1.e4
wave[i2K] = master_filter[43].mean_wave
bwidth[i2K]  = 0.262*1.e4
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
; GALEX FUV-band mag
;
; check GLGA first
if galex_qa_stat eq 1 then begin
    read_radprof,gal,'FUV',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprof=ppath, /silent, $
			phot_ts=pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	mags[iFUV] = nap_mag[0] - glga_getextin(ebmv,'FUV',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		merrs[iFUV] = nap_magerr[0] > 0.02 $
	else	merrs[iFUV] = nap_magerr[0]	; preserve upper limits
	source[iFUV] = 1
    endif
endif
;
; Check literature if needed
if mags[iFUV] le 0. and galdat[g].fuv_int_mag gt 0. then begin
	mags[iFUV] = galdat[g].fuv_int_mag
	merrs[iFUV] = galdat[g].fuv_int_magerr
	source[iFUV] = 2
endif
if mags[iFUV] gt 0 and not keyword_set(silent) then $
	print,'GALEX FUV :',wave[iFUV],bwidth[iFUV],source[iFUV], $
		mags[iFUV],'+-',merrs[iFUV],form=opfmt
;
; GALEX NUV-band mags
;
; check GLGA first
if galex_qa_stat eq 1 then begin
    read_radprof,gal,'NUV',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	mags[iNUV] = nap_mag[0] - glga_getextin(ebmv,'NUV',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		merrs[iNUV] = nap_magerr[0] > 0.02 $
	else	merrs[iNUV] = nap_magerr[0]	; preserve upper limits
	source[iNUV] = 1
    endif
endif
;
; Check literature if needed
if mags[iNUV] le 0. and galdat[g].nuv_int_mag gt 0. then begin
	mags[iNUV] = galdat[g].nuv_int_mag
	merrs[iNUV] = galdat[g].nuv_int_magerr > 0.01
	source[iNUV] = 2
endif
if mags[iNUV] gt 0 and not keyword_set(silent) then $
	print,'GALEX NUV :',wave[iNUV],bwidth[iNUV],source[iNUV], $
		mags[iNUV],'+-',merrs[iNUV],form=opfmt
;
; Johnson U-band mag
if galdat[g].uJ_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'U'))
	mags[iJu] = galdat[g].uJ_int_mag + master_filter[f].ab_offset
	merrs[iJu] = galdat[g].uJ_int_magerr > 0.01
	source[iJu] = 2
	if not keyword_set(silent) then $
	    print,'Johnson U :',wave[iJu],bwidth[iJu],source[iJu], $
	    	mags[iJu],'+-',merrs[iJu],form=opfmt
endif
;
; Johnson B-band mag
if galdat[g].bJ_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'B'))
	mags[iJb] = galdat[g].bJ_int_mag + master_filter[f].ab_offset
	merrs[iJb] = galdat[g].bJ_int_magerr > 0.01
	source[iJb] = 2
	if not keyword_set(silent) then $
	    print,'Johnson B :',wave[iJb],bwidth[iJb],source[iJb], $
	    	mags[iJb],'+-',merrs[iJb],form=opfmt
endif
;
; Johnson V-band mag
if galdat[g].vJ_int_mag gt 0. then begin
	f=where(strcmp(master_filter.shortid,'V'))
	mags[iJv] = galdat[g].vJ_int_mag + master_filter[f].ab_offset
	merrs[iJv] = galdat[g].vJ_int_magerr > 0.01
	source[iJv] = 2
	if not keyword_set(silent) then $
	    print,'Johnson V :',wave[iJv],bwidth[iJv],source[iJv], $
	    	mags[iJv],'+-',merrs[iJv],form=opfmt
endif
;
; get SDSS GLGA QA status
sdss_qa = glga_read_qa_stat(glga_dir+'sdss/fits/'+gal+'_qa.txt', $
	stat=sdss_qa_stat)
;
; SDSS u-band mags
;
; check GLGA first
if sdss_qa_stat eq 1 then begin
    read_radprof,gal,'u',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	mags[isu] = nap_mag[0] - glga_getextin(ebmv,'u',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		merrs[isu] = nap_magerr[0] > 0.02 $
	else	merrs[isu] = nap_magerr[0]	; preserve upper limits
	source[isu] = 1
    endif
endif
;
; Check literature if needed
if mags[isu] le 0. and galdat[g].u_int_mag gt 0. then begin
	mags[isu] = galdat[g].u_int_mag
	merrs[isu] = galdat[g].u_int_magerr > 0.02
	source[isu] = 2
endif
if mags[isu] gt 0 and not keyword_set(silent) then $
	print,'SDSS u :',wave[isu],bwidth[isu],source[isu], $
		mags[isu],'+-',merrs[isu],form=opfmt
;
; SDSS g-band mag
;
; check GLGA first
if sdss_qa_stat eq 1 then begin
    read_radprof,gal,'g',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	mags[isg] = nap_mag[0] - glga_getextin(ebmv,'g',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		merrs[isg] = nap_magerr[0] > 0.02 $
	else	merrs[isg] = nap_magerr[0]	; preserve upper limits
	source[isg] = 1
    endif
endif
;
; Check literature if needed
if mags[isg] le 0. and galdat[g].g_int_mag gt 0. then begin
	mags[isg] = galdat[g].g_int_mag
	merrs[isg] = galdat[g].g_int_magerr > 0.02
	source[isg] = 2
endif
if mags[isg] gt 0 and not keyword_set(silent) then $
	print,'SDSS g :',wave[isg],bwidth[isg],source[isg], $
		mags[isg],'+-',merrs[isg],form=opfmt
;
; SDSS r-band mag
;
; check GLGA first
if sdss_qa_stat eq 1 then begin
    read_radprof,gal,'r',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	mags[isr] = nap_mag[0] - glga_getextin(ebmv,'r',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		merrs[isr] = nap_magerr[0] > 0.02 $
	else	merrs[isr] = nap_magerr[0]	; preserve upper limits
	source[isr] = 1
    endif
endif
;
; Check literature if needed
if mags[isr] le 0. and galdat[g].r_int_mag gt 0. then begin
	mags[isr] = galdat[g].r_int_mag
	merrs[isr] = galdat[g].r_int_magerr > 0.02
	source[isr] = 2
endif
if mags[isr] gt 0 and not keyword_set(silent) then $
	print,'SDSS r :',wave[isr],bwidth[isr],source[isr], $
		mags[isr],'+-',merrs[isr],form=opfmt
;
; SDSS i-band mag
;
; check GLGA first
if sdss_qa_stat eq 1 then begin
    read_radprof,gal,'i',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	mags[isi] = nap_mag[0] - glga_getextin(ebmv,'i',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		merrs[isi] = nap_magerr[0] > 0.02 $
	else	merrs[isi] = nap_magerr[0]	; preserve upper limits
	source[isi] = 1
    endif
endif
;
; Check literature if needed
if mags[isi] le 0. and galdat[g].i_int_mag gt 0. then begin
	mags[isi] = galdat[g].i_int_mag
	merrs[isi] = galdat[g].i_int_magerr > 0.02
	source[isi] = 2
endif
if mags[isi] gt 0 and not keyword_set(silent) then $
	print,'SDSS i :',wave[isi],bwidth[isi],source[isi], $
		mags[isi],'+-',merrs[isi],form=opfmt
;
; SDSS z-band mag
;
; check GLGA first
if sdss_qa_stat eq 1 then begin
    read_radprof,gal,'z',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	mags[isz] = nap_mag[0] - glga_getextin(ebmv,'z',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		merrs[isz] = nap_magerr[0] > 0.02 $
	else	merrs[isz] = nap_magerr[0]	; preserve upper limits
	source[isz] = 1
    endif
endif
;
; Check literature if needed
if mags[isz] le 0. and galdat[g].z_int_mag gt 0. then begin
	mags[isz] = galdat[g].z_int_mag
	merrs[isz] = galdat[g].z_int_magerr > 0.02
	source[isz] = 2
endif
if mags[isz] gt 0 and not keyword_set(silent) then $
	print,'SDSS z :',wave[isz],bwidth[isz],source[isz], $
		mags[isz],'+-',merrs[isz],form=opfmt
;
; get 2MASS GLGA QA status
twomass_qa = glga_read_qa_stat(glga_dir+'2mass/fits/'+gal+'_qa.txt', $
	stat=twomass_qa_stat)
;
; NOTE: GLGA 2MASS mags are now in AB (jdn, 24-FEB-2014)
;
; 2MASS J-band mag
f=where(strcmp(master_filter.shortid,'J-2mass'))
;
; check GLGA first
if twomass_qa_stat eq 1 then begin
    read_radprof,gal,'j',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	mags[i2J] = nap_mag[0] - glga_getextin(ebmv,'j',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		merrs[i2J] = nap_magerr[0] > 0.02 $
	else	merrs[i2J] = nap_magerr[0]	; preserve upper limits
	source[i2J] = 1
    endif
endif
;
; Check literature if needed
if mags[i2J] le 0. and galdat[g].J_int_mag gt 0. then begin
	mags[i2J] = galdat[g].J_int_mag + master_filter[f].ab_offset	; Vega -> AB
	merrs[i2J] = galdat[g].J_int_magerr > 0.01
	source[i2J] = 2
endif
if mags[i2J] gt 0 and not keyword_set(silent) then $
	print,'2MASS J :',wave[i2J],bwidth[i2J],source[i2J], $
		mags[i2J],'+-',merrs[i2J],form=opfmt
;
; 2MASS H-band mag
f=where(strcmp(master_filter.shortid,'H-2mass'))
;
; check GLGA first
if twomass_qa_stat eq 1 then begin
    read_radprof,gal,'h',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	mags[i2H] = nap_mag[0] - glga_getextin(ebmv,'h',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		merrs[i2H] = nap_magerr[0] > 0.02 $
	else	merrs[i2H] = nap_magerr[0]	; preserve upper limits
	source[i2H] = 1
    endif
endif
;
; Check literature if needed
if mags[i2H] le 0. and galdat[g].H_int_mag gt 0. then begin
	mags[i2H] = galdat[g].H_int_mag + master_filter[f].ab_offset	; Vega -> AB
	merrs[i2H] = galdat[g].H_int_magerr > 0.01
	source[i2H] = 2
endif
if mags[i2H] gt 0 and not keyword_set(silent) then $
	print,'2MASS H :',wave[i2H],bwidth[i2H],source[i2H], $
		mags[i2H],'+-',merrs[i2H],form=opfmt
;
; 2MASS K-band mag
f=where(strcmp(master_filter.shortid,'Ks-2mass'))
;
; check GLGA first
if twomass_qa_stat eq 1 then begin
    read_radprof,gal,'k',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	mags[i2K] = nap_mag[0] - glga_getextin(ebmv,'k',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		merrs[i2K] = nap_magerr[0] > 0.02 $
	else	merrs[i2K] = nap_magerr[0]	; preserve upper limits
	source[i2K] = 1
    endif
endif
;
; Check literature if needed
if mags[i2K] le 0. and galdat[g].K_int_mag gt 0. then begin
	mags[i2K] = galdat[g].H_int_mag + master_filter[f].ab_offset	; Vega -> AB
	merrs[i2K] = galdat[g].K_int_magerr > 0.01
	source[i2K] = 2
endif
if mags[i2K] gt 0 and not keyword_set(silent) then $
	print,'2MASS K :',wave[i2K],bwidth[i2K],source[i2K], $
		mags[i2K],'+-',merrs[i2K],form=opfmt
;
; get WISE GLGA QA status
wise_qa = glga_read_qa_stat(glga_dir+'wise/fits/'+gal+'_qa.txt', $
	stat=wise_qa_stat)
;
; WISE W1 mag
;
; check GLGA first
if wise_qa_stat eq 1 then begin
    read_radprof,gal,'w1',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	mags[iw1] = nap_mag[0] - glga_getextin(ebmv,'w1',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		merrs[iw1] = nap_magerr[0] > 0.02 $
	else	merrs[iw1] = nap_magerr[0]	; preserve upper limits
	source[iw1] = 1
    endif
endif
;
; Check literature if needed
if mags[iw1] le 0. and galdat[g].w1_int_mag gt 0. then begin
	mags[iw1] = galdat[g].w1_int_mag + 2.699	; Vega -> AB
	merrs[iw1] = galdat[g].w1_int_magerr > 0.01
	source[iw1] = 2
endif
if mags[iw1] gt 0 and not keyword_set(silent) then $
	print,'WISE w1 :',wave[iw1],bwidth[iw1],source[iw1], $
		mags[iw1],'+-',merrs[iw1],form=opfmt
;
; WISE W2 mag
;
; check GLGA first
if wise_qa_stat eq 1 then begin
    read_radprof,gal,'w2',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	mags[iw2] = nap_mag[0] - glga_getextin(ebmv,'w2',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		merrs[iw2] = nap_magerr[0] > 0.02 $
	else	merrs[iw2] = nap_magerr[0]	; preserve upper limits
	source[iw2] = 1
    endif
endif
;
; Check literature if needed
if mags[iw2] le 0. and galdat[g].w2_int_mag gt 0. then begin
	mags[iw2] = galdat[g].w2_int_mag + 3.339	; Vega -> AB
	merrs[iw2]= galdat[g].w2_int_magerr > 0.01
	source[iw2] = 2
endif
if mags[iw2] gt 0 and not keyword_set(silent) then $
	print,'WISE w2 :',wave[iw2],bwidth[iw2],source[iw2], $
		mags[iw2],'+-',merrs[iw2],form=opfmt
;
; WISE W3 mag
;
; check GLGA first
if wise_qa_stat eq 1 then begin
    read_radprof,gal,'w3',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	mags[iw3] = nap_mag[0] - glga_getextin(ebmv,'w3',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		merrs[iw3] = nap_magerr[0] > 0.02 $
	else	merrs[iw3] = nap_magerr[0]	; preserve upper limits
	source[iw3] = 1
    endif
endif
;
; Check literature if needed
if mags[iw3] le 0. and galdat[g].w3_int_mag gt 0. then begin
	mags[iw3] = galdat[g].w3_int_mag + 5.174	; Vega -> AB
	merrs[iw3] = galdat[g].w3_int_magerr > 0.01
	source[iw3] = 2
endif
if mags[iw3] gt 0 and not keyword_set(silent) then $
	print,'WISE w3 :',wave[iw3],bwidth[iw3],source[iw3], $
		mags[iw3],'+-',merrs[iw3],form=opfmt
;
; WISE W4 mag
;
; check GLGA first
if wise_qa_stat eq 1 then begin
    read_radprof,gal,'w4',nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath, /silent, $
			phot_ts = pts
    if pts gt phot_ts then phot_ts = pts
    if nt_a[0] gt 0. then begin
	mags[iw4] = nap_mag[0] - glga_getextin(ebmv,'w4',yuan13=yuan13)
	if nap_magerr[0] ge 0. then $
		merrs[iw4] = nap_magerr[0] > 0.02 $
	else	merrs[iw4] = nap_magerr[0]	; preserve upper limits
	source[iw4] = 1
    endif
endif
;
; Check literature if needed
if mags[iw4] le 0. and galdat[g].w4_int_mag gt 0. then begin
	mags[iw4] = galdat[g].w4_int_mag + 6.620	; Vega -> AB
	mags[iw4] = galdat[g].w4_int_magerr > 0.01
	source[iw4] = 2
endif
if mags[iw4] gt 0 and not keyword_set(silent) then $
	print,'WISE w4 :',wave[iw4],bwidth[iw4],source[iw4], $
		mags[iw4],'+-',merrs[iw4],form=opfmt
;
; IRAS 12-mu mag
if galdat[g].i12m_int_mag gt 0. then begin
	mags[i12m] = galdat[g].i12m_int_mag
	merrs[i12m]= galdat[g].i12m_int_magerr > 0.01
	source[i12m] = 2
	if not keyword_set(silent) then $
	    print,'IRAS 12-mu :',wave[i12m],bwidth[i12m],source[i12m], $
	    	mags[i12m],'+-',merrs[i12m],form=opfmt
endif
;
; IRAS 25-mu mag
if galdat[g].i25m_int_mag gt 0. then begin
	mags[i25m] = galdat[g].i25m_int_mag
	merrs[i25m]= galdat[g].i25m_int_magerr > 0.01
	source[i25m] = 2
	if not keyword_set(silent) then $
	    print,'IRAS 25-mu :',wave[i25m],bwidth[i25m],source[i25m], $
	    	mags[i25m],'+-',merrs[i25m],form=opfmt
endif
;
; IRAS 60-mu mag
if galdat[g].i60m_int_mag gt 0. then begin
	mags[i60m] = galdat[g].i60m_int_mag
	merrs[i60m]= galdat[g].i60m_int_magerr > 0.01
	source[i60m] = 2
	if not keyword_set(silent) then $
	    print,'IRAS 60-mu :',wave[i60m],bwidth[i60m],source[i60m], $
	   	 mags[i60m],'+-',merrs[i60m],form=opfmt
endif
;
; IRAS 100-mu mag
if galdat[g].i100m_int_mag gt 0. then begin
	mags[i100m] = galdat[g].i100m_int_mag
	merrs[i100m]= galdat[g].i100m_int_magerr > 0.01
	source[i100m] = 2
	if not keyword_set(silent) then $
	   print,'IRAS 100-mu :',wave[i100m],bwidth[i100m],source[i100m], $
	   	mags[i100m],'+-',merrs[i100m],form=opfmt
endif
;
return
end
