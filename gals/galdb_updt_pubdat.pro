pro galdb_updt_pubdat,i,silent=silent,verbose=verbose,sdss=sdss
;+
;	update published data from NED
;-
; common variable for galdat
COMMON galdb_info, galdat, gphsrc
;
; test for source file
if n_elements(gphsrc) le 0 then galdb_src_read
;
; get galaxy id
nhost=galdat[i].id
get_ned_coord,nhost,eq_ra_j=nra,eq_dec_j=ndec,/silent
;
; if this name is not in NED
if nra lt -90 and ndec lt -90 then begin
	;
	; check coords
	nhost=search_ned(galdat[i].ra,galdat[i].dec, $
		galdat[i].majax/120.,altid=altid, $	; radius in arcmin
		silent=silent) 
	nhost = get_hl_name(nhost)
	;
	; did we get an unusable ned name?
	if strlen(altid) ne 0 then $
		galdat[i].altids = gadd_name(galdat[i].altids,altid)
	if strlen(nhost) eq 0 then begin
		if keyword_set(verbose) then $
			print,'Host not found in NED: ',galdat[i].id
		return
	endif
	;
	; re-acquire NED coords
	get_ned_coord,nhost,eq_ra_j=nra,eq_dec_j=ndec,/silent
	;
	; update galaxy ids, use NED canonical
	ahost = galdat[i].id
	galdat[i].id = nhost
	galdat[i].altids = gadd_name(galdat[i].altids,ahost)
endif
;
; we only get here if we are in NED
galdat[i].ned_gal = 1
;
; update coords
if nra ge 0 and ndec ge -90 then begin
	galdat[i].ra = nra
	galdat[i].dec= ndec
	galdat[i].coo_src = 'N'
endif
;
; basic data
get_ned,nhost,rvhel=rv,majdim=majax,mindim=minax,class=type,pgc=pgc,/silent
if rv lt -900. or majax lt 0. or minax lt 0. or strlen(type) le 0 then $
	get_ned,nhost,rvhel=rv,majdim=majax,mindim=minax,class=type, $
		/silent, /reread
galdat[i].majax = majax * 60.
galdat[i].minax = minax * 60.
galdat[i].type = type
galdat[i].cz = rv
if galdat[i].pgc le 0 then galdat[i].pgc = pgc
;
; get linear scale at galaxy
lumd = -9. & dmpc = -9.
get_ned1d,nhost,dmpc=dmpc,mu=mu,/silent
if dmpc le 0. then begin
	get_ned,nhost,cosangd=dmpc,coslummu=mu,/silent
	if dmpc le 0. and finite(rv) eq 1 then begin
		z = rv / !phys_c
		lumd = sullivanlumdist(z,omega_l=!COSMO_OL, $
			omega_m=!COSMO_OM,h0=!COSMO_H0, $
						   /silent)
		dmpc = lumd / (1.+z)^2	; convert to angular size dist.
	endif
endif
if dmpc gt 0. then begin
	sscl = ( dmpc / 206265.d0 ) * 1.d6      ; pc / arcsec
	galdat[i].linear_scale	= sscl
endif else sscl = -9.
;
	; get distance modulus
if mu le 0. and lumd gt 0. then $
		mu = -5. + 5. * alog10( lumd * 10.^6 )
;
; get aperture radii in arcseconds
galdat[i].ap_res		= 5.6*0.5	; NUV resolution
if sscl gt 0. then begin
    galdat[i].ap_500pc	= (250./sscl)	; 500pc diam. ap.
    galdat[i].ap_1kpc	= (500./sscl)	; 1kpc  diam. ap.
    galdat[i].ap_2kpc	= (1000./sscl)	; 2kpc  diam. ap.
endif else begin
    galdat[i].ap_500pc	= 0.
    galdat[i].ap_1kpc	= 0.
    galdat[i].ap_2kpc	= 0.
endelse
;
; get host integrated magnitudes
;
; GALEX NGA mags
get_nga_phot,nhost,fuv=fmg,errfuv=fmge,nuv=nmg,errnuv=nmge,/silent
if fmg gt 0. then $
	srcfmg='NGA' $
else	srcfmg=''
if nmg gt 0. then $
	srcnmg='NGA' $
else	srcnmg=''
;
; NED mags
get_ned_phot,nhost, $
    su_t=sumg,errsu_t=sumge,srcsu_t=srcsumg, $
    sg_t=sgmg,errsg_t=sgmge,srcsg_t=srcsgmg, $
    sr_t=srmg,errsr_t=srmge,srcsr_t=srcsrmg, $
    si_t=simg,errsi_t=simge,srcsi_t=srcsimg, $
    sz_t=szmg,errsz_t=szmge,srcsz_t=srcszmg, $
    u_t=umg,erru_t=umge,srcu_t=srcumg, $
    b_t=bmg,errb_t=bmge,srcb_t=srcbmg, $
    v_t=vmg,errv_t=vmge,srcv_t=srcvmg, $
    j_t=jmg,errj_t=jmge,srcj_t=srcjmg, $
    h_t=hmg,errh_t=hmge,srch_t=srchmg, $
    k_t=kmg,errk_t=kmge,srck_t=srckmg, $
    iras_12m=i12mg,erriras_12m=i12mge,srciras_12m=src12mg, $
    iras_25m=i25mg,erriras_25m=i25mge,srciras_25m=src25mg, $
    iras_60m=i60mg,erriras_60m=i60mge,srciras_60m=src60mg, $
    iras_100m=i100mg,erriras_100m=i100mge,srciras_100m=src100mg, $
    ned_gal=ned_gal, /silent
;
; extinction correct SDSS mags
if sumg gt 0. then $
	sumg = sumg - galdat[i].mwebmv * 5.155
if sgmg gt 0. then $
	sgmg = sgmg - galdat[i].mwebmv * 3.793
if srmg gt 0. then $
	srmg = srmg - galdat[i].mwebmv * 2.751
if simg gt 0. then $
	simg = simg - galdat[i].mwebmv * 2.086
if szmg gt 0. then $
	szmg = szmg - galdat[i].mwebmv * 1.479

; extinction correct Johnson mags
if umg gt 0. then $
	umg = umg - galdat[i].mwebmv * 5.434
if bmg gt 0. then $
	bmg = bmg - galdat[i].mwebmv * 4.315
if vmg gt 0. then $
	vmg = vmg - galdat[i].mwebmv * 3.315

; try NED for GALEX mags if not in NGA
if fmg lt 0 then begin
	get_ned_phot,nhost,fuv=fmg,errfuv=fmge,/silent
	if fmg gt 0 then srcfmg = 'NED'
endif
if nmg lt 0 then begin
	get_ned_phot,nhost,nuv=nmg,errnuv=nmge,/silent
	if nmg gt 0 then srcnmg = 'NED'
endif
;
; try 2MASS XSC mags if not in LGA
if hmg lt 0. then begin
	get_2mass_phot,nhost,h_t=hmg,errh_t=hmge,/silent
	if hmg gt 0. then srchmg = 'XSC'
endif
if jmg lt 0. then begin
	get_2mass_phot,nhost,j_t=jmg,errj_t=jmge,/silent
	if jmg gt 0. then srcjmg = 'XSC'
endif
if kmg lt 0. then begin
	get_2mass_phot,nhost,k_t=kmg,errk_t=kmge,/silent
	if kmg gt 0. then srckmg = 'XSC'
endif
;
; extinction correct 2MASS mags
if jmg gt 0. then $
	jmg = jmg - galdat[i].mwebmv * 0.900
if hmg gt 0. then $
	hmg = hmg - galdat[i].mwebmv * 0.576
if kmg gt 0. then $
	kmg = kmg - galdat[i].mwebmv * 0.365
;
galdat[i].fuv_int_mag	= fmg
galdat[i].fuv_int_magerr= fmge
galdat[i].fuv_int_src	= galdb_phsrc(srcfmg)
galdat[i].nuv_int_mag	= nmg
galdat[i].nuv_int_magerr= nmge
galdat[i].nuv_int_src	= galdb_phsrc(srcnmg)
galdat[i].u_int_mag	= sumg
galdat[i].u_int_magerr	= sumge
galdat[i].u_int_src	= galdb_phsrc(srcsumg)
galdat[i].g_int_mag	= sgmg
galdat[i].g_int_magerr	= sgmge
galdat[i].g_int_src	= galdb_phsrc(srcsgmg)
galdat[i].r_int_mag	= srmg
galdat[i].r_int_magerr	= srmge
galdat[i].r_int_src	= galdb_phsrc(srcsrmg)
galdat[i].i_int_mag	= simg
galdat[i].i_int_magerr	= simge
galdat[i].i_int_src	= galdb_phsrc(srcsimg)
galdat[i].z_int_mag	= szmg
galdat[i].z_int_magerr	= szmge
galdat[i].z_int_src	= galdb_phsrc(srcszmg)
galdat[i].uJ_int_mag	= umg
galdat[i].uJ_int_magerr	= umge
galdat[i].uJ_int_src	= galdb_phsrc(srcumg)
galdat[i].bJ_int_mag	= bmg
galdat[i].bJ_int_magerr	= bmge
galdat[i].bJ_int_src	= galdb_phsrc(srcbmg)
galdat[i].vJ_int_mag	= vmg
galdat[i].vJ_int_magerr	= vmge
galdat[i].vJ_int_src	= galdb_phsrc(srcvmg)
galdat[i].H_int_mag	= hmg
galdat[i].H_int_magerr	= hmge
galdat[i].H_int_src	= galdb_phsrc(srchmg)
galdat[i].J_int_mag	= jmg
galdat[i].J_int_magerr	= jmge
galdat[i].J_int_src	= galdb_phsrc(srcjmg)
galdat[i].K_int_mag	= kmg
galdat[i].K_int_magerr	= kmge
galdat[i].K_int_src	= galdb_phsrc(srckmg)
galdat[i].i12m_int_mag	= i12mg
galdat[i].i12m_int_magerr= i12mge
galdat[i].i12m_int_src	= galdb_phsrc(src12mg)
galdat[i].i25m_int_mag	= i25mg
galdat[i].i25m_int_magerr= i25mge
galdat[i].i25m_int_src	= galdb_phsrc(src25mg)
galdat[i].i60m_int_mag	= i60mg
galdat[i].i60m_int_magerr= i60mge
galdat[i].i60m_int_src	= galdb_phsrc(src60mg)
galdat[i].i100m_int_mag	= i100mg
galdat[i].i100m_int_magerr=i100mge
galdat[i].i100m_int_src	= galdb_phsrc(src100mg)
galdat[i].mu		= mu
galdat[i].ned_gal	= ned_gal
;
; time stamp
galdat[i].mod_time = systime(1)
;
return
end
