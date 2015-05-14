pro galdb_create,silent=silent,verbose=verbose
;+
;	gather all the galaxy data into a master structure
;	start with the Hyper LEDA sample
;-
; common variable for galdat
COMMON galdb_info, galdat, gphsrc
;
; test for source file
if n_elements(gphsrc) le 0 then galdb_src_read
;
; Hyper LEDA struct
restore,!CAT_DATA+'hl_master_structs.sav'
;
; galdat
ngal=n_elements(hldata)
;
; define galaxy data structure: see galdb_data__define for definition
A = {galdb_data}
A = struct_init(A)
;
; galdat
galdat=replicate(A,ngal)
;
; ids
galdat.id = hldata.objname
;
; types
galdat.type = hldata.type
g = where(finite(hldata.t))
galdat[g].tyn = hldata[g].t
;
; coords (canonical is given for now)
galdat.ra = hldata.al2000*15.d0
galdat.dec = hldata.de2000
;
; shape params (canonical is given for now)
galdat.majax = 10.^(hldata.logd25) * 6.	; arcsec
galdat.minax = galdat.majax/10.^(hldata.logr25)
g = where(finite(hldata.pa))
galdat[g].pa = hldata[g].pa
galdat.inc = hldata.incl
;
; radial velocities
g = where(finite(hldata.v) and hldata.v gt -9999.9)
galdat[g].cz = hldata[g].v
;
; MW color excess
galdat.mwebmv = hldata.ag / 4.315
;
; loop over data struct
for i=0L,ngal-1L do begin
	;
	; print status
	if keyword_set(verbose) then $
		print,string(13B),i+1,'/',ngal,galdat[i].id, $
			format='($,a1,i7,a,i7,2x,a-32)'
	;
	; alternate names
	alts = hldata[i].altnames
	repeat begin
		repeat alt = gettok(alts,',') until strpos(alt,':[') lt 0
		galdat[i].altids = gadd_name(galdat[i].altids, alt)
	endrep until strlen(alts) le 0
	;
	; sample
	galdat[i].sample = gadd_sample(galdat[i].sample,'hleda')
	;
	; coo source
	galdat[i].coo_src = 'H'
	;
	; get linear scale at galaxy
	lumd = -9. & dmpc = -9.
	nhost=get_hl_name(galdat[i].id,/silent)
	get_ned1d,nhost,dmpc=dmpc,mu=mu,/silent
	if dmpc le 0. then begin
		get_ned,galdat[i].id,cosangd=dmpc,coslummu=mu,/silent
		if dmpc le 0. and finite(galdat[i].cz) eq 1 then begin
			z = galdat[i].cz / !phys_c
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
	; get aperture sizes in arcseconds
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
;
; extinction correct Johnson mags
	if umg gt 0. then $
		umg = umg - galdat[i].mwebmv * 5.434
	if bmg gt 0. then $
		bmg = bmg - galdat[i].mwebmv * 4.315
	if vmg gt 0. then $
		vmg = vmg - galdat[i].mwebmv * 3.315
;
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
endfor
print,' '
print,'Done.'
;
; get host B_T mags from Garnier et al. 1996
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Reading garnier96.dat'
if file_test(!CAT_DATA+'garnier96.dat') then begin
	readcol,!CAT_DATA+'garnier96.dat',mhost,mpa,mepa,mbt,mebt, $
		format='a,f,f,f,f',/silent
	nmh = n_elements(mhost)
	for i=0L,nmh-1L do begin
		;
		; print status
		if keyword_set(verbose) then $
			print,string(13B),i+1,'/',nmh,mhost[i], $
				format='($,a1,i7,a,i7,2x,a-32)'
		;
		; are we in our db?
		h=gfind(mhost[i],count=nmhf,/silent)
		if nmhf gt 0 then begin
			;
			; get B-band extinction
			if galdat[h[0]].mwebmv gt -1. then $
				extin = 4.315 * galdat[h[0]].mwebmv $
			else	extin = 0.0
			;
			; apply to host
			for j=0L,nmhf-1L do begin
			    ;
			    ; but only if not already there
			    if galdat[h[j]].bJ_int_mag lt 0. then begin
				galdat[h[j]].bJ_int_mag = mbt[i] - extin
				galdat[h[j]].bJ_int_magerr = mebt[i]
				galdat[h[j]].bJ_int_src = $
					galdb_phsrc('Garnier96')
				;
				; time stamp
				galdat[h[j]].mod_time = systime(1)
			    endif
			    galdat[h[j]].sample = $
				    gadd_sample(galdat[h[j]].sample,'garnier96')
			endfor
		endif
	endfor
endif else print,'garnier96.dat not found'
print,' '
print,'Done.'
;
; get SDSS integrated host data from NYU value-added catalog
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Reading lowz_plus_ned.dr6.fits'
if file_test(!SDSS_DATA+'lowz_plus_ned.dr6.fits') then begin
	sdss=mrdfits(!SDSS_DATA+'lowz_plus_ned.dr6.fits',1,slhdr,/silent)
	for i=0L,ngal-1L do begin
		;
		; print status
		if keyword_set(verbose) then $
			print,string(13B),i+1,'/',ngal,galdat[i].id, $
				format='($,a1,i7,a,i7,2x,a-32)'
		gcirc,2,galdat[i].ra,galdat[i].dec,sdss.ra,sdss.dec,dis
		h=where(dis lt 10. and sdss.lowz eq 1,nh)
		if nh gt 0 then begin
		    ;
		    ; get best entry
		    dis2 = dis(h)
		    pp= where(dis2 eq min(dis2))
		    p = h[pp[0]]
		    if finite(sdss[p].absmag[0]) then begin
		    	galdat[i].u_abs_mag = sdss[p].absmag[0]
		    	galdat[i].u_abs_magerr = sqrt(1./sdss[p].absmag_ivar[0])
			galdat[i].u_abs_src = galdb_phsrc('NYU6')
		    endif
		    if finite(sdss[p].absmag[1]) then begin
		    	galdat[i].g_abs_mag = sdss[p].absmag[1]
		    	galdat[i].g_abs_magerr = sqrt(1./sdss[p].absmag_ivar[1])
			galdat[i].g_abs_src = galdb_phsrc('NYU6')
		    endif
		    if finite(sdss[p].absmag[2]) then begin
		    	galdat[i].r_abs_mag = sdss[p].absmag[2]
		    	galdat[i].r_abs_magerr = sqrt(1./sdss[p].absmag_ivar[2])
			galdat[i].r_abs_src = galdb_phsrc('NYU6')
		    endif
		    if finite(sdss[p].absmag[3]) then begin
		    	galdat[i].i_abs_mag = sdss[p].absmag[3]
		    	galdat[i].i_abs_magerr = sqrt(1./sdss[p].absmag_ivar[3])
			galdat[i].i_abs_src = galdb_phsrc('NYU6')
		    endif
		    if finite(sdss[p].absmag[4]) then begin
		    	galdat[i].z_abs_mag = sdss[p].absmag[4]
		    	galdat[i].z_abs_magerr = sqrt(1./sdss[p].absmag_ivar[4])
			galdat[i].z_abs_src = galdb_phsrc('NYU6')
		    endif
		    if finite(sdss[p].absmag[5]) then begin
		    	galdat[i].J_abs_mag = sdss[p].absmag[5]
		    	galdat[i].J_abs_magerr = sqrt(1./sdss[p].absmag_ivar[5])
			galdat[i].J_abs_src = galdb_phsrc('NYU6')
		    endif
		    if finite(sdss[p].absmag[6]) then begin
		    	galdat[i].H_abs_mag = sdss[p].absmag[6]
		    	galdat[i].H_abs_magerr = sqrt(1./sdss[p].absmag_ivar[6])
			galdat[i].H_abs_src = galdb_phsrc('NYU6')
		    endif
		    if finite(sdss[p].absmag[7]) then begin
		    	galdat[i].K_abs_mag = sdss[p].absmag[7]
		    	galdat[i].K_abs_magerr = sqrt(1./sdss[p].absmag_ivar[7])
			galdat[i].K_abs_src = galdb_phsrc('NYU6')
		    endif
	galdat[i].u_int_mag	= 22.5 - 2.5 * alog10(sdss[p].modelflux[0]) - $
			sdss[p].extinction[0]
	galdat[i].u_int_magerr	= $
	1.0857362 * sqrt(1./sdss[p].modelflux_ivar[0]) / sdss[p].modelflux[0]
	galdat[i].u_int_src = galdb_phsrc('NYU6')
	galdat[i].g_int_mag	= 22.5 - 2.5 * alog10(sdss[p].modelflux[1]) - $
			sdss[p].extinction[1]
	galdat[i].g_int_magerr	= $
	1.0857362 * sqrt(1./sdss[p].modelflux_ivar[1]) / sdss[p].modelflux[1]
	galdat[i].g_int_src = galdb_phsrc('NYU6')
	galdat[i].r_int_mag	= 22.5 - 2.5 * alog10(sdss[p].modelflux[2]) - $
			sdss[p].extinction[2]
	galdat[i].r_int_magerr	= $
	1.0857362 * sqrt(1./sdss[p].modelflux_ivar[2]) / sdss[p].modelflux[2]
	galdat[i].r_int_src = galdb_phsrc('NYU6')
	galdat[i].i_int_mag	= 22.5 - 2.5 * alog10(sdss[p].modelflux[3]) - $
			sdss[p].extinction[3]
	galdat[i].i_int_magerr	= $
	1.0857362 * sqrt(1./sdss[p].modelflux_ivar[3]) / sdss[p].modelflux[3]
	galdat[i].i_int_src = galdb_phsrc('NYU6')
	galdat[i].z_int_mag	= 22.5 - 2.5 * alog10(sdss[p].modelflux[4]) - $
			sdss[p].extinction[4]
	galdat[i].z_int_magerr	= $
	1.0857362 * sqrt(1./sdss[p].modelflux_ivar[4]) / sdss[p].modelflux[4]
	galdat[i].z_int_src = galdb_phsrc('NYU6')
		;
		; host name
		if sdss[p].ned eq 1L then begin
			salt  = strcompress(sdss[p].ned_name1,/remove)+$
				strcompress(sdss[p].ned_name2,/remove)
			if strpos(salt,':[') lt 0 then $
			    galdat[i].altids = gadd_name(galdat[i].altids,salt)
		endif
			;
			; time stamp
			galdat[i].mod_time = systime(1)
			galdat[i].sample = gadd_sample(galdat[i].sample,'nyu6')
		endif 
	endfor
endif else print,'lowz_plus_ned.dr6.fits not found'
print,' '
print,'Done.'
;
; sort data on RA
galdat = galdat[sort(galdat.ra)]
;
; save file
savfile=!GALS_DATA+'/galdb_info.sav'
;
; mv old save file
filestamp,savfile,/verbose
;
; create save file
print,'Saving Galaxy info to: ',savfile
save,galdat,filename=savfile,/verbose
;
return
end
