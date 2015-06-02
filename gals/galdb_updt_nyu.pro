pro galdb_updt_nyu,i,silent=silent,verbose=verbose,sdss=sdss
;+
;	update galdat structure from NYU value-added catalog
;-
; common variable for galdat
COMMON galdb_info, galdat, gphsrc
;
; test for source file
if n_elements(gphsrc) le 0 then galdb_src_read
;
; get SDSS integrated host data from NYU value-added catalog
if n_elements(sdss) le 1 then begin
	if not keyword_set(silent) or keyword_set(verbose) then $
		print,'Reading lowz_plus_ned.dr6.fits'
	sdss = read_nyu(silent=silent,verbose=verbose)
	if n_elements(sdss) le 1 then begin
		print,'lowz_plus_ned.dr6.fits not found'
		return
	endif
endif
;
; we only get here if the sdss struct is here
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
		shost = strcompress(sdss[p].ned_name1,/remove)+$
			strcompress(sdss[p].ned_name2,/remove)
		shost = get_hl_name(shost)
		;
		; is name already being used?
		if strcmp(galdat[i].id,shost) ne 1 then begin
		    ;
		    ; check if host name usable
		    if strpos(shost,'[') lt 0 and $
		       strpos(shost,'(') lt 0 then begin
			;
			; replace anonymous ids where possible
			if strmid(galdat[i].id,0,1) eq 'A' then begin
				ahost = galdat[i].id
				galdat[i].id = shost
				if keyword_set(verbose) then $
					print,'Updated host name: ',shost
			endif else ahost = shost
		    endif else ahost = shost
		    ;
		    ; update alternate ids
		    galdat[i].altids = gadd_name(galdat[i].altids,ahost)
		endif 
	endif
	;
	; time stamp
	galdat[i].mod_time = systime(1)
	galdat[i].sample = gadd_sample(galdat[i].sample,'nyu6')
endif 
;
return
end
