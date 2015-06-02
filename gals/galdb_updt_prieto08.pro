pro galdb_updt_prieto08,i,silent=silent,verbose=verbose,prdat=prdat
;+
;	update galdat struct from Prieto 2008
;-
; common variable for galdat
COMMON galdb_info, galdat, gphsrc
;
; test for source file
if n_elements(gphsrc) le 0 then galdb_src_read
;
; get SDSS integrated host data from NYU value-added catalog
if n_elements(prdat) le 0 then begin
	if not keyword_set(silent) or keyword_set(verbose) then $
		print,'Reading Prieto08 data'
	prdat = read_prieto08(verbose=verbose,silent=silent)
	if n_elements(prdat) le 1 then begin
		print,'Prieto08 data not found'
		return
	endif
endif
;
; we only get here if the prdat struct is here
gcirc,2,galdat[i].ra,galdat[i].dec,prdat.hra,prdat.hdec,dis
h=where(dis lt 25.,nh)
if nh gt 0 then begin
	;
	; get best entry
	dis2 = dis(h)
	pp= where(dis2 eq min(dis2))
	p = h[pp[0]]
	;
	; update Prieto08 data
	galdat[i].B_abs_mag	= prdat[p].babs
	galdat[i].O_abund 	= prdat[p].oab
	galdat[i].B_abs_src	= galdb_phsrc('Prieto08')
	galdat[i].O_abund_src	= galdb_phsrc('Prieto08')
	;
	; update velocity and distance scales
	if galdat[i].cz lt -900. then begin
		galdat[i].cz	= prdat[p].cz
		get_linscale,galdat[i].cz/!phys_c,sscl,mu
		if sscl gt 0. then begin
			galdat[i].linear_scale	= sscl
			galdat[i].mu		= mu
			galdat[i].ap_500pc	= (250./sscl)
			galdat[i].ap_1kpc	= (500./sscl)
			galdat[i].ap_2kpc	= (1000./sscl)
		endif
	endif
	;
	; host name
	if strcmp(galdat[i].id,prdat[p].host) ne 1 then begin
		;
		; replace anonymous designations
		if strmid(galdat[i].id,0,1) eq 'A' then begin
			ahost = galdat[i].id
			galdat[i].id = get_hl_name(prdat[p].host)
			if keyword_set(verbose) then $
				print,'Updated host name: ',galdat[i].id
		endif else ahost = get_hl_name(prdat[p].host)
		;
		; add to alternate ids
		galdat[i].altids = gadd_name(galdat[i].altids,ahost)
	endif
	;
	; time stamp
	galdat[i].mod_time = systime(1)
	galdat[i].sample = gadd_sample(galdat[i].sample,'prieto08')
endif 
;
return
end
