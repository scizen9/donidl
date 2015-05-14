pro galdb_updt_snhosts,silent=silent,verbose=verbose
;+
;	update Supernova host data
;-
; common variable for galdat
COMMON galdb_info, galdat, gphsrc
COMMON sndb_info, sndat
;
; test for source file
if n_elements(gphsrc) le 0 then galdb_src_read
;
; new record
A = {galdb_data}
A = struct_init(A)
added = 0L
;
; read in prieto08 data
prdat = read_prieto08(silent=silent,verbose=verbose)
;
; read in nyu data
sdss = read_nyu(silent=silent,verbose=verbose)
;
; loop over sndat struct
nsn = n_elements(sndat)
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Checking this many SN host galaxies: ',nsn
for i=0L,nsn-1L do begin
	;
	; are we in galdat?
	h=gfind(sndat[i].host,count=nmhf,ra=sndat[i].hra,dec=sndat[i].hdec, $
		srad=10.,stat=stat,/silent)
	;
	; if not add a record
	if nmhf le 0 then begin
		galdat = [galdat,A]
		p = n_elements(galdat)-1L
		galdat[p].id		= get_hl_name(sndat[i].host)
		galdat[p].ra		= sndat[i].hra
		galdat[p].dec		= sndat[i].hdec
		galdat[p].majax		= sndat[i].hmajax
		galdat[p].minax		= sndat[i].hminax
		galdat[p].pa		= sndat[i].hpa
		galdat[p].inc		= sndat[i].hinc
		galdat[p].coo_src	= sndat[i].coo_src
		glactc,galdat[p].ra,galdat[p].dec,2000.,gall,galb,1,/degree
		galdat[p].mwebmv	= dust_getval(gall,galb)
		galdat[p].sample	= $
			gadd_sample(galdat[p].sample,'snhosts')
		galdat[p].sne		= $
			gadd_sample(galdat[p].sne,sndat[i].id)
		galdat[p].mod_time	= systime(1)
		;
		; update published data
		galdb_updt_pubdat,p,silent=silent,verbose=verbose
		;
		; update NYU value-added catalog DR6
		galdb_updt_nyu,p,silent=silent,verbose=verbose,sdss=sdss
		;
		; update Prieto08 abundance data
		galdb_updt_prieto08,p,silent=silent,verbose=verbose,prdat=prdat
		;
		; check coords for anonymous galaxies
		if (galdat[p].ra lt 0. or galdat[p].dec lt -90) and $
		   strmid(galdat[p].id,0,1) eq 'A' then begin
			galdat[p].ra	= sndat[i].ra
			galdat[p].dec	= sndat[i].dec
		endif
		;
		; check cz
		if galdat[p].cz lt -999. and sndat[i].cz gt -999. then begin
			galdat[p].cz	= sndat[i].cz
			z = galdat[p].cz / !phys_c
			lumd = sullivanlumdist(z,omega_l=!COSMO_OL, $
				omega_m=!COSMO_OM,h0=!COSMO_H0,/silent)
			galdat[p].linear_scale = $
				( (lumd / (1.+z)^2) / 206265.d0 ) * 1.d6
		endif
		;
		if keyword_set(verbose) then $
			print,'New record for: ',sndat[i].id,' ',galdat[p].id
		added = added + 1L
	endif else begin
		;
		; check cz
		if galdat[h].cz lt -999. and sndat[i].cz gt -999. then begin
			galdat[h].cz	= sndat[i].cz
			z = galdat[h].cz / !phys_c
			lumd = sullivanlumdist(z,omega_l=!COSMO_OL, $
				omega_m=!COSMO_OM,h0=!COSMO_H0,/silent)
			galdat[h].linear_scale = $
				( (lumd / (1.+z)^2) / 206265.d0 ) * 1.d6
		endif
		galdat[h].sne		= $
			gadd_sample(galdat[h].sne,sndat[i].id)
		galdat[h].sample	= $
			gadd_sample(galdat[h].sample,'snhosts')
		galdat[h].mod_time	= systime(1)
	endelse
endfor
;
print,' '
print,'Added this many galdat records: ',added
;
return
end
