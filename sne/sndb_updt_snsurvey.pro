pro sndb_updt_snsurvey,nsne,verbose=verbose,silent=silent
;+
;	update records for non-IAU SNe from Surveys (PTF, SNLS, etc.)
;-
; common variable for sndat
COMMON sndb_info, sndat
;
; define sn structure: see sndb_data__define for definition
A = {sndb_data}
A = struct_init(A)
;
; sndat
nsne=n_elements(sndat)
onsne=nsne
;
; read SN Survey catalog
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Reading list of other survey SNe'
if file_test(!SNE_DATA+'/sne_survey.dat') then begin
	readcol,!SNE_DATA+'/sne_survey.dat', $
		ssn,srv,stype,isn,sra,sdec,shst,hra,hdec,sz, $
		format='a,a,a,a,d,d,a,d,d,f',/silent
	nsrv = n_elements(ssn)
	added = 0L	; new ones?
	for i=0L,nsrv-1L do begin
		;
		; status
		if not keyword_set(silent) or keyword_set(verbose) then $
			print,string(13B),ssn[i],i+1,'/',nsrv,nsne, $
				format='($,a1,a-15,2x,i5,a1,i5,2x,i5)'
		;
		; are we already in the db?
		t=snfind(isn[i],nt,/silent)		; check canonical name
		if nt le 0 then $			; then check survey id
			t=snfind(ssn[i],nt,/silent)
		if nt eq 1 then begin	; does survey sn already have a record?
			sndat[t].srv_id = ssn[i]
			sndat[t].srv_name = srv[i]
			sndat[t].ra = sra[i]		; update coords
			sndat[t].dec = sdec[i]
			sndat[t].coo_src = 'V'
			if sz[i] gt 0. then $		; update cz if good
				sndat[t].cz	= sz[i] * !phys_c
			tyn = get_sntyn(stype[i])
			if tyn ge 0 then begin		; update type if good
				sndat[t].tyn	= tyn
				sndat[t].type	= string(stype[i],form='(a-8)')
			endif
			;
			; time stamp
			sndat[t].mod_time = systime(1)
		endif else begin	; not there so add a record
			sndat = [sndat,A]
			p = nsne
			nsne = nsne + 1L
			added = added + 1L
			sndat[p].id		= ssn[i]
			sndat[p].srv_id		= ssn[i]
			sndat[p].srv_name	= srv[i]
			sndat[p].type		= string(stype[i],form='(a-8)')
			sndat[p].tyn		= get_sntyn(stype[i])
			sndat[p].ra 		= sra[i]
			sndat[p].dec		= sdec[i]
			sndat[p].coo_src	= 'V'
			glactc,sra[i],sdec[i],2000.,gall,galb,1,/degree
			sndat[p].mwebmv		= dust_getval(gall,galb)
			if sz[i] gt 0. then begin	; test if z is good
				sndat[p].cz	= sz[i] * !phys_c
			endif
			;
			; host data
			sndat[p].host		= get_hl_name(shst[i])
			sndat[p].hra		= hra[i]
			sndat[p].hdec		= hdec[i]
			sndat[p].hcoo_src	= 'V'
			;
			; time stamp
			sndat[p].mod_time = systime(1)
		endelse
	endfor
endif else print,'sne_survey.dat not found'
print,' '
print,'New SNe records added: ',added
;
return
end
