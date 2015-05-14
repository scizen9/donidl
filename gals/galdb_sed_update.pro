pro galdb_sed_update,lfil=lfil,verbose=verbose,testmags=testmags
;+
;	update the GLGA apphot mags
;-
; common variable for galdat
COMMON galdb_info, galdat
;
; use a file, or check them all?
if keyword_set(lfil) then begin
	if not file_exist(lfil) then begin
		print,'Error - file not found: ',lfil
		return
	endif
	ind = get_galist(lfil,nsam=ngal,/silent)
endif else begin
	ngal = n_elements(galdat)
	ind = lindgen(ngal)
endelse
;
; are we testing mags?
if keyword_set(testmags) then begin
	tnames = tag_names(galdat)
	tt = where(strpos(tnames,'_INT_MAG') ge 0 and $
		      strpos(tnames,'ERR') lt 0,nt)
endif
;
; open output files
sfil = 'update_seds.list'
filestamp,sfil,/arch
openw,sl,sfil,/get_lun
ffil = 'update_lfits.list'
filestamp,ffil,/arch
openw,fl,ffil,/get_lun
;
; loop over records
for k=0L,ngal-1L do begin
	i = ind[k]
	;
	; get test results
	if keyword_set(testmags) then begin
		gmags = 0
		for j=0,nt-1 do $
			if galdat[i].(tt[j]) gt 0. then gmags = gmags + 1
	endif else	gmags = 100
	;
	; print status
	if keyword_set(verbose) then $
		print,string(13B),i+1,'/',ngal,galdat[i].id, $
			format='($,a1,i7,a,i7,2x,a-32)'
	;
	; get sed file list
	if gmags gt 0 then begin
		ddir = !GLGA_ROOT + 'data/'+glga_degdir(galdat[i].ra)+'/plots/'
		flist = file_search(ddir+galdat[i].id+'_hSED.gif',count=nf)
	;
	; update if missing
		if nf le 0 then begin
			printf,sl,galdat[i].id
		endif else begin
			finfo = file_info(flist[0])
			if galdat[i].mod_time gt finfo.mtime then $
				printf,sl,galdat[i].id
		endelse
	endif
	;
	; get sed fit file list
	if gmags ge 5 then begin
		ddir = !GLGA_ROOT + 'data/'+glga_degdir(galdat[i].ra)+'/plots/'
		flist = file_search(ddir+galdat[i].id+'_hSEDfit.gif',count=nf)
		;
		; update if missing
		if nf le 0 then begin
			printf,fl,galdat[i].id
		endif else begin
			finfo = file_info(flist[0])
			if galdat[i].mod_time gt finfo.mtime then $
				printf,fl,galdat[i].id
		endelse
	endif
endfor	; end loop over db records
free_lun,sl,fl
print,' '
;
return
end
