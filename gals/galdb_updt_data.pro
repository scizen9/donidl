pro galdb_updt_data,silent=silent,verbose=verbose
;+
;	update ancillary data
;-
; common variable for galdat
COMMON galdb_info
;
; test for source file
if n_elements(gphsrc) le 0 then galdb_src_read
;
; are there any files with update data?
flist = file_search(!GALS_DATA+'/*.dat', count=nf)
;
if nf ge 1 then begin
	tnams	= tag_names(galdat)
	for i=0,nf-1 do begin
		fdecomp,flist[i],dsk,dir,typ,qual
		tag	= strupcase(gettok(typ,'-'))
		t	= where(strcmp(tnams,tag),nt)
		if nt eq 1 then begin
			form = 'a,d,d,'+typ
			readcol,flist[i],id,ra,dec,val,format=form
			for j=0,n_elements(val)-1L do begin
				g = gfind(id[j],count=ng,ra=ra[j],dec=dec[j], $
					srad=10.,/silent,status=stat)
;
; found a record
				if g ge 0 then begin
					galdat[g].(t[0]) = val[j]
;
; if not a name match, add alt record
					if stat ne 0 then $
					   galdat[g].altids = $
					       gadd_name(galdat[g].altids,id[j])
				endif else if not keyword_set(silent) then $
					print,'Not found: ',id[j]
			endfor
		endif
	endfor
endif else if not keyword_set(silent) then $
	print,'No ancillary data files found.'
;
return
end
