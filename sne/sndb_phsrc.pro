function sndb_phsrc,src,verbose=verbose
;+
; sndb_phsrc - return index of source for photometry
;-
COMMON lowz_sne_info,sndat,snhphsrc
;
if n_elements(snhphsrc) then sndb_src_read
;
t=where(strcmp(snhphsrc.source,src),nt)
if nt ne 1 then begin
	if keyword_set(verbose) then begin
		if nt gt 1 then $
			print,'SNDB_PHSRC: Error - source ambiguous: ',src
		if nt lt 1 then $
			print,'SNDB_PHSRC: Error - source not found: ',src
	endif
	return,0
endif
return,snhphsrc[t[0]].srn
end
