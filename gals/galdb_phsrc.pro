function galdb_phsrc,src,verbose=verbose
;+
; sndb_phsrc - return index of source for photometry
;-
COMMON galdb_info,galdat,gphsrc
;
if n_elements(gphsrc) then galdb_src_read
;
t=where(strcmp(gphsrc.source,src),nt)
if nt ne 1 then begin
	if keyword_set(verbose) then begin
		if nt gt 1 then $
			print,'GALDB_PHSRC: Error - source ambiguous: ',src
		if nt lt 1 then $
			print,'GALDB_PHSRC: Error - source not found: ',src
	endif
	return,0
endif
return,gphsrc[t[0]].srn
end
