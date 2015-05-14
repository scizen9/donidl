function snhfind,host,n,items=items,silent=silent
;
common sndb_info
;
hind=-1
ind=where(strcmp(strtrim(sndat.host,2),host) eq 1, n)
if n le 0 then begin
	ind=where(strcmp(strtrim(sndat.host,2),get_hl_name(host)) eq 1, n)
	if n le 0 and not keyword_set(silent) then $
		print,host,' not found.'
endif
;
; print items
if n gt 0 then $
	snprint,ind[0],items=items
;
return,ind
end
