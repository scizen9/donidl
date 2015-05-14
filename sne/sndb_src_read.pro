pro sndb_src_read
;+
; sndb_src_read - read list of photometry sources
;-
COMMON lowz_sne_info,sndat,snhphsrc
;
ifil=!SNE_DATA + 'sndb/sources.list'
readcol,ifil,snum,src,format='i,a',delim=',',/silent
nsrc = max(snum+1)
;
A = {sndb_src}
snhphsrc=replicate(A,nsrc)
;
; loop over list
for i=0,n_elements(snum)-1 do begin
	snhphsrc[snum[i]].srn = snum[i]
	snhphsrc[snum[i]].source = src[i]
endfor
;
return
end
