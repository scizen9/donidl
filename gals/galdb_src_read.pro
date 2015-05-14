pro galdb_src_read
;+
; galdb_src_read - read list of photometry sources
;-
COMMON galdb_info,galdat,gphsrc
;
ifil=!GALS_DATA + 'sources.list'
readcol,ifil,snum,src,format='i,a',delim=',',/silent
nsrc = max(snum+1)
;
A = {galdb_src}
gphsrc=replicate(A,nsrc)
;
; loop over list
for i=0,n_elements(snum)-1 do begin
	gphsrc[snum[i]].srn = snum[i]
	gphsrc[snum[i]].source = src[i]
endfor
;
return
end
