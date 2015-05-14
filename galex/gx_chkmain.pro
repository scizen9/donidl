pro gx_chkmain,imdirs,subv,odirs,osubv
;
; gx_chkmain - check for mains dir and data
;
; copy input directories
odirs=imdirs
osubv=subv
;
; get unique tile names
n=n_elements(imdirs)
tiles=strarr(n)
for i=0,n-1 do begin
	sta=strsplit(imdirs(i),'/',/extract)
	tiles(i) = sta(5)
endfor
tiles=tiles(sort(tiles))
tiles=tiles(uniq(tiles))
;
; loop over uniqe tiles and check mains dir
nt=n_elements(tiles)
for i=0,nt-1 do begin
    mdir='/home/galex/fltops/mains/01-vsn/'+tiles(i)+'/d/01-main/use/use'
    fi=file_info(mdir)
    if fi.exists then begin
	t=where(strpos(odirs,tiles(i)) ge 0, nf)
	odirs(t(0)) = mdir
	osubv(t(0)) = '00'
;
; set the rest to null string
	if nf gt 1 then begin
		t=t(1:*)
		odirs(t) = ''
	endif
    endif
endfor
;
t=where(strlen(odirs) gt 0, ng)
if ng gt 0 then begin
	odirs = odirs(t)
	osubv = osubv(t)
endif else print,'GX_CHKMAIN - ERROR: no valid tiles'
;
return
end
