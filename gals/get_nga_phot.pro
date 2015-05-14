pro get_nga_phot,obj,fuv=fuv,errfuv=errfuv,nuv=nuv,errnuv=errnuv, $
	reread=reread,silent=silent
;
; see if it's in archive
ngdir=!NGA_DATA
fspec = ngdir+'/'+obj+'_phot.dat'
flist=file_search(fspec,count=nf)
;
; if not, get NGA data from uv_mags.dat
if nf lt 1 or keyword_set(reread) then begin
    readcol,ngdir+'uv_mags.dat',gal,rflast,rnlast,flast,flaste,nlast,nlaste, $
	    fasym,fasyme,nasym,nasyme,format='a,f,f,f,f,f,f',silent=silent
    t=where(strcmp(gal,strtrim(obj,2)) eq 1, n)
    if n eq 1 then begin
	    t=t(0)
	    fuv = fasym(t)
	    errfuv = fasyme(t)
	    nuv = nasym(t)
	    errnuv = nasyme(t)
	    if fuv ge 90. then begin
		    fuv = -99.99
		    errfuv = -9.99
	    endif
	    if nuv ge 90. then begin
		    nuv = -99.99
		    errnuv = -9.99
	    endif
	    openw,ol,fspec,/get_lun
	    printf,ol,fuv,errfuv,nuv,errnuv
	    free_lun,ol
	    flist=file_search(fspec,count=nf)
    endif else begin
	    fuv = -99.99
	    errfuv = -9.99
	    nuv = -99.99
	    errnuv = -9.99
    endelse
endif
;
; get file
if nf eq 1 then begin
;
; read in file
	openr,il,flist(0),/get_lun
	fuv = -99.99
	errfuv = -9.99
	nuv = -99.99
	errnuv = -9.99
	readf,il,fuv,errfuv,nuv,errnuv
	free_lun,il
endif
;
return
end
