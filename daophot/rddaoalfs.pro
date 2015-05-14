pro rddaoalfs, flist, indx, mags, merrs, chis, shrps
;
if n_params(0) lt 2 then begin
	print,'rddaoalfs, flist, indx, mags, merrs, chis, shrps'
	return
endif
;
nfiles = n_elements(flist)
s = size(indx)
nstars = s(2)

print,'Reading ',nstars, ' stars in ',nfiles,' files...'

mags	= fltarr(nfiles, nstars) + 99.999
merrs	= fltarr(nfiles, nstars) + 9.999
chis	= fltarr(nfiles, nstars) + 9.999
shrps	= fltarr(nfiles, nstars) + 9.999

for ifrm=0,nfiles-1 do begin

    idx = reform(indx(ifrm,*)) - 1

    imgs = fltarr(nstars)
    imes = fltarr(nstars)
    ichs = fltarr(nstars)
    ishp = fltarr(nstars)

    print,'File (',strn(ifrm+1),'/',strn(nfiles),'): ',flist(ifrm)

    openr,ilun,flist(ifrm),/get_lun
    rddaohdr,ilun
	
    m = 0.
    e = 0.
    c = 0.
    s = 0.

    star = 0L

    while not eof(ilun) do begin


    	readf,ilun,m,e,c,s, form = '(24x,f9.3,f9.3,18x,f9.2,f9.3)'

	imgs(star) = m
	imes(star) = e
	ichs(star) = c
	ishp(star) = s

	star = star + 1L

	if star mod 100L eq 0 then $
		print,string(13B),'stars read in: ',star, format='($,a1,a,i7)'

    endwhile
    free_lun,ilun
    print,string(13B),'Stars read in: ',star, format='($,a1,a,i7)'
    print,' '

    for star=0L,nstars-1 do begin

    	if idx(star) ge 0 then begin

	    mags(ifrm,star)	= imgs(idx(star))
	    merrs(ifrm,star)	= imes(idx(star))
	    chis(ifrm,star)	= ichs(idx(star))
	    shrps(ifrm,star)	= ishp(idx(star))

	endif

    endfor

endfor	; loop over files
;
return
end	; pro rddaoalfs
