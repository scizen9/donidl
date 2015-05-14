common toc,f,r,d

pro getsrc,ra,dec,dmax,src0,ierror,exptime,init,ff,dst0

common toc
ierror = 0
;print,'getsrc:',ra,dec,dmax
if init eq 1 then readcol,'/home/ull9/cmartin/galex/status/toc.dat',f,r,d,format='a,f,f',/silent
j = 0
d2r = !PI/180.
n = n_elements(f)
for i=0,n-1 do begin
gcirc,1,ra/15.d,dec,r[i]/15.d,d[i],dis
dst0 = 9999.
;print,ra,dec,r[i],d[i],dis
if dis lt 0.6*3600. then begin
print,ra,dec,r[i],d[i],dis
	ff = f[i]
	print,ff
	if strpos(f[i],'cat') ge 0 then begin
		ff = f[i]
		strput,ff,'cnt',strpos(f[i],'cat')
	endif
	ff = ff + ' '
	strput,ff,'xd-mcat.fits',strpos(ff,'fd-cnt')
	print,ff

	s = mrdfits(ff,1,fhdr,status=stat)
	if stat ge 0 then begin
	extast,fhdr,fastr,noparams
	fra = sxpar(fhdr,'RA_CENT')
	fdec = sxpar(fhdr,'DEC_CENT')
	exptime = sxpar(fhdr,'EXPTIME')

	ns = n_elements(s.alpha_j2000)
	src = s
	for is=0,ns-1 do begin
		rs = s[is].alpha_j2000
		ds = s[is].delta_j2000
		gcirc,1,ra/15.d,dec,rs/15.d,ds,dst
		if dst lt dst0 then dst0 = dst
		if dst le dmax then begin
		src[j] = s[is]
		j = j+1
		print,format='("SOURCE:",i10,2f15.6,3f6.2,f10.1)',j,rs,ds,dst,s[is].nuv_mag,s[is].fuv_mag,exptime
		endif
	endfor
	if j ge 1 then src0 = src[0:j-1]
	if j eq 0 then ierror = 1 
	return
	endif
	ierror = 3
	return
endif

endfor
ierror = 2

return
end


