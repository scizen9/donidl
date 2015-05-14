pro gx_xplist,xfil
	xf = mrdfits(xfil,1,hdr)
	temp=xfil
	rute=gettok(temp,'.')
	ofil=rute+'.list'
	cln = xf[where((xf.flags and 4) ne 4,nc)]
	
	t = cln.t+315964800.d0
	x = cln.x/2147483647.d0
	y = cln.y/2147483647.d0
	z = cln.z/2147483647.d0
	dec = asin(z) / !dtor
	ra = atan(y,x) / !dtor
	m = where(ra lt 0., nm)
	if nm gt 0 then $
		ra[m] = ra[m] + 360.d0

	openw,ol,ofil,/get_lun
	printf,ol,'# GX_XPLIST: '+systime(0)
	printf,ol,'#RA, DEC, T(unix)'

	for i=0L,nc-1 do $
		printf,ol,ra[i],dec[i],t[i],format='(2f13.8,f19.3)'

	free_lun,ol

	return
end
