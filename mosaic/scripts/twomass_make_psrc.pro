pro twomass_make_psrc,dfile
;+
; twomass_make_psrc - make point source list for each image
;-
; restore template
restore,!2MASS_DATA+'twomass_psc2mass_template.sav'
;
; read in data file
print,'Reading data file (this could take a while)...'
data = read_ascii(dfile,template=template_psc2mass)
;
; get numbers
npts = n_elements(data.cntr_u)
nuob = max(data.cntr_u)
print,'Read ',npts,' pointsources for this many objects: ',nuob, $
	format='(a,i9,a,i5)'
;
; input dir
iroot = !GLGA_2MASS_DATA+'data/sort/'
;
; loop over objects
for i=0L,nuob-1 do begin
    w=where(data.cntr_u eq i, nw)
    if nw ge 1 then begin

	obj = data.object_u[w[0]]
	print,i+1,'/',nuob,obj,form='(i5,a1,i5,2x,a-25)'

	im = mrdfits(iroot + obj + '/' + obj + '_j.fits',0,hdr,/silent)
	extast, hdr, astr
	sz = size(im,/dim)

	rtst= data.major_u[w[0]] / 6.0	; no objects within this radius

	ofile = !GLGA_ROOT + 'data/' + $
		string(floor(data.ra_u[w[0]]),format='(i03)')+'D/aux/' + $
		obj + '_2mass_pointsrc.dat'
	openw,ol,ofile,/get_lun
	printf,ol,'# 2MASS PSC catalog (a,d,x,y,r_asec,astr?): '+systime(0)
	for j=0,nw-1 do begin
	    p=w[j]
	    if data.j_snr[p] ge 3.0 and data.dist_x[p] gt rtst then begin
		ad2xy,data.ra[p],data.dec[p],astr,x,y
;
; use subim to speed things up
		x0=fix(x-63.)>0.
		x1=x0+127<(sz[0]-1)
		y0=fix(y-63.)>0.
		y1=y0+127<(sz[1]-1)
		mrad=get_mask_radius(im[x0:x1,y0:y1],x-float(x0),y-float(y0), $
				'2mass')
		printf,ol,data.ra[p],data.dec[p],x,y,mrad,1, $
			format='(2f13.8,3f9.3,i5)'
	        print,string(13B),j+1,'/',nw,x,y,mrad, $
			form='($,a1,i5,a1,i5,3f9.3)'
	    endif
	endfor
	free_lun,ol
	print,' '
    endif
endfor
;
return
end
