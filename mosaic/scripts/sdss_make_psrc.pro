pro sdss_make_psrc,lfile
;+
; sdss_make_psrc - make point source list for each image
;
; lfile - galaxy list with one entry per galaxy with the following columns:
;       id
;       ra,dec  - decimal degrees
;       majdiam,mindiam - arcminutes
;       pa      - position angle in degrees
;-
; read input
readcol,lfile,id,ra,dec,majdiam,mindiam,pa,format='a,d,d,f,f,f'
;
; convert from arcmin to arcsec: 3 arcmin is min and 26 arcmin is max size
r=((majdiam*2.0)>3.<26.) * 60.
;
name = strtrim(id,2)
nuob = n_elements(ra)
;
; read catalog
readcol,'sdss_psrc.dat',sname,sra,sdec,form='a,d,d'
;
; get numbers
npts = n_elements(sra)
print,'Read ',npts,' pointsources for this many objects: ',nuob, $
	format='(a,i9,a,i5)'
;
; input dir
iroot = !GLGA_SDSS_DATA+'data/sort/'
;
; loop over objects
for i=0L,nuob-1 do begin
    gcirc,2,ra[i],dec[i],sra,sdec,dis
    w=where(dis lt r[i],nw)
    if nw ge 1 then begin

	obj = name[i]
	print,i+1,'/',nuob,obj,form='(i5,a1,i5,2x,a-25)'

	ffil = iroot + obj + '/' + obj + '_g.fits'
	if file_test(ffil) then begin
	    im = mrdfits(ffil,0,hdr,/silent)
	    sz = size(im,/dim)

	    extast, hdr, astr
	    wra = sra[w]
	    wdec= sdec[w]
	    wdis= dis[w]
	    ad2xy,wra,wdec,astr,x,y
	    flag = intarr(nw) + 1
	    for j=0,nw-1 do begin
		    gcirc,2,wra[j],wdec[j],wra,wdec,ds
		    w = where(ds le 1.0,nww)
		    if nww gt 1 then flag[w[1:*]] = 0
	    endfor

	    rtst= (majdiam[i]*60.) / 6.0 ; no objects within this radius (asec)

	    good= where(flag eq 1 and wdis gt rtst, ngood)

	    if ngood gt 0 then begin

		wra = wra[good]
		wdec= wdec[good]
		x   = x[good]
		y   = y[good]

	        ofile = !GLGA_ROOT + 'data/' + $
		    string(floor(ra[i]),format='(i03)')+'D/aux/' + $
		    obj + '_sdss_pointsrc.dat'
	        openw,ol,ofile,/get_lun
	        printf,ol,'# SDSS Star catalog (a,d,x,y,r_asec,astr?): '+ $
			systime(0)
	        for j=0,ngood-1 do begin
;
; use subim to speed things up
		    x0=fix(x[j]-63.)>0.
		    x1=x0+127<(sz[0]-1)
		    y0=fix(y[j]-63.)>0.
		    y1=y0+127<(sz[1]-1)
		    mrad=get_mask_radius(im[x0:x1,y0:y1], $
			    x[j]-float(x0),y[j]-float(y0),'sdss')
		    printf,ol,wra[j],wdec[j],x[j],y[j],mrad,1, $
			    format='(2f13.8,3f9.3,i5)'
	            print,string(13B),j+1,'/',ngood,x[j],y[j],mrad, $
			    form='($,a1,i5,a1,i5,3f9.3)'
	        endfor	; loop over match objects
	        free_lun,ol
	        print,' '
	    endif	; ngood gt 0
	endif	; fits file exists
    endif
endfor
;
return
end
