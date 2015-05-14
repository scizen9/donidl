pro lcur_dao_zoom,chip,cid,refpa,refmag,qual,coords,note,field, $
	finder1=finder1,finder2=finder2
;+
;	zoom on star of interest
;
;-
COMMON dao_imdata	; dao_im, dao_imhdr, dao_imgno

if dao_imgno lt 0 then return	; no image requested
;
s=size(dao_im)
nx=s(1)
ny=s(2)
;
do_hardcopy = (keyword_set(finder1) or keyword_set(finder2))
;
; get box size
if keyword_set(finder1) then begin
	bx=800
	mag=1
endif else begin
	bx = 128
	mag = 4
endelse
bx1 = bx - 1
hbx = bx / 2
hbx1 = hbx - 1
;
; get coords
cx = lcur_com_get('dao_cx')
cy = lcur_com_get('dao_cy')
id = lcur_com_get('dao_cid')

dx = lcur_com_get('dao_x')
dy = lcur_com_get('dao_y')
cir = lcur_com_get('dao_neith')
if cir le 0. then cir = 10.0
;
; print coords
;print,'x,y: ',cx,cy
;
; create display window
;window,2,xpos=30,ypos=650,xsize=(mag*bx),ysize=(mag*bx),title=strtrim(id,2)
;window,3,xpos=300,ypos=650,xsize=512,ysize=512,title=strtrim(id,2)
;window,4,xpos=78,ypos=950,xsize=160,ysize=160,title=strtrim(id,2)
;
; get x,y position in image
	x=fix(cx-.5)
	y=fix(cy-.5)
	xs=fix(dx-.5)
	ys=fix(dy-.5)
;
; get subim limits
	x0=max([0,min([x-hbx1,nx-bx])])
	x1=x0+bx1
	y0=max([0,min([y-hbx1,ny-bx])])
	y1=y0+bx1
;
; extract subim and get stats
	sim=dao_im(x0:x1,y0:y1)
	good = where(sim gt 0, ngood)
	if ngood gt 0 then $
		ims,sim[good],mn,sg $
	else	begin
		mn = 0.
		sg = 0.
	endelse
;
; check offsets so we center star in window
	xoff = x0 - (x-hbx1)
	yoff = y0 - (y-hbx1)
	if xoff ge 0 then begin
		xd0 = xoff
		xd1 = bx1
		xs0 = 0
		xs1 = bx1 - xoff
	endif else begin
		xd0 = 0
		xd1 = bx1 + xoff
		xs0 = -xoff
		xs1 = bx1
	endelse
	if yoff ge 0 then begin
		yd0 = yoff
		yd1 = bx1
		ys0 = 0
		ys1 = bx1 - yoff
	endif else begin
		yd0 = 0
		yd1 = bx1 + yoff
		ys0 = -yoff
		ys1 = bx1
	endelse
;
; create display sub im making sure we center star in subimage
	dsim = replicate(0.0,bx,bx)
	dsim(xd0:xd1,yd0:yd1) = sim(xs0:xs1,ys0:ys1)
;
; display subim
	if keyword_set(finder1) then begin
		if chip le 3 then $
			dsim = transpose(dsim) $
		else	dsim = rotate(dsim,1)
		dsim(0,0) = mn-3*sg
		dsim(1,0) = mn+9*sg
		tvscl,dsim>(mn-3*sg)<(mn+9*sg)
		rr = 50.
		pscircle, 399., 399., rr, 800., 800., color=120
		xscl = 1000.0 * 17.78 / 24.13
		yscl = 1000.0
		xx1 = (399. + rr * sin(refpa*!DPI/180.d0))*xscl
		yy1 = (399. - rr * cos(refpa*!DPI/180.d0))*yscl
		xx2 = (399. + rr * sin((refpa+180.)*!DPI/180.d0))*xscl
		yy2 = (399. - rr * cos((refpa+180.)*!DPI/180.d0))*yscl
		oplot,[xx1,xx2],[yy1,yy2],color=120
		xyouts,600000,750000,'ID = '+string(cid,form='(i6)'),$
			charsize=2,color=0
		xyouts,600000,700000,'MAG= '+string(refmag,form='(f5.1)'),$
			charsize=2,color=0
		xyouts,600000,650000,'PA = '+string(refpa,form='(f5.1)'),$
			charsize=2,color=0
		xyouts,600000,600000,'FOV = 144"',charsize=2,color=0
		;
		xyouts,600000,400000,'QUAL= '+string(qual,form='(i3)'), $
			charsize=2,color=0
		xyouts,600000,350000,'NOTE= '+note,charsize=1,color=0
		;
		xyouts,600000,170000,field,charsize=2,color=0
		rastr = gettok(coords,' ')
		xyouts,600000,120000,'RA,DEC (J2000)',charsize=2,color=0
		xyouts,600000,70000,rastr, charsize=2,color=0
		xyouts,600000,20000,strtrim(coords,2), charsize=2,color=0
	endif else begin
		wset,2
		tvscl,rebin(dsim,bx*mag,bx*mag,/sample)>(mn-3*sg)<(mn+9*sg)
		mycircle,bx*(mag/2)-mag,bx*(mag/2)-mag,cir*mag,bx*mag,bx*mag
	endelse
;
; get other stars
	if not keyword_set(finder1) then begin
		xs = (xs - x0) + xoff
		ys = (ys - y0) + yoff
		onsub = where(xs ge 0 and xs lt bx and $
		    ys ge 0 and ys lt bx, non)
		if non gt 0 then $
		    for i=0,non-1 do $
			mycircle,xs(onsub(i))*mag-1, ys(onsub(i))*mag-1,1, $
			    bx*mag, bx*mag
	endif
;
; do a 2d gaussian fit
;	fim = dsim(hbx1-9:hbx1+10,hbx1-9:hbx1+10)
;	fit = gauss2dfit(fim,a,/tilt)
;	res = fim - fit
;
; display gaussian residuals
;	wset,4
;	tvscl,rebin(res,160,160,/sample)
;
; print gaussian fit results
;	print,'FWHM - x: ',string(a(2),form='$(f4.2)'), $
;		   '  y: ',string(a(3),form='$(f4.2)'), $
;		  '  dl: ',string(a(2)-a(3),form='$(f5.2)')
;	print,'Theta: (deg)',a(6) * 180.0 / !pi
;	print,' '
;
; display radial plot of star profile
;	wset,3
;	cntrd,dsim,hbx1,hbx1,xc,yc,10,/silent
;	dist_circle,r,bx,xc,yc
;	plot,r,dsim,psym=1,xran=[-0.1,6],xsty=1,xtitle='R(pix)',ytitle='DN', $
;		title='Radial Profile', charsize=2,$
;		xthick=2,ythick=2,charthick=2
;
if not do_hardcopy then wset,0
return
end	; lcur_dao_zoom
