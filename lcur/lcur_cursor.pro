pro lcur_cursor,done=done
;+
;	use cursor to get next star
;
;-
COMMON dao_imdata	; dao_im, dao_imhdr, dao_imgno

if dao_imgno lt 0 then return	; no image requested
;
s=size(dao_im)
nx=s(1)
ny=s(2)
;
; get box size
bx = 128
mag = 4
bx1 = bx - 1
hbx = bx / 2
hbx1 = hbx - 1
;
; get coords
cx = lcur_com_get('dao_cx')
cy = lcur_com_get('dao_cy')
;
dx = lcur_com_get('dao_x')
dy = lcur_com_get('dao_y')
id = lcur_com_get('dao_id')
;
; get x,y position in image
x=fix(cx-.5)
y=fix(cy-.5)
xs=fix(dx-.5)
ys=fix(dy-.5)
;
; get subim limits
x0=max([0,min([x-hbx1,nx-bx])])
y0=max([0,min([y-hbx1,ny-bx])])
;
; check offsets 
xoff = x0 - (x-hbx1)
yoff = y0 - (y-hbx1)
;
; get other stars
xs = (xs - x0) + xoff
ys = (ys - y0) + yoff
onsub = where(xs ge 0 and xs lt bx and ys ge 0 and ys lt bx, non)
if non gt 0 then begin
	xg = xs(onsub)*mag-1
	yg = ys(onsub)*mag-1
	idg = id(onsub)
	wset,2
	wait,1.
	cursor,a,b,/up
	if a lt 20 and b lt 20 then begin
		done = (1 eq 1)
		wset,0
	endif else begin
		r = sqrt( (a-xg)^2 + (b-yg)^2 )
		t=where(r eq min(r))
		newid = idg(t(0))
		wset,0
		lcur_dao_next,newid
	endelse
endif else begin
	print,'No other stars in field'
	done = (1 eq 1)
endelse
;
return
end
