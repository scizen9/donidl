pro chkpsf,imf,ifile,ofile
;
; check inputs
if n_params(0) le 1 then begin
	print,'CHKPSF - Usage: chkpsf, imfile, ifile, ofile'
	return
endif
;
; get image
im = readfits(imf,h)
;
s=size(im)
nx=s(1)
ny=s(2)
;
; get box size
bx = 64
mag = 4
bx1 = bx - 1
hbx = bx / 2
hbx1 = hbx - 1
;
; open i/o files
openr,ilun,ifile,/get_lun
openw,olun,ofile,/get_lun
;
; read and write headers
rec = ''
for i=0,2 do begin
	readf,ilun,rec
	printf,olun,rec
endfor
;
; loop over coords
q = ''
while not eof(ilun) and q ne 'q' do begin
	readf,ilun,rec
	orec = rec
	print,rec
	id = gettok(rec,' ')
	xs = float(gettok(rec,' '))
	ys = float(gettok(rec,' '))
;
; create display window
window,0,xpos=30,ypos=650,xsize=(mag*bx),ysize=(mag*bx),title=strtrim(id,2)
;
; get x,y position in image
	x=fix(xs-1.5)
	y=fix(ys-1.5)
;
; get subim limits
	x0=max([0,min([x-hbx1,nx-bx])])
	x1=x0+bx1
	y0=max([0,min([y-hbx1,ny-bx])])
	y1=y0+bx1
;
; extract subim and get stats
	sim=im(x0:x1,y0:y1)
	ims,sim,mn,sg
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
	wset,0
	tvscl,rebin(dsim,bx*mag,bx*mag,/sample)>(mn-3*sg)<(mn+9*sg)
	tvcircle,bx*(mag/2)-1,bx*(mag/2)-1,50,bx*mag,bx*mag
;
; get user response
	read,'d - delete, q - quit, <cr> - keep: ',q
;
	if strtrim(q,2) eq '' then $
		printf,olun,orec
endwhile
;
free_lun,ilun,olun
;
return
end	; pchk
