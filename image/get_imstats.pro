pro get_imstats,im,imfile,fwhm,back,nf
;+
;	get image statistics
;
;-
; init values
fwhm = 0.
back = 0.
nf = 0L
if n_elements(im) le 0 then $
	im=mrdfits(imfile,0,hdr)
;
; get sky
sky,im>0,lastmn,lastsg,/silent
;
s=size(im)
nx=s(1)
ny=s(2)
;
; get box size
bx = 600
mag = 1.0
bx1 = bx - 1
hbx = bx / 2
hbx1 = hbx - 1
grid = get_grid(bx,nx,ny,ng)
;
; create display window
window,2,xpos=10,ypos=100,xsize=(mag*bx),ysize=(mag*bx),title=imfile
;
; loop over grids
first = (1 eq 1)
for ig = 0L, ng-1 do begin
;
; print grid status
	print,'g/t,xc,yc: ',strn(ig+1)+'/'+strn(ng)+' '+$
		strn(grid(4,ig))+', '+strn(grid(5,ig))
;
; get subim limits
	x0=grid(0,ig)>0
	x1=grid(1,ig)<(nx-1)
	y0=grid(2,ig)>0
	y1=grid(3,ig)<(ny-1)
	xc=(x1 - x0) / 2 + x0
	yc=(y1 - y0) / 2 + y0
;
; extract subim and get stats
	sim=im(x0:x1,y0:y1)
	sky,sim,mn,sg,/silent
	if sg < 0. then $
		if lastsg < 0. then $
			sg = mn / 10. $
		else	sg = lastsg
	lastsg = sg
	print,'mn,sg: ',mn,sg
; display subim
	wset,2
	tvscl,sim>(mn-3*sg)<(mn+9*sg)
;
	qq = ''
	while qq ne 'q' do begin
	    myrdpix,sim,x0,y0,mag,xc,yc,ret
	    cursor,xx,yy,/nowait,/device
;
	    if ret eq 1 then begin
    		cntrd,sim,xx,yy,xc,yc,10,/silent
    		r = sqrt( (float(xx)-xc)^2 + (float(yy)-yc)^2 )
    		if r lt 8 then begin
		    xx = fix(xc+0.5)
		    yy = fix(yc+0.5)
	    	    if xx gt 16 and xx lt min([bx,nx])-17 and $
		       yy gt 16 and yy lt min([bx,ny])-17 then $
		    begin
			ssim = sim( xx-16:xx+15,yy-16:yy+15 )
			res = gauss2dfit(ssim,a,/tilt)
			if a(2) gt 0 and a(3) gt 0 and $
			   a(2) lt 15.0 and a(3) lt 15.0 then begin
			    see = max( [ a(2), a(3) ] )
			    fwhm = fwhm + see * 2.354
			    back = back + a(0)
			    nf = nf + 1L
			    print,nf,see*2.354,fwhm/nf,a(0),back/nf
			    mycircle,xx,yy,15,bx*mag,bx*mag
			endif
		    endif else begin
			print,'Star too near edge: ',xx,yy
		    endelse
    		endif else print,'!!bad centroid!!'
		wait,0.25
	    endif else if ret eq 2 then begin
		qq = 'q'
		wait,0.25
	    endif else if ret eq 4 then begin
		qq  = 'q'
		ig = ng
		wait,0.25
	    endif
	endwhile
;
endfor	; end loop over grids
;
if nf gt 0 then begin
	fwhm = fwhm / nf
    	back = back / nf
endif else begin
	fwhm = 99.999
	back = -99.999
endelse

return
end	; get_imstats
