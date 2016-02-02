pro sedm_check_bkg,ifuf
;+
; sedm_check_bkg - check the background subtraction using a bright star
;-
img = mrdfits(ifuf,0,ihdr)
sz = size(img,/dim)
;
; find peak
cntrd,img,sz[0]/2,sz[1]/2,xc,yc,sz[0]/5.
x = fix(xc)
;
; get plot ranges
xrng = [fix(xc-sz[0]/3.)>0, fix(xc+sz[0]/3.)<(sz[0]-1)]
yrng = [fix(yc-sz[1]/3.)>0, fix(yc+sz[1]/3.)<(sz[1]-1)]
yprn = [1.,max(img[xrng[0]:xrng[1],yrng[0]:yrng[1]])]
;
; get background
bgd = mrdfits('bgd_'+ifuf+'.gz',0,bhdr)
;
; get subtraction
sub = mrdfits('bs_'+ifuf+'.gz',0,shdr)
;
; get width of Gaussian filter
gw = sxpar(shdr,'gaufwid')
;
; plot
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=3
;
plot,img[x,*],title='GAUSSIAN WIDTH = '+string(gw)+' PX', $
	xtitle='X PX', xran=yrng,/xs, $
	ytitle='Int', yran=yprn,/ys,/ylog
oplot,bgd[x,*], linesty=2,color=colordex('B'),thick=th
;
return
end
