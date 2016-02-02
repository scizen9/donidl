pro sedm_check_bkg,ifuf
;+
; sedm_check_bkg - check the background subtraction using a bright star
;-
img = mrdfits(ifuf,0,ihdr)
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
plot,img[1024,*],title='GAUSSIAN WIDTH = '+string(gw)+' PX', $
	xtitle='X PX', xran=[800,1500],/xs,/ylog
oplot,bgd[1024,*], linesty=2
;
return
end
