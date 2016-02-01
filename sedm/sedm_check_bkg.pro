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
; plot
plot,img[1024,*],xran=[800,1500],/xs,/ylog
oplot,bgd[1024,*]
;
return
end
