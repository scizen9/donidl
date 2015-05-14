pro test_sdss_sky,f
;+
; TEST_SDSS_SKY - do a diagnostic plot of SDSS sky from drField files
; INPUTS:
;	f	- filter number (0-4, for u,g,r,i, or z)
;-
flist=file_search('drField*.fit',count=nf)
print,'Found ',nf
sky_sfs = fltarr(nf)
sigpix = fltarr(nf)
sky = fltarr(nf)
skysig = fltarr(nf)
;
for i=0,nf-1 do begin
	calib=mrdfits(flist[i],1,/silent)
	sky_sfs[i] = calib.sky_frames_sub[f]
	sigpix[i]  = calib.sigpix[f]
	sky[i]     = calib.sky[f]
	skysig[i]  = calib.skysig[f]
endfor
;
!p.multi=[0,1,2]
plot,sky_sfs,psym=4,ytitle='SKY'
oplot,sky,psym=5
plot,sigpix,psym=4,ytitle='SIG'
oplot,skysig,psym=5
;
diff = sky_sfs-sky
ims,diff,mn,sig,wgt,siglim=2.5
good = where(wgt ge 1)
badsky = sky_sfs
badsky[good] = 0.
;
forprint,indgen(nf),flist,badsky,form='(i5,2x,a,g12.5)'
return
end
