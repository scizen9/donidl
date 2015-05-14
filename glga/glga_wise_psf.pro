pro glga_wise_psf,pffl
;+
; glga_wise_psf - generate the PSF data needed for glga with wise PSF iamges
;-
pim = mrdfits(pffl,0,hdr)
s=size(pim,/dim)
nx = s[0]
ny = s[1]
dist_ellipse,rim,s,float((nx/2.)-1.),float((ny/2.)-1.),1.0,0.
rim=rim*0.25	; 0.25 arcsec/pixel
peak = max(pim)
plot,rim,pim,psym=3
rr = fltarr(50)
nin = fltarr(50)
tmp=pffl
rute=gettok(tmp,'.')
openw,ol,rute+'.dat',/get_lun
for i=0,49 do begin
	r0 = float(i)
	r1 = r0 + 1.0
	t=where(rim ge r0 and rim lt r1, nt)
	if nt gt 0 then begin
		flx = mean(pim[t])/peak
		rr[i] = (r0 + r1) / 2.0
		nin[i] = flx
		print,rr[i],flx
		printf,ol,rr[i],flx
	endif
endfor
oplot,rr,nin*peak,psym=4,symsi=3.
free_lun,ol
;
return
end
