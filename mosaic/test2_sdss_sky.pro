pro test2_sdss_sky,f
;+
; TEST2_SDSS_SKY - Another test of the sky determination for SDSS data
;			Uses calibrated mimap* files
;	f	- filter number (0-4, for u,g,r,i, or z)
;-
;
filts=['u','g','r','i','z']
flist=file_search('mimap_mjy_sdss-??????-'+filts[f]+'?-????.fit',count=nf)
print,'Found ',nf
skys = fltarr(nf)
skysigs = fltarr(nf)
;
for i=0,nf-1 do begin
	im1=mrdfits(flist[i],/silent)
	sky,im1,skymode,skysig,/nan,/silent
	skys[i] = skymode
	skysigs[i] = skysig
	print,i,'/',nf,' sky for ',flist[i],' : ',skymode,' +- ',skysig, $
	format='(i4,a1,i4,a,a,a,f10.7,a,f10.7)'
endfor
;
yran=minmax([skys-skysigs,skys+skysigs])
xx=indgen(nf)
plot,xx,skys,psym=-4,ytitle='SKY',yrange=yran,xran=[-1,nf],xsty=1
oplot,xx,skys+skysigs,psym=5
oplot,xx,skys-skysigs,psym=5
ims,skysigs,ms,sg,wgt,siglim=2.0
print,'Avg skysig: ',ms,' +- ',sg,form='(a,f10.7,a,f10.7)'
bad=where(wgt lt 1, nbad)
if nbad gt 0 then begin
    print,'Baddies: ',nbad
    for i=0,nbad-1 do begin
	j = bad[i]
	im1=mrdfits(flist[j],0,hdr,/silent)
	nx=sxpar(hdr,'naxis1')
	ny=sxpar(hdr,'naxis2')
	skymode = 1.e9
	grid = get_grid(300,nx,ny,ng)
	for ig=0,ng-1 do begin
		x0=grid(0,ig)>0
		x1=grid(1,ig)<(nx-1)
		y0=grid(2,ig)>0
		y1=grid(3,ig)<(ny-1)
		sim=im1(x0:x1,y0:y1)
		sky,sim,mn,sg,/nan,/silent
		if mn lt skymode then begin
			skymode = mn
			skysig = sg
		endif
		print,ig,'/',ng,' sky = ',mn,' +- ',sg, $
			format='(i4,a1,i4,a,f10.7,a,f10.7)'
	endfor
	skys[j] = skymode
	skysigs[j] = skysig
	print,j,'/',nbad,' sky for ',flist[j],' : ',skymode,' +- ',skysig, $
	format='(i4,a1,i4,a,a,a,f10.7,a,f10.7)'
    endfor
    oploterr,xx[bad],skys[bad],skysigs[bad]
endif
;
;forprint,indgen(nf),flist,badsky,form='(i5,2x,a,g12.5)'
return
end
