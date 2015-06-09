; $Id$
; KCWI utility routine
pro fpc_mira_cut,imf,pxscl,xc,yc,fpcbin=fpcbin,pmfm=pmfm,rotpposn=rotpposn
;+
;	fpc_mira_cut - cut out a simulated fpc mira image from input image
;
; INPUTS:
;	imf	- input image file
;	pxscl	- input image pixel scale
;	xc,yc	- center pixel position of MIRA star in input image
;
; KEYWORDS:
;	fpcbin	- fpc binning, square (defaults to 4x4)
;	pmfm	- MIRA PMFM parameter (defaults to 300)
;	rotpposn- Rotator physical angle
;-
; check inputs
if keyword_set(fpcbin) then $
	bin = fpcbin $
else	bin = 4
if keyword_set(pmfm) then $
	pm = pmfm $
else	pm = 300
if keyword_set(rotpposn) then $
	rotp = rotpposn $
else	rotp = 0.
;
; read image
im = mrdfits(imf, 0, hdr)
sz = size(im,/dimen)
;
; calculate fpc pixel scale requested
fpxscl = double(bin) * 0.007535
;
; calculate fpc output image size
fpsz = [ fix(3296/bin), fix(2472/bin) ]
;
; calculate scale ratio
scrat = pxscl/ fpxscl
;
; new image dimensions
xrs = fix(sz[0]*scrat)
yrs = fix(sz[1]*scrat)
;
; resample input image to fpc pixel scale
rsim = congrid(im, xrs, yrs, /interp)
rsz = size(rsim,/dimen)
;
; calculate center in resampled image
xrc = xc * scrat
yrc = yc * scrat
;
; calculate resampled image limits
xr0 = fix(xrc - fpsz[0]/2) > 0
yr0 = fix(yrc - fpsz[1]/2) > 0
xr1 = fix(xrc + fpsz[0]/2 - 1) < (rsz[0] - 1)
yr1 = fix(yrc + fpsz[1]/2 - 1) < (rsz[1] - 1)
;
xo0 = ((fpsz[0] - xr1)/2) > 0
yo0 = ((fpsz[1] - yr1)/2) > 0
xo1 = xo0 + (xr1 - xr0)
yo1 = yo0 + (yr1 - yr0)
;
; create output image
oim = fltarr(fpsz)
;
; fill with data
oim[xo0:xo1,yo0:yo1] = rsim[xr0:xr1,yr0:yr1]
;
; get output filename
rute = strmid(imf,0,strpos(imf,'.'))
;
; update header
sxaddpar,hdr,'FPCBIN',bin,' FPC binning (square)'
sxaddpar,hdr,'PMFM',pm
sxaddpar,hdr,'ROTPPOSN',rotp
sxaddpar,hdr,'INSTRUME','KCWI'
sxaddpar,hdr,'PXSCL',fpxscl,' FPC pixel scale asec/px'
;
; output image
outf = rute+'_fpc'+strn(bin)+'x'+strn(bin)+'.fits'
mwrfits,oim,outf,hdr,/create
;
; report
fmts = '(a,f6.3)'
fmtc = '(a,2f8.2)'
fmti = '(a,2i8)'
print,'output file              : ',outf
print,'input pixel scale ("/pix): ',pxscl,format=fmts
print,'FPC   pixel scale ("/pix): ',fpxscl,format=fmts
print,'FPC   image dimensions   : ',fpsz,format=fmti
print,'input image dimensions   : ',sz,format=fmti
print,'resamp. image dimensions : ',xrs,yrs,format=fmti
print,'resampled center position: ',xrc,yrc,format=fmtc
print,'x range from resampled im: ',xr0,xr1,format=fmti
print,'y range from resampled im: ',yr0,yr1,format=fmti
print,'x range from output image: ',xo0,xo1,format=fmti
print,'y range from output image: ',yo0,yo1,format=fmti
;
return
end
