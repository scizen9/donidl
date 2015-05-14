;+
; NAME:
;   mimap_match_sky
;
; VERSION:
;   2.0 (Sep, 2011)
;
; PURPOSE:
;   match the sky backgrounds before coadding
;
; CATEGORY:
;   Image Manipulation
;
; CALLING SEQUENCE:
;   mimap_match_sky,dir,ims,ra=ra,dec=dec,ps=ps,size=size, $
;	wtype=wtype,tweak=tweak,pixratio=pixratio,outs=outs,band=band
;
; INPUTS:
;     dir - directory containing image files
;     ims - image filenames within directory
;
; KEYWORD PARAMETERS:
;  ra,dec - coordinates of center
;      ps - pixel scale in arcsecs/pixel
;    size - vector containing x and y size in arcseconds
;   wtype - SWARP weight type
;   tweak - vector of tweak values for each image
;    outs - output file names
;    band - filter
;
; OUTPUT:
;    creates sky matched set of images with names in OUTS
;
; COMMENTS:
;    called by combine command
;
; REVISION HISTORY:
;    Mar 2008 - written, B. Johnson
;    Sep 2011 - adapted, D. Neill
;
;--------------------------------------------------------------------
PRO mimap_match_sky,dir,ims,ra=ra,dec=dec,ps=ps,size=size, $
	tweak=tweak,natpix=natpix,outs=outs,band=band, $
	fast=fast,medium=medium,slow=slow

;
; deafults to 0.396 (SDSS native pxscl)
if keyword_set(natpix) EQ 0 then natpix=0.396

outs=repstr(ims,'.fit','_mms.fit')
nim=n_elements(ims)

;get and apply the tweaks for each image
print,'Calculating sky tweaks'

if nim GT 1 then begin
  tweak=mimap_mms_tweaks(ims,outs,softening=0.5,ra=ra,dec=dec, $
	  ps=ps,natpix=natpix,size=size, $
	  fast=fast,medium=medium,slow=slow)
endif else tweak=[0]

;overwrites the reprojected images (if any)
print,'Applying sky tweaks'
mimap_mms_apply_tweak,ims,tweak,outnames=outs

;record tweaks
openw,1,dir+'mms_'+band+'.tweaks',width=800
printf,1,'file   tweak (to subtract from file)'
for i=0,nim-1 do printf,1,ims[i],tweak[i],format='(A,F15.10)'
close,1

print,'sky tweaks calculated and applied to calibrated images:'
for i=0,nim-1 do print,ims[i],tweak[i],format='(A,F15.10)'

end
