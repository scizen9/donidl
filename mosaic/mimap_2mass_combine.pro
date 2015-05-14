;+
; NAME:
;   MIMAP_2MASS_COMBINE
;
; VERSION:
;   0.0 (Mar, 2008)
;
; PURPOSE:
;   To take 2MASS survey images (corrected frames) and convert to a
;   physical photometric scale, updating the header, and then combine
;   using SWARP
;
; REFERENCE:
;   Bertin, E. 2008
;
; CATEGORY:
;   Image Manipulation
;
; CALLING SEQUENCE:
;   mimap_2mass_combine,dir,band,outname
;     ra,dec,ps=ps,size=size,unitname=unitname,unit=unit,/nolimit
;
;
; INPUTS:
;    dir - the directory containing the 2mass images
;    band - string (JHK) specifiying the band to combine
;    ra - the central ra of the output image in decimal degrees
;    dec - the central dec of the output image in decimal degree
;    
; KEYWORD PARAMETERS:
;    ps - two element array giving the output x and y pixel size in
;         arcseconds
;    size - two elements array giving the output x and y image size in
;           pixels
;    nolimit - set to remove limiting number of images to 30
;
;
; OUTPUT:
;    Output images are written to the filenames given in outnames
;
; COMMENTS:
;    2MASS survey images are read, the conversion is made from counts
;    to a physical flux unit (default is microJy) per pixel, using the
;    photometric data in the image header.  Uncertainty images may be
;    made from the count data (and detector gain information).  Images
;    are then combined using SWARP
;
; USES:
;    mimap_2mass_convert
;
; REVISION HISTORY:
;    Mar 2008 - written, B. Johnson
;
;--------------------------------------------------------------------

PRO mimap_2mass_combine,dir,band,outname,ra,dec,ps=ps,size=size,cskip=cskip, $
	osample=osample,rtype=rtype,ctype=ctype,skytweak=skytweak, $
	nolimit=nolimit,fast=fast,medium=medium,slow=slow

;***********
;reprojection keywords (some are redundant with default.swarp
;defaults)
;***********

if keyword_set(ps) EQ 0 then $
   ps=[1.0,1.0]            ;in arcsec
if keyword_set(size) EQ 0 then $
   size=[9.5,9.5]*60.*3         ;in arcsec
wtype='MAP_RMS'
ctype='AVERAGE'
if keyword_set(osample) EQ 0 then osample='2'
if keyword_set(rtype) EQ 0 then rtype='LANCZOS3'

;***********
;filename bookkeeping
;**********

;fspc = '*_asky_*fit*'
fspc = '?i???????.fit*'
;input images
ims=file_search(dir+fspc)
thisBand=where(strpos(ims,band) GE 0)
ims=ims[thisBand]
nims=n_elements(ims)
;limit number
if nims gt 31 and not keyword_set(nolimit) then begin
	ims=ims[0:30]
	nims=n_elements(ims)
endif

;output image names after unit conversion
;outs=repstr(ims,'asky','mimap_mJy_2mass')
outs='mimap_mJy_2mass_' + ims
outs=repstr(outs,'.gz','')
unc=repstr(outs,'_mJy_','_mJyErr_')

;output image names after reprojection and combination (final product)
outname=strcompress(outname,/remove_all)
weightname=repstr(outname,'.fits','_ivar.fits')

if keyword_set(cskip) EQ 0 then $
   mimap_2mass_convert,ims,outs,unc_image=unc,/sskysub
if keyword_set(skytweak) then begin
   mimap_match_sky,dir,outs,ra=ra,dec=dec,ps=[1.0,1.0],natpix=1.,size=size, $
	   outs=oo,band=band,fast=fast,medium=medium,slow=slow
   outs=oo
endif
;*************
;do the swarp reprojection and combining
;*************

print,'Final coaddition for ',outname
openw,1,'swarplist_tmp'
for i=0,n_elements(outs)-1 do printf,1,outs[i]
close,1

uims=unc
mimap_swarp_config,ra=ra,dec=dec,ps=ps,size=size,uims=uims,wtype=wtype, $
	ctype=ctype,outimname=outname,weightname=weightname,osample=osample, $
	rtype=rtype

call='swarp @swarplist_tmp -c mimap.swarp'
spawn,call
spawn,'mv mimap.swarp '+dir+'mimap_'+band+'.swarp'
spawn,'mv swarplist_tmp '+dir+'inlist_'+band+'.swarp'

end
