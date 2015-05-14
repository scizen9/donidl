;+
; NAME:
;   MIMAP_SDSS_COMBINE
;
; VERSION:
;   0.0 (Mar, 2008)
;
; PURPOSE:
;   To take SDSS survey images (corrected frames) and convert to a
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
;   mimap_sdss_combine,dir,band,outname
;     ra,dec,ps=ps,size=size,unitname=unitname,unit=unit,/nolimit
;
;
; INPUTS:
;    dir - the directory containing the sdss images and tsField files
;    band - string (ugriz) specifiying the band to combine
;    ra - the central ra of the output image in decimal degrees
;    dec - the central dec of the output image in decimal degree
;    
; KEYWORD PARAMETERS:
;    ps - two element array giving the output x and y pixel size in
;         arcseconds
;    size - two elements array giving the output x and y image size in
;           arcseconds
;    nolimit - set to remove limiting number of images to 30
;
;
; OUTPUT:
;    Output images are written to the filenames given in outnames
;
; COMMENTS:
;    SDSS survey images are read, the conversion is made from counts
;    to a physical flux unit (default is microJy) per pixel, using the
;    photometric data in the tsField files.  Uncertainty images may be
;    made from the count data (and detector gain information).  Images
;    are then combined using SWARP
;
; USES:
;    mimap_sdss_convert
;
; REVISION HISTORY:
;    Mar 2008 - written, B. Johnson
;
;--------------------------------------------------------------------

PRO mimap_sdss_combine,dir,band,outname,ra,dec,ps=ps,size=size,cskip=cskip, $
	osample=osample,rtype=rtype,ctype=ctype,skytweak=skytweak, $
	nolimit=nolimit,fast=fast,medium=medium,slow=slow

;***********
;reprojection keywords (some are redundant with default.swarp
;defaults)
;***********

if keyword_set(ps) EQ 0 then $
   ps=[0.5,0.5]            ;in arcsec
if keyword_set(size) EQ 0 then $
   size=[9.5,9.5]*60.*3         ;in arcsec
wtype='MAP_RMS'
ctype='AVERAGE'
if keyword_set(osample) EQ 0 then osample='2'
if keyword_set(rtype) EQ 0 then rtype='LANCZOS3'

;***********
;filename bookkeeping
;**********

;input images
ims=file_search(dir+'fpC*fit*')
thisBand=where(strpos(ims,'-'+band) GE 0)
ims=ims[thisBand]
nims=n_elements(ims)
;limit number
if nims gt 31 and not keyword_set(nolimit) then begin
	ims=ims[0:30]
	nims=n_elements(ims)
endif

;output image names after unit conversion
outs=repstr(ims,'fpC','mimap_mjy_sdss')
outs=repstr(outs,'.gz','')
unc=repstr(outs,'mjy','mjyErr')

;output image names after reprojection and combination (final product)
outname=strcompress(outname,/remove_all)
weightname=repstr(outname,'.fits','_ivar.fits')

;******************
;calibration data filenames
;******************

tsf=strarr(nims)

for ii=0,nims-1 do begin
  iname=(reverse(strsplit(ims[ii],'/',/extract)))[0]
  fp=(strsplit(repstr(iname,'.gz',''),'-',/extract))
  fp[2]=strmid(fp[2],1,1)
  cf=file_search(dir+'drField-'+fp[1]+'-'+fp[2]+'-*-'+fp[3],count=ncf)
  if ncf le 0 then $
    cf=file_search(dir+'tsField-'+fp[1]+'-'+fp[2]+'-??-'+fp[3],count=ncf)
  tsf[ii]=cf[n_elements(cf)-1]
endfor
;
; trim images without calibration
bad =where(strlen(strtrim(tsf,2)) eq 0, nbad)
good=where(strlen(strtrim(tsf,2)) gt 0, ngood)
if nbad gt 0 then begin
	ims = ims[good]
	outs= outs[good]
	unc = unc[good]
	tsf = tsf[good]
	nims= ngood
endif

;************
;do the calibration and unit conversion of fpC data, with optional
;tweaking of sky background
;*************

if keyword_set(cskip) EQ 0 then $
   mimap_sdss_convert,ims,outs,unc_image=unc,tsField=tsf,/sskysub
if keyword_set(skytweak) then begin
   mimap_match_sky,dir,outs,ra=ra,dec=dec,ps=[0.5,0.5],size=size, $
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

mimap_swarp_config,ra=ra,dec=dec,ps=ps,size=size,uims=unc,wtype=wtype, $
	ctype=ctype,outimname=outname,weightname=weightname,osample=osample, $
	rtype=rtype

call='swarp @swarplist_tmp -c mimap.swarp'
spawn,call
spawn,'mv mimap.swarp '+dir+'mimap_'+band+'.swarp'
spawn,'mv swarplist_tmp '+dir+'inlist_'+band+'.swarp'

end
