;+
; NAME:
;   MIMAP_2MASS_CONVERT
;
; VERSION:
;   0.0 (Mar, 2008)
;
; PURPOSE:
;   To take 2MASS survey images (corrected frames) and convert to a
;   physical photometric scale, updating the header.  Optionally
;   create an uncertainty image.
;
; CATEGORY:
;   Image Manipulation
;
; CALLING SEQUENCE:
;   mimap_2mass_convert,imnames,outnames,tsobj=tsobj,uncname=uncname,
;                      unitname=unitname,unit=unit
;
; INPUTS:
;    imnames - string array of the names (and paths) 2mass images to be
;              converted 
;    outnames - the output image names
;    
; KEYWORD PARAMETERS:
;    unc_image - if present, create uncertainty images with the names 
;                given in this string array.
;    unitname - output flux unit.  Default is milliJy
;    unit - if present, the conversion from mJy to the unit
;           given in unitname above
;    sskysub - if present subtract the sky background using the 2MASS
;             measurement of the sky background from tsField
;    myskysub - N_im x 2 array. If present use this as the sky
;               background value to subtract, the second element is
;               the sky error for the uncertainty image
;
;
; OUTPUT:
;    Output images are written to the filenames given in outnames
;
; COMMENTS:
;    2MASS survey images are read, the conversion is made from counts
;    to a physical flux unit (default is microJy) per pixel, using the
;    photometric data in the image header.  Uncertainty images may be
;    made from the count data (and gain information)
;
; REVISION HISTORY:
;    Mar 2008 - written, B. Johnson
;    Mar 2009 - Fixed uncertainty calculation
;
;--------------------------------------------------------------------

PRO mimap_2mass_convert,imnames,outnames,unc_image=unc_image,unit=unit,unitname=unitname,band=band,sskysub=sskysub,myskysub=myskysub

aband=['j','h','k']
cal=[1594.d3, 1024.d3, 666.7d3]	; mJy fluxes of 0 mag

nim=n_elements(imnames)

print,'Calibrating: '

for iim=0,nim-1 do begin
  ;read in image and calibration data
  image=mrdfits(imnames[iim],0,header,/dscale,/silent)

  ;extract calibration data and flux conversion factor
  bandname=strcompress(sxpar(header,'FILTER'),/remove_all)
  if keyword_set(band) EQ 0 then band=(where(bandname EQ aband))[0]
  zp=sxpar(header,'MAGZP')
  fluxconv=cal[band] * 10.^(-0.4*zp)		; milliJy
  gain=8.0
  rednoi=5.0

  sky=0 & skyerr=0
  if keyword_set(sskysub) then begin
    sky=sxpar(header,'SKYVAL')
    skyerr=sxpar(header,'SKYSIG')
  endif
  if keyword_set(myskysub) then begin 
    sky=myskysub[iim,0] 
    skyerr=myskysub[iim,1]  
  endif

  ;convert to other units if necessary and apply flux conversion
  if keyword_set(unit) then fluxconv=fluxconv*unit
  image=(image-sky)*fluxconv

  ;update header
  if keyword_set(unitname) then sxaddpar,header,'BUNITS',unitname $
    else sxaddpar,header,'BUNITS','mJy (per pixel)'
  if keyword_set(unitname) then sxaddpar,header,'BUNIT',unitname $
    else sxaddpar,header,'BUNIT','mJy (per pixel)'
  sxaddpar,header,'SUBSKY',sky,' Subtracted sky value (ADU)'
  sxaddpar,header,'SUBSKY_ERR',skyerr,$
    ' Estimated error/pix on the sky value (ADU)'
  sxaddpar,header,'FLUXCONV',1./fluxconv,' Conversion from flux unit to ADU'
  sxaddpar,header,'GAIN',gain,' ADU to e-'
  sxaddpar,header,'RDNOISE',rednoi,' readnoise per pixel (ADUs)'
  date=string(systime(/julian),format='(C(CMOI2.2, "/", CDI2.2, "/", CYI))')
  sxaddpar,header,'HISTORY',' Created by MIMAP_2MASS_CONVERT ' $
    +strcompress(date,/remove_all)


  ;create uncertainty image
  if keyword_set(unc_image) then begin
     ;print,fluxconv
     ee=(image/fluxconv+sky)	;counts
     poisson=sqrt(ee)		;poisson noise
     error=sqrt(poisson^2+rednoi^2+skyerr^2)*fluxconv*0.1	; fudge factor
     uheader=header
     sxaddpar,uheader,'HISTORY',' MIMAP Uncertainty Image'
     mwrfits,error,unc_image[iim],uheader,/create
  endif 

  mwrfits,image,outnames[iim],header,/create

  print,iim+1,'/',nim,imnames[iim],' -> ',outnames[iim], $
  	form='(i4,a1,i4,2x,a,a,a)'

endfor

end
