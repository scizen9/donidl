;+
; NAME:
;   MIMAP_SDSS_CONVERT
;
; VERSION:
;   0.0 (Mar, 2008)
;
; PURPOSE:
;   To take SDSS survey images (corrected frames) and convert to a
;   physical photometric scale, updating the header.  Optionally
;   create an uncertainty image.
;
; CATEGORY:
;   Image Manipulation
;
; CALLING SEQUENCE:
;   mimap_sdss_convert,imnames,outnames,tsobj=tsobj,uncname=uncname,
;                      unitname=unitname,unit=unit
;
; INPUTS:
;    imnames - string array of the names (and paths) sdss images to be
;              converted 
;    outnames - the output image names
;    
; KEYWORD PARAMETERS:
;    tsField - the the names of the tsfield files that hold the
;            photometric calibration data
;    unc_image - if present, create uncertainty images with the names 
;                given in this string array.
;    unitname - output flux unit.  Default is milliJy
;    unit - if present, the conversion from mJy to the unit
;           given in unitname above
;    sskysub - if present subtract the sky background using the SDSS
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
;    SDSS survey images are read, the conversion is made from counts
;    to a physical flux unit (default is microJy) per pixel, using the
;    photometric data in the tsField files.  Uncertainty images may be
;    made from the count data (and gain information)
;
; REVISION HISTORY:
;    Mar 2008 - written, B. Johnson
;    Mar 2009 - Fixed uncertainty calculation
;
;--------------------------------------------------------------------

PRO mimap_sdss_convert,imnames,outnames,tsField=tsField,unc_image=unc_image, $
	unit=unit,unitname=unitname,band=band,sskysub=sskysub,myskysub=myskysub

aband=['u','g','r','i','z']

;because sky fluxes are given in asinh maggies
softening=[0.4E-10,0.9E-10,1.29E-10,1.89E-10,7.49E-10]
m0=0.
ff0=2.0*softening*sinh( (-1.0*alog(10)*m0/2.5)-alog(softening) ) 

;conversion from SDSS to AB zeropoints
delta_cal=[-0.04,0.0,0.0,0.0,0.02]
delta_as=10.0^(0.-delta_cal/2.5)

exptime=53.907456
nim=n_elements(imnames)

print,'Calibrating: '

for iim=0,nim-1 do begin
  ;read in image and calibration data
  image=mrdfits(imnames[iim],0,header,/dscale,/silent)
  calib=mrdfits(tsField[iim],1,/silent)

  ;extract calibration data and flux conversion factor
  bandname=strcompress(sxpar(header,'FILTER'),/remove_all)
  if keyword_set(band) EQ 0 then band=(where(bandname EQ aband))[0]
  aa=calib.aa[band]
  kk=calib.kk[band]
  airmass=calib.airmass[band]
  c0=exptime*10.^(0.-0.4*(aa+kk*airmass))
  fluxconv=3631*delta_as[band]*1E3/c0
  gain=calib.gain[band]
  drkvar=calib.dark_variance[band]
  softbias=sxpar(header,'SOFTBIAS')
  getrot,header,pa,cd
  ps=avg(abs(cd)*3600.)

  ;determine sky background to subtract
  sky=0 & skyerr=0
  if keyword_set(sskysub) then begin
    ;use calib.sky and calib.skysig, below not as robust
    ;ssky=[calib.sky_frames_sub[band],calib.sigPix[band]]
    ssky=[calib.sky[band],calib.skysig[band]]
    ssky_counts=ssky[0]*c0*ps^2*ff0[band]
    sky=ssky_counts[0] 
    skyerr=ssky[1]*ssky_counts/1.086
  endif
  if keyword_set(myskysub) then begin 
    sky=myskysub[iim,0] 
    skyerr=myskysub[iim,1]  
  endif

  ;convert to other units if necessary and apply flux conversion
  if keyword_set(unit) then fluxconv=fluxconv*unit
  image=(image-softbias-sky)*fluxconv

  ;update header
  sxaddpar,header,'KK',kk,' tsField Extinction Coefficient'
  sxaddpar,header,'AA',aa,' tsField Zeropoint'
  sxaddpar,header,'AIRMASS',airmass
  if keyword_set(unitname) then sxaddpar,header,'BUNITS',unitname $
    else sxaddpar,header,'BUNITS','mJy (per pixel)'
  if keyword_set(unitname) then sxaddpar,header,'BUNIT',unitname $
    else sxaddpar,header,'BUNIT','mJy (per pixel)'
  sxaddpar,header,'SUBSKY',sky,' Subtracted sky value (ADU)'
  sxaddpar,header,'SUBSKY_ERR',skyerr,$
    ' Estimated error/pix on the sky value (ADU)'
  sxaddpar,header,'FLUXCONV',1./fluxconv,' Conversion from flux unit to ADU'
  sxaddpar,header,'GAIN',gain,' ADU to e-'
  sxaddpar,header,'DARK_VAR',drkvar,$
    ' readnoise and dark noise per pixel (ADUs)'
  date=string(systime(/julian),format='(C(CMOI2.2, "/", CDI2.2, "/", CYI))')
  sxaddpar,header,'HISTORY',' Created by MIMAP_SDSS_CONVERT ' $
    +strcompress(date,/remove_all)


  ;create uncertainty image
  if keyword_set(unc_image) then begin
     ;print,fluxconv
     ee=(image/fluxconv+sky)/gain  ;electrons
     poisson=sqrt(ee)*gain  ;poisson noise in electrons converted to counts
     error=sqrt(poisson^2+drkvar+skyerr^2)*fluxconv
     uheader=header
     sxaddpar,uheader,'HISTORY',' MIMAP Uncertainty Image'
     mwrfits,error,unc_image[iim],uheader,/create
  endif 

  mwrfits,image,outnames[iim],header,/create

  print,iim+1,'/',nim,imnames[iim],' -> ',outnames[iim], $
	  form='(i4,a1,i4,2x,a,a,a)'

endfor

end
