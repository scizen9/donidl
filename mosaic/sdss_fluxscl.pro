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

FUNCTION sdss_fluxscl,tsField,bandname

aband=['u','g','r','i','z']

;because sky fluxes are given in asinh maggies
softening=[0.4E-10,0.9E-10,1.29E-10,1.89E-10,7.49E-10]
m0=0.
ff0=2.0*softening*sinh( (-1.0*alog(10)*m0/2.5)-alog(softening) ) 

;conversion from SDSS to AB zeropoints
delta_cal=[-0.04,0.0,0.0,0.0,0.02]
delta_as=10.0^(0.-delta_cal/2.5)

exptime=53.907456

calib=mrdfits(tsField,1,/silent)

;extract calibration data and flux conversion factor
band=(where(bandname EQ aband))[0]
aa=calib.aa[band]
kk=calib.kk[band]
airmass=calib.airmass[band]
c0=exptime*10.^(0.-0.4*(aa+kk*airmass))
fluxconv=3631*delta_as[band]*1E3/c0
gain=calib.gain[band]
drkvar=calib.dark_variance[band]
ssky=[calib.sky[band],calib.skysig[band]]

return,fluxconv

end
