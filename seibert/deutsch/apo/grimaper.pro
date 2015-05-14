pro grimaper,image,h,mx,my,radii
;+
; NAME:
;	GRIMAPER
;
; PURPOSE:
;	Quick aperture photometry wrapper for an image from the APO GRIM II.
;
; CATEGORY:
;	APO software
;
; CALLING SEQUENCE:
;	grimaper,image,header,xcenter,ycenter,radii
;
; INPUTS:
;	image:	2D GRIM image array, usually read with GRIMREAD.
;
;	header:	Header array, usually read with GRIMREAD.
;
;	xcenter: X position of the center of the aperture.
;
;	ycenter: Y position of the center of the aperture.
;
;	radii:	array of aperture radii.
;
; OPTIONAL INPUT KEYWORDS:
;	None.
;
; OUTPUTS:
;	Printed aperture information.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	AStronomy library routine APER required.
;
; PROCEDURE:
;	Astronomy User's Library APER routine is called with appropriate
;	parameters.
;
; EXAMPLE:
;	grimread,img,h,'/host/dione/u3/deutsch/grim/sample/n1.0004',/flagdef
;	grimaper,img,h,125,92,[5,7,9]
;
; MODIFICATION HISTORY:
;	1995 Written by E. Deutsch
;
;-

  if (n_params(0) ne 5) then begin
    print,"Call> grimaper,image,h,xcen,ycen,radii"
    print,"e.g.> grimaper,img,h,125,92,[3,5,7,9]"
    return
    endif

  expt=sxpar(h,'OPENTIME')
  print,'Exposure Time: ',expt

  filt=sxpar(h,'FILTER1')
  zpt=0.0
  if (filt eq 1) then begin
    zpt=1.77
    print,'Filter=J  Using Zero Point: ',zpt
    endif
  if (filt eq 4) then begin
    zpt=2.32
    print,'Filter=K''  Using Zero Point: ',zpt
    endif
  if (zpt eq 0) then begin
    print,'Uncalibrated filter:  Using Zero Point: ',zpt
    endif

  skyaper=[15,25]
  if (skyaper(0) lt max(radii)) then skyaper=max(radii)+[5.0,20.0] 

  aper,image/expt,mx,my,mags,merr,skyv,serr,5.0,radii,skyaper, $
    [-1,65530.0],/silent
  print,''
  print,'Apers: ',radii*1L
  print,'Cnt/sec: ',10^((mags-25)/(-2.5))
  print,'Mags: ',mags-zpt
  print,'MErrs: ',merr
  print,'Sky',vect(skyaper),': ',skyv(0),' +/- ',serr(0)

  return

end
