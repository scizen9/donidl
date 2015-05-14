pro wfpcmrg,infile,outfile,trim,sky,CRclean
;+
; NAME:
;   WFPCMRG
; DESCRIPTION:
;   This procedure is designed to load all four chips of a WF or a PC
;   and combining them into one 16 bit INTEGER image.  The chips may
;   be trimmed or left whole.  Sky subtraction options are available.
;   This is desgined for WFPC I images only!
; CALLING SEQUENCE:
;   wfpcmrg,infile,outfile,trim,sky,CRclean
; INPUT PARAMETERS:
;   INFILE     The name of the input WFPC group file with HEADER extender
;   OUTFILE    The name of the output assembled file with HEADER extender
;   TRIM       Trim Method:  This flag specifies the type of trimming used:
;                0 = No Trimming
;                1 = Trimming according to parameters decided by RCB & EWD
;                    such that there is a small gap between the chips, so
;                    that no part of the image is lost. (DEFAULT)
;                2 = Trimming so that the Astrometry is fairly accurate for
;                    all 4 chips.  (NOT YET AVAILABLE)
;   SKY	       Sky/background subtraction.  The background can be subtracted
;                according to the following specifications.  The SKYCALC
;                procedure returns a median sky value and the RMS of the
;                sky level noise.  Values of SKY subtract to obatain the
;                following specifications:
;                  0 = No Sky subtraction.  Chips assembled as is. (DEFAULT)
;                  1 = Subtract SKYV+1.5*RMS which is usually about the top
;                      of the noise.
;                  2 = Subtract SKYV which is usually the middle of the
;                      sky level noise
;                  3 = Subtract SKYV-1.5*RMS which is usually about the
;                      bottom of the noise level.
;                  4 = Adjust sky level of all chips to be equal to that of
;                      chip 1 (add or subtract as necessary)
;                  5 = Use DJL routine SKY_VALUE to compute the sky and
;                      maximum value of each chip
;                  n = If SKY is greater than 5, the sky level of all chips
; 	               will be set to this value (+ or - as necessary)
;   CRCLEAN    Set to 1 to remove Cosmic Rays.  (Default=0)
; OUTPUT:
;   The output files: both header and data files
;   All passed variables remain unchanged
; NOTES:
;   - Chips are never rotated during the matching.  To do the best fitting
;     job, they need to be rotated (resampled).  The current trimming
;     is 'best guess
;   - Some portions of this program were taken from a procedure called
;     MOSAIC by E. Malumuth (written 25-MAY-90.)
; HISTORY:
;   05-JUN-90 Version 1 written by Eric W. Deutsch
;   22-JUN-90 Trimming values finalized for WF (RCB and EWD)
;   08-AUG-90 Sky subtraction, History writing, and Astrometry
;     Interpretation parts fixed and extended.  EWD.
;   09-AUG-90 Added parameters needed by DJL for his CELCO submission
;     rountines.  EWD.
;   21-SEP-90 Chips reoriented to match Press release orientation
;     (presumably correct.  Original MOSAIC was wrong...)  EWD.
;   10-DEC-92 Fixed header and tidied up a bit.  EWD.
;-

  arg=n_params(0)
  if (arg lt 2) then begin
    print,'Call: IDL> WFPCMRG,input_file,output_file,[trim_flag,sky_flag,CRclean]'
    print,"e.g.: IDL> WFPCMRG,'W0340Z03R.D0H','MRG034.HHH',1,5,0"
    return
    endif
  if (arg lt 3) then trim=1    ; default trimming is YES
  if (trim gt 1) then begin
    print,'Invalid TRIM option.  Setting to default of 1=YES'
    trim=1
    endif
  if (arg lt 4) then sky=0        ; default sky subtraction is NO
  if (arg lt 5) then CRclean=0    ; default CR cleaning is NO

  image=fltarr(1600,1600)
  size=800 & skvals=fltarr(4) & maxval=0 & mxvals=skvals

;  Define WFPC Fitting offsets::  Trim= o(chip-1,axis[x=0,y=1])
;    All values are in # of columns or row to remove.

  print,'Loading Chip 1...'
  wfpcread,infile,0,htmp,tmp,par

  CAMERA=strtrim(sxpar(htmp,'CAMERA'),2)
  if (CAMERA ne 'WF') and (CAMERA ne 'PC') then begin
    print,'Unrecognized camera: ',CAMERA
    print,'Assuming Camera is: WF'
    CAMERA='WF'
    endif

  o=intarr(4,2)
  if (trim eq 1) and (CAMERA eq 'WF') then begin
    o(0,0)=17+13 & o(0,1)=25-00
    o(1,0)=11+02 & o(1,1)=25-00
    o(2,0)=20+07 & o(2,1)=25-03
    o(3,0)=17+03 & o(3,1)=23+08
    endif
  if (trim eq 1) and (CAMERA eq 'PC') then begin
    o(0,0)=31 & o(0,1)=30
    o(1,0)=19 & o(1,1)=32
    o(2,0)=30 & o(2,1)=33
    o(3,0)=20 & o(3,1)=30
    endif

  if (CRclean eq 1) then begin
    print,'** Remove Cosmic Rays from Chip **'
    IMGclean,tmp
    endif
  if (SKY gt 5) then setsky=SKY
  if (SKY gt 0) then begin
    if (SKY eq 5) then begin
      BITPIX=sxpar(htmp,'BITPIX')
      sky_value,tmp,BITPIX,10,skyv,maxv
      skvals(0)=skyv & mxvals(0)=maxv
      if (maxv gt maxval) then maxval=maxv
      print,'Sky Subtracted: ',strn(skyv),'    MaxVal: ',strn(maxv)
    endif else begin
      skycalc,tmp,skyv,rmsv
      if (SKY eq 4) then setsky=skyv
      if (SKY lt 4) then skvals(0)=skyv+1.5*rmsv*(-1*(SKY-2))
      if (SKY ge 4) then skvals(0)=skyv-setsky
      print,'Sky: ',strn(skyv),'    RMS: ',strn(rmsv), $
        '    Sky Subtracted: ',strn(skvals(0))
      endelse
    tmp2=tmp-skvals(0) & tmp=1
  endif else begin
    tmp2=tmp
    endelse
  print,'Rotating and Assembling Chip 1...'
  arry=1 & arry=rotate(tmp2,3) & tmp2=1
  image(size:2*size-1-o(0,0),o(0,1):size-1)= $
    arry(o(0,0):size-1,0:size-1-o(0,1))
  arry=1

  print,'Loading Chip 2...'
  wfpcread,infile,1,h,tmp
  if (CRclean eq 1) then begin
    print,'** Remove Cosmic Rays from Chip **'
    IMGclean,tmp
    endif
  if (sky gt 0) then begin
    if (SKY eq 5) then begin
      sky_value,tmp,BITPIX,10,skyv,maxv
      skvals(1)=skyv & mxvals(1)=maxv
      if (maxv gt maxval) then maxval=maxv
      print,'Sky Subtracted: ',strn(skyv),'    MaxVal: ',strn(maxv)
    endif else begin
      skycalc,tmp,skyv,rmsv
      if (SKY lt 4) then skvals(1)=skyv+1.5*rmsv*(-1*(SKY-2))
      if (SKY ge 4) then skvals(1)=skyv-setsky
      print,'Sky: ',strn(skyv),'    RMS: ',strn(rmsv), $
        '    Sky Subtracted: ',strn(skvals(1))
      endelse
    tmp2=tmp-skvals(1) & tmp=1
  endif else begin
    tmp2=tmp
    endelse
  print,'Rotating and Assembling Chip 2...'
  arry=tmp2 & tmp2=1
  image(size:2*size-1-o(1,0),size:2*size-1-o(1,1))= $
    arry(o(1,0):size-1,o(1,1):size-1)
  arry=1

  print,'Creating equivalent Astrometry for image (accurate for Chip 2)'
  CRPIX1=sxpar(h,'CRPIX1')
  CRPIX2=sxpar(h,'CRPIX2')
  sxaddpar,h,'CRPIX1',800+CRPIX1-o(1,0),' Calculated From chip 2'
  sxaddpar,h,'CRPIX2',800+CRPIX2-o(1,1),' Calculated From chip 2'

  xyad,h,800,800,ra,dec
  sxaddpar,h,'RA',ra,' Image Center (degrees)
  sxaddpar,h,'DEC',dec,' Image Center (degrees)'
  astrmfix,h,2

  print,'Loading Chip 3...'
  wfpcread,infile,2,htmp,tmp,par
  if (CRclean eq 1) then begin
    print,'** Remove Cosmic Rays from Chip **'
    IMGclean,tmp
    endif
  if (sky gt 0) then begin
    if (SKY eq 5) then begin
      sky_value,tmp,BITPIX,10,skyv,maxv
      skvals(2)=skyv & mxvals(2)=maxv
      if (maxv gt maxval) then maxval=maxv
      print,'Sky Subtracted: ',strn(skyv),'    MaxVal: ',strn(maxv)
    endif else begin
      skycalc,tmp,skyv,rmsv
      if (SKY lt 4) then skvals(2)=skyv+1.5*rmsv*(-1*(SKY-2))
      if (SKY ge 4) then skvals(2)=skyv-setsky
      print,'Sky: ',strn(skyv),'    RMS: ',strn(rmsv), $
        '    Sky Subtracted: ',strn(skvals(2))
      endelse
    tmp2=tmp-skvals(2) & tmp=1
  endif else begin
    tmp2=tmp
    endelse
  print,'Rotating and Assembling Chip 3...'
  arry=rotate(tmp2,1) & tmp2=1
  image(o(2,0):size-1,size:2*size-1-o(2,1))= $
    arry(0:size-1-o(2,0),o(2,1):size-1)
  arry=1

  print,'Loading Chip 4...'
  wfpcread,infile,3,htmp,tmp,par
  if (CRclean eq 1) then begin
    print,'** Remove Cosmic Rays from Chip **'
    IMGclean,tmp
    endif
  if (sky gt 0) then begin
    if (SKY eq 5) then begin
      sky_value,tmp,BITPIX,10,skyv,maxv
      skvals(3)=skyv & mxvals(3)=maxv
      if (maxv gt maxval) then maxval=maxv
      print,'Sky Subtracted: ',strn(skyv),'    MaxVal: ',strn(maxv)
    endif else begin
      skycalc,tmp,skyv,rmsv
      if (SKY lt 4) then skvals(3)=skyv+1.5*rmsv*(-1*(SKY-2))
      if (SKY ge 4) then skvals(3)=skyv-setsky
      print,'Sky: ',strn(skyv),'    RMS: ',strn(rmsv), $
        '    Sky Subtracted: ',strn(skvals(3))
      endelse
    tmp2=tmp-skvals(3) & tmp=1
  endif else begin
    tmp2=tmp
    endelse
  print,'Rotating and Assembling Chip 4...'
  arry=rotate(tmp2,2) & tmp2=1
  image(o(3,0):size-1,o(3,1):size-1)= $
    arry(0:size-1-o(3,0),0:size-1-o(3,1))
  arry=1

  print,'Modifying Header file...'
  sxaddpar,h,'NAXIS1',1600,' Assembled WFPC samples'
  sxaddpar,h,'NAXIS2',1600,' Assembled WFPC lines'

;  sxaddpar,h,'BITPIX',16,' 16 bit Integer'
;  sxaddpar,h,'DATATYPE','INTEGER*2',' 16 bit Integer'

  FPKTTIME=sxpar(h,'FPKTTIME')
  sxaddpar,h,'FPKTTIME',strmid(FPKTTIME,0,strlen(FPKTTIME)-2),' Seconds removed'
  LPKTTIME=sxpar(h,'LPKTTIME')
  sxaddpar,h,'LPKTTIME',strmid(LPKTTIME,0,strlen(LPKTTIME)-2),' Seconds removed'
  avesky=avg(skvals)
  sxaddpar,h,'AVESKY',avesky,' Average sky of all 4 chips'
  if (maxval gt 0) then $
    sxaddpar,h,'MAXVAL',maxval,' Maximum value of all 4 chips'

  sxaddhist,'WFPCMRG: WFPC Image assembled '+systime(0),h
  tmp='Chip Trimming: '+strn(trim)+' -- Sky Calc. type: '+strn(SKY)
  if (SKY eq 5) then tmp=tmp+'   (SKY_VAL[DJL])'
  if (SKY gt 0) and (SKY ne 5) then tmp=tmp+'   (SKYCALC[EWD])'
  sxaddhist,'WFPCMRG: '+tmp,h
  sxaddhist,'WFPCMRG: Group information extracted from Chip '+CAMERA+'2',h
  sxaddhist,'WFPCMRG: Astrometry Accurate for this chip only',h
  sxaddhist,'WFPCMRG: Orientation: If (0,0) is in the lower left corner, '+ $
    CAMERA+'1 is in',h
  sxaddhist,'          the lower right quadrant.  Other chips '+ $
    'ascend counterclockwise.',h
  if (CRclean eq 1) then $
    sxaddhist,'IMGclean: Cosmic Rays removed from each chip sepatately, ',h

  if (SKY gt 0) then begin
    sks=vect(skvals)
    sxaddhist,'WFPCMRG: Subtracted Sky Values: '+sks,h
    endif
  if (maxval gt 0) then begin
    sks=vect(mxvals)
    sxaddhist,'WFPCMRG: Maximum Values: '+sks,h
    endif

  filtr=strn(sxpar(h,'FILTNAM1')) & space=strpos(filtr,' ')
  if (space ne -1) then begin
    filtr=strmid(filtr,0,space)
    sxaddpar,h,'FILTNAM1',filtr,' First filter name'
    endif

  print,'Saving output file...'

  fdecomp,outfile,disk,dir,file,qual
  if (qual eq '') then qual='hhh'
  outfile=disk+dir+file+'.'+qual
  sxhwrite,outfile,h
  outfile=disk+dir+file+'.'+strmid(qual,0,2)+'d'

  openw,1,outfile,512,/block
  tmp=assoc(1,image)
  tmp(0)=image
  close,1

  print,'Procedure Finished.'
  return
end
