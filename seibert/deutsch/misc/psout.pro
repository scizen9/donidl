pro psout,image,xsize,ysize,xoffset,yoffset,color=colr,ctable=ctable, $
  autoprint=autoprint,inverse=inverse,bits=bits,dontclose=dontclose, $
  filename=filename
;+
; NAME:
;   PSOUT
; PURPOSE:
;   This procedure writes the specified image to a postscript file IDL.PS and
;   automatically sends it to the printer is the flag is set.
; CALLING SEQEUNCE:
;   pro psout,image,xsize,ysize,xoffset,yoffset,[/color,ctable=,
;     autoprint=,/inverse]
; INPUT:
;   IMAGE     2D array to be printed or IDL window number to read and print.
;               It should be a BYTE array or only the least significant byte
;               is used for display.
; OPTIONAL INPUT:
;   XSIZE     Width of image in inches
;   YSIZE     Height of image in inches
;   XOFFSET   Lwr-lft corner of image starts XOFFSET inches from the
;               left edge of paper.
;   YOFFSET   Lwr-lft corner of image starts YOFFSET inches from the
;               bottom edge of the paper.
; OPTIONAL KEYWORDS:
;   AUTOPRINT Specifies whether IDL.PS should automatically be sent to
;               the printer determined by:
;                 0=NO
;                 1=Last Printer printed to (PSCLOSE) or the Default Printer
;                 2=Choose Printer from list
;                 string=assumed to be the Queue Name.
;   INVERSE   If set, TV,255-image is used which will yield a white sky for
;               BYTE type images. (1=YES, 0=no)
;   COLOR     Specifies whether the output is to be color Postscript or not.
;               (1=YES, 0=no)
;   BITS      Number of BitsPerPixel to use.  Default is 8.
;   CTABLE    The optional keyword allows the user to specify a color table.
;             If CTABLE is a BYTARR(3,256) this array is used as the table.
;             If CTABLE is a scalar LOADCT is run with this parameter.
;             If CTABLE is not specified, the current color table loaded in
;               the COMMON block "colors" (CURRENT set) is used.  If no table
;               has been previously loaded, LOADCT is run and the user must
;               type in a color.
;   DONTCLOSE If this keyword is set, then the PostScript channel is left open
;               and the user must manually close it.  However, this allows
;               the user to send more output over the image (e.g. annotation)
;   FILENAME  This keyword allows the PostScript filename to be specified.
;               The default is 'idl.ps'
; OUTPUT:
;   IDL.PS    Postscript File
;   All passed variables remain unchanged
; NOTES:
;   none
; HISTORY:
;   18-JUN-90 Version 1 written    E. Deutsch
;   27-FEB-92 Added Color capability and automatic image sizing.  Parameters
;               were changed to keywords.  EWD
;   14-MAY-92 Added STScI specifiic /PARAM=LAYUP needed to override the default
;               margins on the 3C Colormate PS printer.  Help from Greg
;               McLesky to set up the override.  EWD
;   27-MAY-92 Added IDL_SYSDEF support for keywords PSTSCRPT and CLRPSCPT. EWD
;   07-FEB-92 Added /DONTCLOSE and FILENAME= keyword.  EWD
;-

  COMMON colors,r1,g1,b1,red,green,blue
  COMMON SETPS_ComBlk,CurDev,LastPrn,PSfilename

  arg=n_params(0)
  if (arg eq 0) or (arg eq 2) or (arg gt 5) then begin
    print,'Call: IDL> PSOUT,image,[xsize,ysize],[xoffset,yoffset],
    print,'             [autoprint=,/inverse,/color,ctable=]'
    print,'e.g.: IDL> PSOUT,img1,8,8,.25,.25,/color,/auto,/inv'
    return
    endif

  if (n_elements(colr) eq 0) then colr=0
  if (n_elements(dontclose) eq 0) then dontclose=0
  if (n_elements(filename) eq 0) then filename='idl.ps'
  PSfilename=filename

  if (n_elements(image) eq 1) then begin
    wset,image
    image=tvrd(0,0,!d.x_size,!d.y_size)
    endif
  s=size(image)
  if (s(0) ne 2) then begin
    print,'Error[PSOUT]: First parameter IMAGE must be a 2D image.'
    return & endif
  if (arg lt 2) then begin 
    xsize=8. & ysize=8.
    if (s(2) ne s(1)) then ysize=xsize*(1.*s(2)/s(1))
    if (ysize gt 10.5) then begin
      xsize=8.*(10.5/ysize) & ysize=10.5
      endif
    endif
  if (arg lt 4) then begin
    xoffset=(8.5-xsize)/2. & yoffset=.25
    if (colr eq 1) then yoffset=1.
    endif
  if (n_elements(inverse) eq 0) then inverse=0
  if (n_elements(bits) eq 0) then bits=8
  if (n_elements(autoprint) eq 0) then autoprint=0

  if (strupcase(!d.name) ne 'PS') then begin
    CurDev=!d.name & set_plot,'ps'
    device,xoffset=xoffset,yoffset=yoffset,xsize=xsize,ysize=ysize,/inches, $
      /portrait,filename=PSfilename
    print,'Creating '+PSfilename+':  Image Size (', $
      strn(xsize,format='(f5.2)'),'"x', $
      strn(ysize,format='(f5.2)'),'")   Image Offset (', $
      strn(xoffset,format='(f5.2)'),'" & ',strn(yoffset,format='(f5.2)'),'")'
  endif else begin
    device,xoffset=xoffset,yoffset=yoffset,xsize=xsize,ysize=ysize,/inches
    plot,indgen(2),position=[0,0,1,1],xsty=5,ysty=5,/nodata
    print,'Resetting Image Size (', $
      strn(xsize,format='(f5.2)'),'"x', $
      strn(ysize,format='(f5.2)'),'")   Image Offset (', $
      strn(xoffset,format='(f5.2)'),'" & ',strn(yoffset,format='(f5.2)'),'")'
    endelse

  if (colr eq 1) then begin
    if (n_elements(ctable) eq 1) then loadct,ctable
    if (n_elements(ctable) gt 1) then begin
      red=ctable(0,*) & green=ctable(1,*) & blue=ctable(2,*)
      endif
    if (n_elements(ctable) eq 0) then begin
      if (n_elements(red) eq 0) then loadct
      endif
    device,/color,bits=bits
    tvlct,red,green,blue
    if (max(image) gt n_elements(red)-1) then print, $
      'PSOUT Warning: Highest pixel value in image is higher than n_elements(red)'
  endif else device,color=0,bits=bits

  if (inverse eq 0) then tv,image
  if (inverse eq 1) then tv,255b-image

  if (dontclose eq 1) then return

  s=size(autoprint)
  if (s(1) eq 2) then begin
    if (autoprint eq 1) and (colr eq 1) then GetDefVal,autoprint,'CLRPSCPT'
    endif
  psclose,auto=autoprint

  return
end
