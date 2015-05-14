pro setps,xsize,ysize,xoffset,yoffset,landscape=landscape,silent=silent, $
  top=top,filename=filename
;+
; NAME:
;   SETPS
;
; PURPOSE:
;   This procedure sets up the output channel for POSTSCRIPT output.
;   Designed to be used in conjunction with PSCLOSE.
;
; CALLING SEQEUNCE:
;   SETPS,[xsize,ysize],[xoffset,yoffset],[/landscape,/silent]
;
; OPTIONAL INPUT:
;   XSIZE     Width of the output in inches
;   YSIZE     Height of the output in inches
;   XOFFSET   Lwr-lft corner of the output starts XOFFSET inches from the
;               left edge of paper.
;   YOFFSET   Lwr-lft corner of the output starts YOFFSET inches from the
;               bottom edge of the paper.
;
; OPTIONAL KEYWORDS:
;   LANDSCAPE Specifies that the output is to be in Landscape (sideways) mode.
;   SILENT    Indicates that none of the informational messages are printed.
;   TOP       This keyword generally is only used when no other parameters
;               are specified.  It moves the portrait 7x7 inch plot to
;               the top of the page instead of the default bottom.  This is
;               especially useful for putting plots so that there is room
;               for the caption at the bottom.
;   FILENAME  This keyword specifies the name of the PostScript file.  The
;               default value is 'idl.ps'
;
; HISTORY:
;   11-JUL-90 Version 1 written by Eric Deutsch
;   26-MAY-92 Proper header and other minor modifications. EWD
;   12-NOV-92 Added /TOP keyword.  EWD
;   06-FEB-92 Added FILENAME= keyword.  EK & EWD
;-

  COMMON SETPS_ComBlk,CurDev,LastPrn,PSfilename

  arg=n_params(0)
  if (arg eq 1) or (arg eq 3) then begin
    print,'Call: IDL> SETPS,[xsize,ysize],[xoffset,yoffset],[/landscape]'
    print,'e.g.: IDL> SETPS'
    print,'e.g.: IDL> SETPS,7,7,.75,1    (these are the defaults)'
    return
    endif

  if (n_elements(silent) eq 0) then silent=0
  if (n_elements(landscape) eq 0) then landscape=0
  if (n_elements(top) eq 0) then top=0
  if (n_elements(filename) eq 0) then filename='idl.ps'
  if (landscape eq 0) then begin
    if (arg lt 2) then begin xsize=7 & ysize=7 & endif
    if (arg lt 4) then begin xoffset=.75
      if (top eq 0) then yoffset=1 else yoffset=10-ysize
      endif
    endif
  if (landscape eq 1) then begin
    if (arg lt 2) then begin xsize=10 & ysize=7.5 & endif
    if (arg lt 4) then begin xoffset=.25 & yoffset=.5 & endif
    tmp=yoffset & yoffset=xoffset & xoffset=tmp
    yoffset=11-yoffset
    endif

  if (!d.name ne 'PS') then CurDev=!d.name
  set_plot,'ps'

  if (landscape eq 0) then $
    device,xoffset=xoffset,yoffset=yoffset,xsize=xsize,ysize=ysize,/inches, $
      /portrait,filename=filename
  if (landscape eq 1) then $
    device,xoffset=xoffset,yoffset=yoffset,xsize=xsize,ysize=ysize,/inches, $
      /landscape,filename=filename

  PSfilename=filename

  if silent then return

  print,"Graphics Output: PostScript file '"+PSfilename+"'.  Use PSCLOSE to finish output."
  print,' Settings(in.): (Xsize,Ysize,LL X,LL Y): ', $
    vect([xsize,ysize,xoffset,yoffset])

  return
end
