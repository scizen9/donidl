pro WhiteSky,dummy,alw=alw,reserve=reserve
;+
; NAME:
;   WHITESKY
; PURPOSE:
;   Changes current color table to black on white.
; CALLING SEQEUNCE:
;   WhiteSky
; OPTIONAL INPUT KEYWORDS:
;   ALW       If set, the usual linear white to black color table is replaced
;               by an odd exponential function which approximates an
;               Apple Laster Writer density color table (i.e. if you use
;               /ALW the stretch on the screen will look more like an ALW)
;   RESERVE   string which specifies if some colors are to be reserved at
;             the top of bottom of the color table.  Current options are:
;             'RED'
; OUTPUT:
;   Changes current color table and updates color table COMMON block.
; HISTORY:
;   17-NOV-92 Header added to old routine  (E. Deutsch)
;   11-JAN-93 Changed name from NEG to WHITESKY  (E. Deutsch)
;   04-APR-94 Added proper handling when !d.n_colors is low.  (E. Deutsch)
;   29-MAY-94 Added /ALW keyword and handling.  (E. Deutsch)
;   29-APR-97 Added limit of !d.n_colors=256.  (E. Deutsch)
;-

  COMMON colors,r1,g1,b1,red,green,blue

  if (n_elements(alw) eq 0) then alw=0
  if (n_elements(plotflag) eq 0) then plotflag=0

  top=!d.n_colors*1.0<256

  x=findgen(top)*255./(top-1)
  if alw then begin
    tmp=(1.08-exp(x/99)/12>0)^(1/2d)*255
    red=fix(tmp)
  endif else red=255-fix(x)
  green=red & blue=red

  if (n_elements(reserve) ne 0) then begin
    if (strn(reserve) eq 'RED') then begin
      red(top-1)=255 & green(top-1)=0 & blue(top-1)=0
      endif
    endif

  r1=red & g1=r1 & b1=r1

  tvlct,red,green,blue

  return

end
