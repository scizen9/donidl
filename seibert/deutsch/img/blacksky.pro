pro BlackSky,dummy
;+
; NAME:
;   BLACKSKY
; PURPOSE:
;   Changes current color table to white on black.
; CALLING SEQEUNCE:
;   BlackSky
; INPUT:
;   none
; OUTPUT:
;   Changes current color table and updates color table COMMON block.
; HISTORY:
;   17-NOV-92 Header added to old routine  (E. Deutsch)
;   11-JAN-93 Changed name from POS to BLACKSKY  (E. Deutsch)
;   04-APR-94 Added proper handling when !d.n_colors is low.  (E. Deutsch)
;   29-APR-97 Added limit of !d.n_colors=256.  (E. Deutsch)
;-

  COMMON colors,r1,g1,b1,red,green,blue

  top=!d.n_colors*1.0<256

  red=fix(indgen(top)*255./(top-1)) & green=red & blue=red
  r1=red & g1=r1 & b1=r1

  tvlct,red,green,blue

  return

end
