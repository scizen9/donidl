pro drawcross,xc,yc,radius,top=top,thick=thick
;+
; NAME:
;   DRAWCROSS
; DESCRIPTION:
;   This procedures draws a graphics cursor-like cross centered at the
;   supplied X,Y data coordinates.  Size and thickness can be supplied.  the
;   /top keyword puts a littel hat on top of the cross as a distinguishing
;   mark from a regular cross.
; INPUT:
;   XC        The X center coordinate
;   YC        The Y center coordinate
;   RADIUS    Radial size of the cross
; OPTIONAL INPUT KEYWORDS:
;   TOP       Setting /TOP puts a "hat" distinguishing mark on top of cross
;   THICK     Sets the thickness of the cross (generally only useful with
;               PostScript (high resolution) output).
; OUTPUT:
;   Draws cross to current graphics channel
; HISTORY:
;   27-MAY-94 Added header and description to old code.  E.Deutsch
;-

  if (n_elements(top) eq 0) then top=0
  if (n_elements(thick) eq 0) then thick=1

  if (!d.name eq 'X') then colr=0
  if (!d.name eq 'PS') then colr=255
  th=thick
  th2=thick*3

  oplot,[xc-radius+1,xc+radius],[yc,yc],color=255-colr,thick=th2
  oplot,[xc,xc],[yc-radius,yc+radius-1],color=255-colr,thick=th2
  oplot,[xc-radius+1,xc+radius],[yc,yc],color=colr,thick=th
  oplot,[xc,xc],[yc-radius,yc+radius-1],color=colr,thick=th

  if (top eq 1) then begin
    oplot,[xc-radius/2,xc+radius/2],[yc+radius-1,yc+radius-1],color=255-colr,thick=th
    oplot,[xc-radius/2,xc+radius/2],[yc+radius,yc+radius],color=colr,thick=th
    oplot,[xc,xc],[yc,yc+radius],color=colr,thick=th
    endif

  return
end
