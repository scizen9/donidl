pro IR_AstDisp,x,y,ra,dec,DN,coordtype=coordtype,x2=x2,y2=y2, $
RetCoords=RetCoords, JustDisp=JustDisp

; This procedure handles display of cursor coordinates
; in the IMGroam environment.  It is not useful by itself.

  if (n_elements(coordtype) eq 0) then coordtype='Cursor'
  if (n_elements(JustDisp) eq 0) then JustDisp=0

  COMMON IR_ASTROM,astrom_type,hdr,astr,gsa
  COMMON IR_ENVIR,stat,itype

  x2=x & y2=y
  if (n_elements(RetCoords) eq 0) then RetCoords=0

  if (JustDisp eq 1) then goto,SKIPCALC

  s=size(astrom_type)
  if (s(1) ne 7) then begin & RA=-1. & return & endif
  if (astrom_type eq 'GSSS') then gsssxyad,gsa,x,y,ra,dec
  if (astrom_type eq 'NONE') then begin & ra=0.0D & dec=0.0D & endif
  if (astrom_type ne 'GSSS') and (astrom_type ne 'NONE') then begin
    xy2ad,x2,y2,astr,ra,dec
    endif

SKIPCALC:

  astdisp,x2,y2,ra,dec,DN,Coords=Coords,/silent
  if (RetCoords eq 1) then begin
    RetCoords=Coords
    return
    endif

  COMMON DS_Comm,DS_LftBut,DS_MidBut,DS_RgtBut,DS_CenCrd,DS_CrsCrd,DS_CrdNme

  if (coordtype eq 'Center') then Line=DS_CenCrd else Line=DS_CrsCrd
  if (coordtype ne 'Center') and (coordtype ne 'Cursor') then $
    widget_control,DS_CrdNme,set_value=coordtype

  widget_control,Line,set_value=Coords(1)

  return
end
