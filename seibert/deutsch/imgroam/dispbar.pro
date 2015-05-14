pro dispbar,cbllx,cblly,cbx,cby,Border=Border
;+
; NAME:
;   DISPBAR
; DESCRIPTION:
;   This procedure creates and displays a color bar on the screen.
; CALLING SEQUENCE:
;   DISPBAR,cbllx,cblly,cbx,cby,Border=Border
; INPUT:
;   CBLLX     Color Bar Lower Left hand X coordinate
;   BCLLY     Color Bar Lower left hand Y coordinate
;   CBX       Color Bar X length
;   CBY       Color Bar Y height
; OUTPUT:
;   Screen output only.  All passed variables remain unchanged.
; OPTIONAL FLAGS:
;   BORDER    Puts a white (BLACK in PS) border around the color bar.
; HISTORY:
;   30-MAY-90 Version 1 written by Eric W. Deutsch
;   24-AUG-91 Added /Border Keyword    (E. Deutsch)
;-

  if (n_params(0) lt 4) then begin
    print,'Call: IDL> DISPBAR,lwrlft_X,lwrlft_Y,X_length,Y_height,[/Border]'
    print,'e.g.: IDL> DISPBAR,0,0,512,10,/Border'
    return
    endif
  if (n_elements(Border) eq 0) then Border=0

  if (cby eq 0) or (cbx eq 0) then return
  bar=byte(indgen(cbx)*(!d.n_colors*1./cbx<255))
  for i=0,cby-1 do tv,bar,cbllx,cblly+i
  if (Border eq 1) then plots,[cbllx,cbllx+cbx,cbllx+cbx,cbllx,cbllx], $
    [cblly,cblly,cblly+cby-1,cblly+cby-1,cblly],/device

  return
end
