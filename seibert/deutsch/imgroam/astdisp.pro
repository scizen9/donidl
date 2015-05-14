pro AstDisp,x,y,ra,dec,DN,Coords=Coords,silent=silent
;+
; NAME:
;   ASTDISP
; DESCRIPTION:
;   This procedure prints of the X,Y,RA,DEC,DN in a standard format.  X,Y must
;   be supplied.  RA,DEC may also be supplied, and a DN may also be supplied.
;   With use of the Coords= keyword, a string containing the formatted data
;   can be returned in addition or instead (with /silent) of printing.
; INPUT:
;   X         The X pixel coordinate
;   Y         The Y pixel coordinate
; OPTIONAL INPUT:
;   RA        Right Ascention
;   DEC       DEClination  (if RA is supplied, DEC must also be supplied)
;   DN        Data Number
; OPTIONAL INPUT KEYWORDS:
;   SILENT    Prevents printing.  Only useful when used with Coords=
; OUTPUT:
;   Printed positions in both degrees and sexigesimal format
;   All passed variables remain unchanged
; OPTIONAL KEYWORD OUTPUT:
;   COORDS    Returns the formatted coordinates in a string
; HISTORY:
;   10-AUG-90 Version 1 written by Eric W. Deutsch
;   20-AUG-91 Converted to standard header.  Vectorized Code.  E. Deutsch
;   20-NOV-92 Added Coords= and /silent.  E.Deutsch
;-

  arg=n_params(0)
  if (arg lt 2) then begin
    print,'Call: IDL> AstDisp,x_pixel,y_pixel,[RA,DEC],[DN],[/silent,coords=]'
    print,'e.g.: IDL> AstDisp,x,y,ra,dec'
    return
    endif
  if (arg eq 3) then dec=0.0D

  if (n_elements(silent) eq 0) then silent=0

  hdr='    X        Y'
  fmt='$(f8.2,1x,f8.2'
  if (arg le 2) then begin & type=0 & goto,PRN & endif

  hdr=hdr+'         RA       DEC           RA           DEC'
  fmt=fmt+',2x,F9.4,1x,F9.4,2x,A'
  if (arg le 4) then begin & type=1 & goto,PRN & endif

  hdr=hdr+'           DN'
  fmt=fmt+',2x,f10.3'
  type=2

PRN:
  if not silent then print,hdr
  Coords=strarr(n_elements(x)+1)
  Coords(0)=hdr
  for i=0,n_elements(x)-1 do begin
    if (type eq 0) then out=string(format=fmt+')',x(i),y(i),/print)
    if (type eq 1) then out=string(format=fmt+')',x(i),y(i),ra(i),dec(i), $
      adstring(ra(i),dec(i),2),/print)
    if (type eq 2) then out=string(format=fmt+')',x(i),y(i),ra(i),dec(i), $
      adstring(ra(i),dec(i),2),DN(i),/print)
    if not silent then print,out
    Coords(i+1)=out
    endfor

  return
end
