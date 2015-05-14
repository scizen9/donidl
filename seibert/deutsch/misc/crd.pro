pro crd,x,y,normal=normal,data=data,device=device
;+
; NAME:
;   CRD
; PURPOSE:
;   This program reads and prints the x,y coordinates of the position of
;   the mouse click in an IDL window.
; CALLING SEQEUNCE:
;   crd
;   crd,[/data,/normal,/device]
;   crd,x,y,[/data,/normal,/device]
;-

  if (n_elements(normal) eq 0) then normal=0
  if (n_elements(data) eq 0) then data=0
  if (n_elements(device) eq 0) then device=0

  if (normal+data+device eq 0) then data=1

  print,'Click mouse button...'
  cursor,x,y,normal=normal,data=data,device=device

  print,x,y

  return

end
