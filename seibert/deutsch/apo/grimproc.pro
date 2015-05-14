;+
;
; this is a quick hack main program to dark-sub and flat and set up a shifts
; file to run grimcombine.pro.
;
; "needs work"
;-

files=findfile('n12*.hhh')
openw,3,'shifts.lis'

for i=0,n_elements(files)-1 do begin
  grimread,img,h,files(i),/levfit

  scmin=sxpar(h,'ir_scmin')
  scmax=sxpar(h,'ir_scmax')
  lgmax=(scmax-scmin)*8+scmin
  tp1=!d.n_colors-1
  tv,congrid(imscl(img-scmin,0,lgmax-scmin,scmax-scmin,top=tp1-1),512,512)

  print,'Click on ref star'
  cursor,x,y,/device
  if (i eq 0) then begin & refx=x/2.0 & refy=y/2.0 & endif
  print,refx-x/2.0,refy-y/2.0
  printf,3,refx-x/2.0,refy-y/2.0

  stwrt,img,h,'pr'+files(i),/sdas
  endfor

  close,3

end
