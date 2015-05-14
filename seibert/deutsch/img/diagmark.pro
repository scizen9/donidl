pro diagmark,infile,xc,yc,rad
;+
; NAME:
;   DIAGMARK
; PURPOSE:
;   This procedures finds all stars within radius pixels of x,y and draws
;   a line to it on the output window of diagram.pro
; CALLING SEQEUNCE:
;   diagmark,infile,xc,yc,radius
; INPUT:
;   INFILE  Output file of diagram.pro.  Typically diagram.out.
;   X       X coordinate of star to look for.
;   Y       Y coordinate of star to look for.
;   RADIUS  Search radius about X,Y
;-

  if (n_params(0) lt 3) then begin
    print,'Call> diagmark,infile,xc,yc,radius'
    print,"e.g.> diagmark,'diagram.out',75.4,88.2"
    print,"e.g.> diagmark,'diagram.out',75.4,88.2,3"
    return
    endif

  if (n_elements(rad) eq 0) then rad=2

  openr,1,infile
  lin=''
  while (strmid(lin,0,1) ne '-') do begin
    readf,1,lin
    print,lin
    endwhile

  while not EOF(1) do begin
    readf,1,lin
    reads,lin,n1,n2,n3,x1,y1,b1,u1,UmB
    if (sqrt( (x1-xc)^2 + (y1-yc)^2 ) lt rad) then begin
      print,lin
      plots,/data,[UmB,UmB-2],[b1,b1-2]
      plots,/data,[UmB,UmB],[b1,b1-2]
      goto,BREAK1
      endif
    endwhile

BREAK1:
  close,1
  return

end
