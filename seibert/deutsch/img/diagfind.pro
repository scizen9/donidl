pro diagfind,infile,x=rx,y=ry,first=first
;+
; NAME:
;   DIAGFIND
; PURPOSE:
;   This procedures finds all (or first) star(s) within 0.1 magnitudes of 
;   a cursor click on the CM diagram output of diagram.pro and prints out
;   relevant lines and draws arrow to object.
; CALLING SEQEUNCE:
;   diagfind,diagramoutfile,[x=,y=,/first]
; INPUT:
;   DIAGRAMOUTFILE  Output file of diagram.pro.  Typically diagram.out.
; OPTIONAL KEYWORD:
;   X      Returned X value.
;   Y      Returned Y value.
;   FIRST  Set theis flag to 1 if the program should stop looking after the
;          first match instead of looking for all matches.
;-

  if (n_params(0) ne 1) then begin
    print,'Call> diagfind,infile'
    print,"e.g.> diagfind,'diagram.out'"
    return
    endif

  if (n_elements(first) eq 0) then first=0

  print,'Click on the object to find...'
  rx=0.0 & ry=0.0
  cursor,x,y,/data
  if (!ERR ne 1) then return

  openr,1,infile
  lin=''
  while (strmid(lin,0,1) ne '-') do begin
    readf,1,lin
    print,lin
    endwhile

  while not EOF(1) do begin
    readf,1,lin
    reads,lin,n1,n2,n3,x1,y1,b1,u1,UmB
    if (sqrt( (x-UmB)^2 + (y-b1)^2 ) lt .1) then begin
      print,lin
      if (rx eq 0) then begin & rx=x1 & ry=y1 & endif
      plots,/data,[UmB,UmB-2],[b1,b1-2]
      if (first eq 1) then goto,DONE
      endif
    endwhile

DONE:
  close,1
  return

end
