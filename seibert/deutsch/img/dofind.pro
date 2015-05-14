pro dofind,filename,xc,yc,srchrad,rtnbest,silent=silent,first=first

;+
; Procedure: DoFind,filename
;
;  This procedure through the output of DoPHOT in the COMPLETE
; format only for an object of the specified coordinates
;
;  dofind,filename,xc,yc,srchrad
;-

  if (n_params(0) lt 3) then begin
    print,"Call> DoFind,filename,xpos,ypos,[searchradius,rtnbest,/silent,/first]"
    print,"e.g.> DoFind,'filename.objout',210,415"
    return
    endif

  if (n_elements(srchrad) eq 0) then srchrad=5.0
  if (n_elements(silent) eq 0) then silent=0
  if (n_elements(first) eq 0) then first=0

  if not silent then begin
    print,'Obj # Ty     X        Y       Mag     Err      Sky    Maj FWHM  Min FWHM'
    print,'----- --  -------  -------  -------  -----  --------  --------  --------'
    endif

  openr,1,filename
  lin='' & i=0 & rtnbest=fltarr(9)+99
  while not EOF(1) do begin
    readf,1,lin
    x1=float(strmid(lin,10,8)) & y1=float(strmid(lin,19,8))
    if (sqrt((x1-xc)^2+(y1-yc)^2) le srchrad) then begin
      if not silent then print,strmid(lin,0,72)
      typ1=float(strmid(lin,6,3))
      if (typ1 ne 8) then begin
        tmp1=getopt(strmid(lin,0,72))
        if (typ1 lt rtnbest(1)) and (tmp1(4) lt rtnbest(4)) then rtnbest=tmp1
        if (first eq 1) then goto,DONE
        endif
      endif
    endwhile

DONE:
  close,1

  return

end

