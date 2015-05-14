pro GS_Roam,ls,ss,Starno

; This procedure is the analog of IR_Roam in the Astrometry package
; in the IMGroam environment.  It is not useful by itself.

  COMMON PLPAR,xmn,ymn,xmx,ymx,GuideStWIN,ssptr
  COMMON ANSI,cr,lf,ESC,up,clrscrn,bell
  COMMON Widgets,w

  Starno=-1
  cursor,mx,my,0,/data
  button=!ERR
  if (mx eq -1) and (my eq -1) then return

  if (button eq 0) then begin
    IR_astdisp,0,0,mx,my,0.,/JustDisp
    return
    endif

  if (button eq 4) then begin
    tmpmenu=strarr(6)
    tmpmenu(0)='Reset zoom to original size'
    tmpmenu(1)="Set to 60' field"
    tmpmenu(2)="Set to 18' field"
    tmpmenu(3)="Set to 8' field"
    tmpmenu(4)='Set field edge to cursor DEC'
    tmpmenu(5)='Cancel'
    choice=wmenu(tmpmenu)
    if (choice eq 5) then return
    if (choice eq 0) then begin
      GS_FOVplot,ls,ss,1
      GS_Disp,ss & Starno=0
      return
      endif
    zz=0
    if (choice eq 1) then zz=60/60.
    if (choice eq 2) then zz=18/60.
    if (choice eq 3) then zz=8/60.
    xrang=xmx-xmn & yrang=ymx-ymn
    xcent=(xmn+xrang/2) & ycent=(ymn+yrang/2)
    if (zz ne 0) then ysz=zz/2.
    if (zz eq 0) then ysz=abs(ycent-my)
    factr=ysz/yrang
    ymn=ycent-yrang*factr & ymx=ycent+yrang*factr
    xmn=xcent-xrang*factr & xmx=xcent+xrang*factr
    GS_FOVplot,ls,ss,0
    GS_Disp,ss & Starno=0
    return
    endif

  if (button ne 2) then return

  xrang=xmx-xmn & yrang=ymx-ymn
  xcent=(xmn+xrang/2) & ycent=(ymn+yrang/2)
  rangeratio=xrang/yrang
  starno=-1 & dist=999

  for i=0,n_elements(ls)-1 do begin
    if (abs(ls(i).ra-mx) le abs(xrang/64.)) and $
      (abs(ls(i).dec-my) le abs(yrang/64.)) then begin
      curdis=((abs(ls(i).ra-mx))^2+((abs(ls(i).dec-my))*rangeratio)^2)^(1/2)
      if (curdis lt dist) then begin
        dist=curdis & idno=ls(i).id & starno=i
        endif
      endif
    endfor

  if (starno eq -1) then begin
    xmessage,'No star found here.  Please try again.',['OK'],tmp
    return
    endif

  for i=0,ss.stars-1 do begin
    if (ss.id(i) eq idno) then begin
      xmessage,'This star has already been selected!!  Delete from list?', $
        ['DELETE','IGNORE'],type
      if (type eq 0) then begin
        while (i lt ss.stars-1) do begin
          ss.id(i)=ss.id(i+1) & ss.ra(i)=ss.ra(i+1)
          ss.dec(i)=ss.dec(i+1) & ss.mag(i)=ss.mag(i+1)
          ss.x(i)=ss.x(i+1) & ss.y(i)=ss.y(i+1)
          i=i+1
          endwhile
        ss.stars=ss.stars-1 & starno=-1000-starno
        GS_Disp,ss
        endif
      if (type gt 0) then starno=-1
      return
      endif
    endfor

  newflag=1
  if (ss.stars gt 0) then begin
    if (ssptr ne ss.stars) then begin
      ss.stars=ss.stars-1
      newflag=0
    endif else begin
      if (ss.ra(ss.stars-1) eq 0.) and (ss.dec(ss.stars-1) eq 0.) then begin
        ss.stars=ss.stars-1 & ssptr=ssptr-1
        newflag=0
        endif
      endelse
    endif

  ss.id(ssptr)=ls(starno).id & ss.ra(ssptr)=ls(starno).ra
  ss.dec(ssptr)=ls(starno).dec & ss.mag(ssptr)=ls(starno).mag
  if (newflag eq 1) then begin
    ss.x(ssptr)=0. & ss.y(ssptr)=0.
    endif    
  GS_FOVplot,ls,ss,2,ss.stars
  ss.stars=ss.stars+1 & ssptr=ssptr+1
  IR_Widgets2,w,'GS_UPDATE',Ret_Val,ss
  IR_Widgets2,w,'SetSelStr',ssptr,ss.stars
  GS_Disp,ss,/NoList
  wait,1

  return
end
