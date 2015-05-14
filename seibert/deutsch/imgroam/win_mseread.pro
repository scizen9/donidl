pro win_mseread,window,mx,my,button,Wid_Chk=Wid_Chk,KeyHit=KeyHit

  COMMON Win_mgr_dat,win_dat
  COMMON Widgets,w
  COMMON CPUsaver,LastX,LastY

  arg=n_params(0)
  if (arg lt 1) then return
  if (n_elements(Wid_Chk) eq 0) then Wid_Chk=0
  if (n_elements(LastX) eq 0) then begin & LastX=-1 & LastY=-1 & endif

  window=win_dat.Active_win

DoAgain:
  winfnd=0
  if (win_dat.win(window).open eq 1) then begin
    wset,window
    cursor,mx,my,0,/device
    if (mx ne -1) or (my ne -1) then winfnd=1
    button=!ERR
    endif

  if (n_elements(KeyHit) eq 0) then KeyHit=get_kbrd(0)

  if (winfnd eq 1) and (KeyHit eq '') and (button eq 0) and (LastX eq mx) and (LastY eq my) then begin
    KeyHit=get_kbrd(0)
    wait,1 & goto,DoAgain
    endif

  LastX=mx & LastY=my
  if (winfnd eq 1) then return

  i=0 & flag=0
  while (flag eq 0) do begin
    winfnd=0
    if (i eq win_dat.Total_wins) then begin
      if (Wid_Chk eq 1) then begin
        event=widget_event(w.IR_MnBase,/nowait)
        if (event.ID ne 0) then begin Wid_Chk=event & window=-2 & return & endif
        if (w.GS_MnBase ne 0) then begin
          event=widget_event(w.GS_MnBase,/nowait)
          if (event.ID ne 0) then begin Wid_Chk=event & window=-3 & return & endif
          endif
        endif
      i=0 & wait,1
      endif
    if (win_dat.win(i).open eq 1) then begin
      wset,i
      cursor,mx,my,0,/device
      if (mx ne -1) or (my ne -1) then begin
        winfnd=1
        button=!ERR
        win_dat.Active_win=i & window=i & flag=1
        endif
      endif
    i=i+1
    endwhile

  return
end
