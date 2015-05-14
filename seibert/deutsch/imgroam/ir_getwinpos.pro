pro IR_GetWinPos,WinID,x,y

; This procedure contains a half-hearted attempt at having standard window
; positions in the IMGroam environment.  It is not useful by itself.

  device,get_screen_size=scrnsize
  x=0 & y=0

  if (WinID eq 'FrameWIN') then begin
    x=scrnsize(0)-530
    y=scrnsize(1)-512
    endif

  if (WinID eq 'CmpressWIN') then begin
    x=scrnsize(0)-530-18
    y=scrnsize(1)
    endif

  if (WinID eq 'ButtnWid') then begin
    x=0
    y=scrnsize(1)-500
    endif

  if (WinID eq 'CoordWid') then begin
    x=0
    y=scrnsize(1)-250
    endif

  return
end
