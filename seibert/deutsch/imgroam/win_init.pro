; This procedure creates the Window Manager structure if it does not already
;   exist.  A call to this function will not disrupt the window structure if it
;   already exists, unless flag is set to 1, in which case, all windows are
;   deleted.
pro win_init,flag

  COMMON Win_mgr_dat,win_dat

  if (n_params(0) lt 1) then flag=0

  s=size(win_dat)
  exist=s(0)

  if (exist eq 0) then begin
    win_dat={Window_Params,Display_type:0,Total_wins:10,Active_win:0, $
      win:replicate({Window_Status,open:0,xsize:0,ysize:0,xpos:0, $
      ypos:0,title:'NONE'},10)}
    endif

  if (flag eq 1) then begin
    for i=0,win_dat.Total_wins-1 do begin
      if (win_dat.win(i).open gt 0) then win_dele,i
      endfor
    endif

  return
end
