pro win_dele,win_to_dele

  COMMON Win_mgr_dat,win_dat

  s=size(win_dat)
  exist=s(0)

  if (exist eq 0) or (n_params(0) lt 1) then begin
    print,'WIN_DELE: Insufficient calling parameters'
    return
    endif

  if (win_dat.win(win_to_dele).open gt 0) then begin
    wdelete,win_to_dele
    win_dat.win(win_to_dele).open=0
    endif

  win_to_dele=-1

  return
end
