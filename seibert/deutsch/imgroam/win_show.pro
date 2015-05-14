pro win_show,handle,icon=icon

  COMMON Win_mgr_dat,win_dat

  s=size(win_dat)
  exist=s(0)

  arg=n_params(0)
  if (exist eq 0) then win_init
  if (arg lt 1) then win=0

  s=size(icon)
  if (s(1) eq 0) then icon=0

  if (icon eq 0) then begin
    win_dat.Active_win=handle
    wshow,handle
    win_dat.win(handle).open=1
  endif else begin
    wshow,handle,/icon
    win_dat.win(handle).open=2
    endelse

  return
end
