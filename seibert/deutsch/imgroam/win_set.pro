pro win_set,handle

  COMMON Win_mgr_dat,win_dat

  s=size(win_dat)
  exist=s(0)

  arg=n_params(0)
  if (exist eq 0) then win_init
  if (arg lt 1) then win=0

  win_dat.Active_win=handle
  wset,handle

  return
end
