pro win_open,win,xsize,ysize,xpos,ypos,title

  COMMON Win_mgr_dat,win_dat

  s=size(win_dat)
  exist=s(0)

  arg=n_params(0)
  if (exist eq 0) then win_init
  if (arg lt 1) then win=0
  if (arg lt 2) then xsize=512
  if (arg lt 3) then ysize=512
  if (arg lt 4) then xoff=0
  if (arg lt 5) then yoff=0
  if (arg lt 6) then title='Window Mgr: '+strn(win)

  window,win,retain=2,title=title,xsize=xsize,ysize=ysize,xpos=xpos,ypos=ypos
  win_dat.Active_win=win
  win_dat.win(win).open=1
  win_dat.win(win).xsize=xsize
  win_dat.win(win).ysize=ysize
  win_dat.win(win).xpos=xpos
  win_dat.win(win).ypos=ypos
  win_dat.win(win).title=title

  return
end
