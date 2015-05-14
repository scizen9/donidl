pro win_alloc,next_avail

  COMMON Win_mgr_dat,win_dat

  s=size(win_dat)
  exist=s(0)

  if (n_params(0) lt 1) then begin
    print,'WIN_ALLOC: Insufficient calling parameters'
    return
    endif
  if (exist eq 0) then win_init

  i=0 & flag=0
  while (flag eq 0) do begin
    if (i eq win_dat.Total_wins) then begin
      flag=99
    endif else begin
      if (win_dat.win(i).open eq 0) then flag=1
      endelse
    i=i+1
    endwhile

  if (flag eq 99) then begin
    print,'WIN_ALLOC: All windows are used up.  Unable to allocate'
    return
    endif

  next_avail=i-1

  return
end
