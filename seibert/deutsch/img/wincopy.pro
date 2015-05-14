pro wincopy,sourcewin,targwin

  wset,sourcewin
  xs=!d.x_size
  ys=!d.y_size

  tflag=1
  if (!d.n_colors le 256) then tflag=0
  tmp1=tvrd(true=tflag)

  window,targwin,xs=xs,ys=ys
  tv,tmp1,true=tflag

  wset,sourcewin
  return

end
