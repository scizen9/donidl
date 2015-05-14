pro starcontour,img,xc,yc

  tmp1=extrac(img,xc-15,yc-15,30,30)
  tmp2=where(tmp1 ne 0)
  if (tmp2(0) ne -1) then tmp1=tmp1(tmp2)
  medn=median(tmp1)
  sig=medn-min(tmp1)

  contour,img,levels=2.0^(findgen(30)/2)*sig+(medn-sig), $
    xr=[xc-15,xc+15],yr=[yc-15,yc+15],xsty=1,ysty=1

  return

end


