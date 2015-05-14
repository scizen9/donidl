pro grimstarmask,img,h

  window,xs=512
  tp=(!d.n_colors<256)-1
  skyline,img(200:300,200:300),s1,r1 & print,s1,r1
  tv,bytscl(img,s1-r1*3,s1+r1*15,top=tp-1)
  openw,1,'starmask.dat'

  flag=1 & istar=0
  while (flag eq 1) do begin
    cursor,/device,x,y
    mseflag=!err
    bscentrd,img,x,y,xc,yc
    maxv=max(img(xc-2:xc+2,yc-2:yc+2))
    peak=maxv-s1
    rad=alog10(peak)*3.0-4>2
    if (istar eq 0) then begin
      xcref=xc & ycref=yc
      endif
    print,xc-xcref,yc-ycref,rad
    printf,1,xc-xcref,yc-ycref,rad
    tvcircle,rad,xc,yc,/device
    if (mseflag eq 4) then flag=0
    istar=istar+1
    endwhile


  close,1

end

