pro quickskysub,img,outimg,flatarr,gran=gran,badmask=badmask

  if (n_params(0) lt 2) then begin
    print,'IDL> quickskysub,img,outimg,[flatarr,gran=]
    return
    endif

  s=size(img)
  flatarr=img*0.0-999

  if (n_elements(gran) eq 0) then gran=30
  gran=fix(gran)

  for y=0,s(2)/gran do begin
    for x=0,s(1)/gran do begin
      tmp1=img(x*gran:(x*gran+gran-1)<(s(1)-1), $
        y*gran:(y*gran+gran-1)<(s(2)-1))
      if (n_elements(badmask) gt 0) then begin
        btmp1=badmask(x*gran:(x*gran+gran-1)<(s(1)-1), $
          y*gran:(y*gran+gran-1)<(s(2)-1))
        good=where(btmp1 ne -999)
        if (good(0) eq -1) then tmp1=0 else tmp1=tmp1(good)
        endif
      if (n_elements(tmp1) gt 4) then begin
        tmp2=sort(tmp1)
        nval=n_elements(tmp2)/100.0*3
        val=tmp1(tmp2(nval))
        flatarr(x*gran:(x*gran+gran-1)<(s(1)-1),y*gran:(y*gran+gran-1)<(s(2)-1))=val
        endif
      endfor
    endfor

  for y=0,s(2)/gran do begin
    for x=0,s(1)/gran do begin
      xc=x*gran+gran/2 & yc=y*gran+gran/2
      val=flatarr(xc<(s(1)-1),yc<(s(2)-1))
      r=1
      if (val eq -999) then begin
TRYAGAIN:
print,'poop'
        tmp1=extrac(flatarr,xc-r*gran,yc-r*gran,r*2*gran,r*2*gran)
        good=where((tmp1 ne -999) and (tmp1 ne 0))
        if (good(0) eq -1) then begin
          r=r+1
          goto,TRYAGAIN
          endif
        val=avg(tmp1(good))
        flatarr(x*gran:(x*gran+gran-1)<(s(1)-1),y*gran:(y*gran+gran-1)<(s(2)-1))=val
        endif
      endfor
    endfor


  flatarr=smooth(flatarr,5)
  smflatarr=congrid(flatarr,s(1)/10,s(2)/10)
  smflatarr=smooth(smflatarr,3)
  s2=size(smflatarr)
  flatarr=rebin(smflatarr,s2(1)*10,s2(2)*10)
  flatarr=congrid(flatarr,s(1),s(2))
  outimg=img-flatarr

  return

end





