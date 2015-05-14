pro UIT_sky_value,img,skyv,maxv

;+
;  Call: UIT_sky_value,img,skyv,maxv
;   This procedure is much like DJL's SKY_VALUE is calling sequence but
;   but it optimaized to return nice display values for UIt images.
;-


  if (n_params(0) lt 2) then begin
    print,'Call> UIT_sky_value,image_array,sky_value,max_value'
    print,'e.g.> UIT_sky_value,img,skyv,maxv'
    return
    endif

  av=fltarr(13) & a=intarr(13)
  bkgcnt=200.

  for i=0,60,5 do begin
    x=1024+cos(i/10.)*700
    y=1024+sin(i/10.)*700
    tmp=extrac(img,x,y,50,50)
    ww=histogram(tmp,min=0,max=30) & j=20
    while (ww(j) lt bkgcnt) and (j gt 0) do j=j-1
    av(i/5)=j+(ww(j)-bkgcnt)/(ww(j)*1.-ww(j+1))
    a(i/5)=(av(i/5) mod fix(av(i/5))) *10
    if (a(i/5) ge 5) then av(i/5)=av(i/5)+1
    endfor

  wtmp=where(av lt 6) & if (!err gt 0) then av(wtmp)=100
  skyv=fix(min(av))
  maxv=100.

  return
end
