pro starchck,image,x,y,ratio,radius,skyv,rms,test=test,crim=crim,sens=sens, $
  qphot=qflag,noclean=noclean

  if (n_elements(test) eq 0) then test=0

  COMMON strck2,dist

  xx=fix(x) & yy=fix(y)
  tmp=extrac(image,xx-10,yy-10,20,20)
  xc=10 & yc=10
  for i=0,6 do begin
    ttmp=extrac(tmp,xc-2,yc-2,5,5) & mval=where(ttmp eq max(ttmp))
    mval=mval(0)
    if (mval eq 12) then goto,READY
    yc=yc+mval/5-2 & xc=xc+fix((mval/5.-mval/5)*5+.1)-2
    endfor
  print,'Unable to locate center... Trying at orginal position...'
  xc=10 & yc=10

READY:
;  print,'Center of ',vect([xc,yc]),' found after ',strn(i),' tries..'
  tmp=extrac(image,xx-20+xc,yy-20+yc,20,20)

  if (qflag ge 1) then qphot,image,xx-10+xc,yy-10+yc,qflag
  if (noclean eq 1) then begin
    ratio=.001 & what='Star'
    return
    endif

  ttt1=where(tmp lt -10)
  if (ttt1(0) ne -1) then begin
    tmp(ttt1)=0
;    print,'Suspiciously low pixels found near: ',vect([xx-10+xc,yy-10+yc])
    endif

  skyline,tmp,skyv,rms
;  t2=tmp(where(tmp ne 0))
;  t1=t2(sort(t2))
;  skyv=t1(10)

goto,OLDMETHOD


; ****** new test for tiny single pixel CR's ************
  what='????'
  a1=fltarr(4) & a1(0)=tmp(10,10)
  for i=1,3 do a1(i)=(total(tmp(10-i:10+i,10-i:10+i))-a1(i-1)*(8*(i-1)))/(8*i)
  tnum=a1(0)/stdev(a1(1:3))

  flat=(a1(0)-a1(1))/(a1(1)-min(a1(1:3)))
  is_a_CR=0
  if (tnum gt 13) or (flat gt 2) then is_a_CR=1
  if (tnum gt 10) and (flat gt 1.5) then is_a_CR=1

  if (is_a_CR eq 1) then begin & ratio=1 & what='CRay' & endif $
  else begin & ratio=.001 & what='Star' & endelse


  printf,5, $
    what,strn(vect(fix([xx+xc-10,yy+yc-10])),length=9), $
    '  a1: ',vect(a1), $
    '  Tnum: ',vect([tnum,flat])


  return

OLDMETHOD:

  if (n_elements(dist) lt 350) then begin
    print,n_elements(dist),'     Creating dist....'
    xdist=intarr(20,20)
    for i=0,19 do xdist(*,i)=indgen(20)
    ydist=rotate(xdist,1)
    dist=sqrt(abs(xdist-xc)^2+abs(ydist-yc)^2)
    endif

  prof=fltarr(25) & radius=-99 & prev=-99 & maxval=-99
  for i=1,5 do begin
    mask=where(dist le i)
    if (i eq 3) then maxval=max(tmp(mask))
    prof(i)=total(tmp(mask))-skyv*n_elements(mask)
    if (i gt 3) and (prof(i) ge prev) and (radius eq -99) then begin
      radius=i-1
      goto,SKIP & endif
    prev=prof(i)
    endfor
  if (radius eq -99) then begin
;    print,'Object too big... Assuming a Star....'
;    radius=24 & ratio=.001 & goto,SKIP2 & endif
    radius=5 & endif

SKIP:
  ratio=maxval/prof(radius)
  if (ratio lt 0) then ratio=9.9999


; Further safety net check for 'obvious' CR's
  if (ratio le sens) then begin
    x5=xx+xc-10 & y5=yy+yc-10
    tmp5=extrac(crim,x5-1,y5-1,3,3)
    ttt5=where(tmp5 eq 0)
    if (n_elements(ttt5) gt 2) then ratio=9.500
    endif

SKIP2:

  if (test eq 0) then return

  if (ratio gt .25) then what='CRay' else what='Star'
  printf,5, $
    what,strn(vect(fix([xx+xc-10,yy+yc-10])),length=9), $
    '  R: ',strn(radius), $
;    '   Prfl: ',strn(vect(fix(prof(1:5))),length=21), $
    '   Max: ',strn(maxval), $
    '  Vol: ',strn(prof(radius)), $
    '   Ratio: ',strn(ratio,format='(f6.4)'), $
    '   Sky: ',strn(skyv,format='(f8.2)')

; if (xx+xc-10 eq 172) and (yy+yc-10 eq 68) then print,qq


  return

end
