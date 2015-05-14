pro disfindspec,img,h,plot=plot
;+
; No formal header yet.  See the documentation in
;   http//www.astro.washington.edu/deutsch/apoinfo.html
;-

  if (n_elements(plot) eq 0) then plot=0

  s=size(img)
  if (s(2) eq 820) then begin
    x1=370 & x2=469
    y1=425 & y2=695
    endif
  if (s(2) eq 522) then begin
    x1=260 & x2=359
    y1=180 & y2=429
    endif

  img2=img
  for i=y1,y2 do img2(*,i)=img2(*,i)-median(img(x1:x2,i))

  strips=fltarr(100,5)

  for x=x1,x2 do begin
    for y=0,4 do begin
      strips(x-x1,y)=median(img2(x,y*50+y1:y*50+y1+49))
      endfor
    endfor

  xarr=indgen(100)+x1
  print,'Possible spectrum X positions (and spectrum amplitudes)'
  for y=0,4 do begin
    fit=gaussfit(xarr,median(strips(*,y),3),coeff)
    print,coeff(1),coeff(0)
    if (plot eq 1) then begin
      plot,xarr,median(strips(*,y),3)
      oplot,xarr,fit
      key=get_kbrd(1)
      endif
    endfor

  return

end
