pro dismkspflat,inlist,outflat
;+
; No formal header yet.  See the documentation in
;   http//www.astro.washington.edu/deutsch/apoinfo.html
;-


  if (n_params(0) ne 2) then begin
    print,"Call> dismkspflat,inlist,outflat"
    print,"e.g.> dismkspflat,['n1.0005r','n1.0006r','n1.0007r'],'Flat_Rsp'"
    print,"e.g.> dismkspflat,['n1.0005b','n1.0006b','n1.0007b'],'Flat_Bsp'"
    return
    endif


  nfiles=n_elements(inlist)
  for i=0,nfiles-1 do begin
    filename=inlist(i)
    ext=strmid(filename,strlen(filename)-3>0,99)
    if not exist(filename) then begin
      if (ext ne 'hhh') then filename=filename+'.hhh'
      if not exist(filename) then begin
        print,'File "',filename,'" not found. (i=',strn(i),')'
        return
        endif
      endif

    disread,img,h,filename
    if (i eq 0) then begin
      disinfo,img,h,inf
      cube=fltarr(inf.NAXIS1,inf.NAXIS2,n_elements(inlist))
      avval1=avg(img(inf.leftspecedge:inf.rightspecedge, $
        inf.NAXIS2*.25:inf.NAXIS2*.75))
      endif

    avval2=avg(img(inf.leftspecedge:inf.rightspecedge, $
      inf.NAXIS2*.25:inf.NAXIS2*.75))
    print,'Multiplying '+filename+' by ',avval1/avval2
    cube(*,*,i)=img*(avval1/avval2)
    endfor



  print,'Medianing images...'
  fimg=img*0
  for y=0,inf.NAXIS2-1 do begin
    for x=0,inf.NAXIS1-1 do begin
      vals=cube(x,y,*)
      medval=median(vals)
      fimg(x,y)=medval
      endfor
    if (y/100 eq y/100.) then print,'Finished line ',strn(y)
    endfor


  ngood=inf.NAXIS2-inf.topbadrows-inf.botbadrows
  strip=fltarr(ngood)
  for i=0,ngood-1 do strip(i)= $
    median(fimg(inf.leftspecedge:inf.rightspecedge,i+inf.botbadrows))
  strip2=smooth(strip,5)


  strip3=splfit(indgen(n_elements(strip2)),strip2,20)
  good=where(strip3 le 0)
  if (good(0) ne -1) then strip3(good)=strip2(good)
  plot,strip2
  oplot,strip3,color=!d.n_colors-2


  if (inf.grating ne 'high') then begin
;   print,'Ignoring spline fit.  Just using median...'
;   strip3=strip2
    endif


  img2=fimg
  for i=0,ngood-1 do img2(*,i+inf.botbadrows)=strip3(i)
  img3=fimg/img2
  toolow=where(img3 lt .05) & if (toolow(0) ne -1) then img3(toolow)=.05
  toohigh=where(img3 gt 10) & if (toohigh(0) ne -1) then img3(toohigh)=10


  img3=rotate(img3,2)		; rotate back to original orientation
  stwrt,img3,h,/sdas,outflat

  return
end

