;+
;
; this is a quick hack main program to shift and combine a set of GRIM
; images using a bad pixel map.  This works really well, but the program
; needs a lot of work.  I don't have any GRIM data exciting enough to
; put time into this at the moment.
;
; "needs work"
;
; good idea:
; 1) run it through this program once
; 2) create a star mask from the final image.
; 3) Then, run it through again masking not just bad pixels, but also the
;    stars to generate a better flat field.
; 4) Then run it through one final time with the better flat field.
;  someday I'll write a program which will do all this... sigh.
;-

  shiftfile='shifts.lis'
  refstarfile='refstars.irsf'
  files=findfile('n12*.hhh')

; ---------------------------------------
  grimread,img,h,files(0),/flag
  s=size(img)
  sumimg=img*0
  expimg=img*0

; ---------------------------------------
  lin='' & v1=0.0 & v2=0.0 & shifts=fltarr(2,100) & i=0
  openr,1,shiftfile
  while not EOF(1) do begin
    readf,1,lin
    if (strn(lin) ne '') then begin
      reads,lin,v1,v2
      shifts(*,i)=[v1,v2]
      i=i+1
      endif
    endwhile
  close,1
  shifts=shifts(*,0:i-1)

; ---------------------------------------
  irsfload,refstarfile,ss
  nrefs=ss.stars-1
  refs=fltarr(2,nrefs)
  refs(0,*)=ss.x(0:nrefs-1)
  refs(1,*)=ss.y(0:nrefs-1)

  close,2 & openw,2,'shifts.out'

; ---------------------------------------
;  for i=0,n_elements(files)-1 do begin
  for i=0,11 do begin
    grimread,img,h,files(i),/flag,/levfit
    med=median(img)
    bad=where(img gt 65000.0)

    img2=img
    img2(bad)=med
    cens=refs*0.0
    skyline,img2,skyv,rmsv

    if (i eq 0) then refskyv=skyv else begin
      img2=img2/(skyv/refskyv)
      img=img/(skyv/refskyv)
      endelse

    tv,congrid(bytscl(img,refskyv-100,refskyv+500,top=!d.n_colors-2),512,512)
    for j=0,nrefs-1 do begin
      plots,[refs(0,j)-shifts(0,i)]*2,[refs(1,j)-shifts(1,i)]*2,psym=4,/dev
      bscentrd,img2,refs(0,j)-shifts(0,i),refs(1,j)-shifts(1,i),xc,yc
      plots,[xc]*2,[yc]*2,psym=6,/device
      print,refs(0,j)-xc,refs(1,j)-yc
      cens(*,j)=[refs(0,j)-xc,refs(1,j)-yc]
      endfor
    xsh=median(cens(0,*))
    ysh=median(cens(1,*))
    print,'X,Y Shift=',xsh,ysh
    printf,2,xsh,ysh

    img2(bad)=0
    msk=img2*0.0+1
    msk(bad)=0
    img3=interpolate(img2,findgen(s(1))-xsh,findgen(s(2))-ysh,/grid,miss=0)
    msk3=interpolate(msk,findgen(s(1))-xsh,findgen(s(2))-ysh,/grid,miss=0)

    tv,congrid(bytscl(img3,refskyv-100,refskyv+500,top=!d.n_colors-2),512,512)
    bscentrd,img3,165,157,xc,yc & print,xc,yc

    sumimg=sumimg+img3
    expimg=expimg+msk3

    endfor

  tmp1=where(expimg eq 0)
  if (tmp1(0) ne -1) then expimg(tmp1)=1

  close,2

  tmp6=sumimg/expimg
  tv,congrid(bytscl(tmp6,refskyv-50,refskyv+150,top=!d.n_colors-2),512,512)
  skyline,tmp6,skyv,rmsv & print,skyv,rmsv
  tmp7=tmp6
  tmp7(where(tmp7 gt skyv+rmsv*4))=0
  tv,congrid(bytscl(tmp7,refskyv-50,refskyv+150,top=!d.n_colors-2),512,512)


end
