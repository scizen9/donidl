pro gdrmkcals,size=imsize
;+
; NAME:
;	GDRMKCALS
;
; PURPOSE:
;	Generate guider calibration frames Zero and Dark from files
;	zero[1-7] and dark[1-7].
;
; CATEGORY:
;	APO software
;
; CALLING SEQUENCE:
;	gdrmkcals
;	gdrmkcals,[size=]
;
; INPUTS:
;	none
;
; OPTIONAL INPUT KEYWORDS:
;	size:	If the images are not 512x512 (the default 1x1 bin of the
;		512^2 chip), then you must specify the size of the image
;		with this keyword.
;
; OUTPUTS:
;	files Zero.hhh & .hhd  and  Dark.hhh & .hhd
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Source images are loaded and combined.
;
; EXAMPLE:
;	gdrmkcals
;
; MODIFICATION HISTORY:
;	09/22/95 Written by E. Deutsch
;	11/20/95 Modified for 512x512 camera default by E. Deutsch
;
;-

  if (n_params(0) ne 0) then begin
    print,'Call> gdrmkcals,size=]'
    print,"e.g.> gdrmkcals"
    return
    endif

  if (n_elements(imsize) eq 0) then imsize=512

  cube=fltarr(imsize,imsize,7)

  for i=0,6 do begin
    filename='ZERO'+strn(i+1)
    if (not exist(filename)) then begin
      print,"File '"+filename+"' not found.  Aborting calibration sequence."
      return
      endif
    print,'Reading '+filename+'...'
    gdrread,img,h,filename,size=imsize
    cube(*,*,i)=img
    endfor

  tmp1=img(30:80,imsize-100:imsize-30)
  typavg=avg(tmp1)
  typmed=median(tmp1)
  typsig=stdev(tmp1)
  print,'Typical Zero average, median, stdev: ',typavg,typmed,typsig

  Zero=img*0
  stats=fltarr(8)

  for y=0,imsize-1 do begin
    for x=0,imsize-1 do begin
      strip=cube(x,y,*)
      good=where(strip lt median(strip)+typsig*3)
      stats(n_elements(good))=stats(n_elements(good))+1
      if (n_elements(good) lt 5) then good=where(strip ge 0)
      Zero(x,y)=avg(strip(good))
      endfor
    if (y/100 eq y/100.0) then $
      print,strn(y*100.0/imsize,format='(i5)')+'% done'
    endfor

  print,'Percent of pixels using 0-7 values:'
  print,stats/imsize^2.0*100,format='(8f8.1)'

  print,'Writing Zero.hhh & .hhd...'
  stwrt,Zero,h,'Zero',/sdas

; ----------------------------------------------------------------------------

  for i=0,6 do begin
    filename='DARK'+strn(i+1)
    if (not exist(filename)) then begin
      print,"File '"+filename+"' not found.  Aborting calibration sequence."
      return
      endif
    print,'Reading '+filename+'...'
    gdrread,img,h,filename,size=imsize
    cube(*,*,i)=img-Zero
    endfor

  tmp1=img(30:80,imsize-100:imsize-30)
  typavg=avg(tmp1)
  typmed=median(tmp1)
  typsig=stdev(tmp1)
  print,'Typical Dark average, median, stdev: ',typavg,typmed,typsig

  Dark=img*0
  stats=fltarr(8)

  for y=0,imsize-1 do begin
    for x=0,imsize-1 do begin
      strip=cube(x,y,*)
      good=where(strip lt median(strip)+typsig*3)
      stats(n_elements(good))=stats(n_elements(good))+1
      if (n_elements(good) lt 5) then good=where(strip ge 0)
      Dark(x,y)=avg(strip(good))
      endfor
    if (y/100 eq y/100.0) then $
      print,strn(y*100.0/imsize,format='(i5)')+'% done'
    endfor

  print,'Percent of pixels using 0-7 values:'
  print,stats/imsize^2.0*100,format='(8f8.1)'

  print,'Standard deviation of subsection of img: ',$
    stdev(img(30:80,imsize/2:imsize/2+50))
  img2=img-Zero
  print,'Standard deviation of subsection of (img-Zero): ',$
    stdev(img2(30:80,imsize/2:imsize/2+50))
  img2=img-Dark
  print,'Standard deviation of subsection of (img-Dark): ',$
    stdev(img2(30:80,imsize/2:imsize/2+50))
  img2=img-Zero-Dark
  print,'Standard deviation of subsection of (img-Zero-Dark): ',$
    stdev(img2(30:80,imsize/2:imsize/2+50))

  tmp1=Dark(30:80,imsize/2:imsize/2+50)
  print,'Min,Max,avg,median Dark counts: ',$
    min(tmp1),max(tmp1),avg(tmp1),median(tmp1)

  print,'Writing Dark.hhh & .hhd...'
  stwrt,Dark,h,'Dark',/sdas

  return
end
