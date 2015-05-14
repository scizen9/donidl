pro imgput,destimg,srcimg,xllf,yllf

;+
; IMGPUT
;
; PURPOSE
;    Paste a small image into a larger one with edge-clipping handling
;
; imgput,destimg,srcimg,xll,yll
;
;-

  sd=size(destimg)
  ss=size(srcimg)
  xll=fix(xllf+.5) & yll=fix(yllf+.5)

; Find the corners of the dest image in which to put the src image
  xd0=xll & xd1=xll+ss(1)-1
  yd0=yll & yd1=yll+ss(2)-1

; Define the default corners of the src image
  xs0=0 & xs1=ss(1)-1
  ys0=0 & ys1=ss(2)-1

; If the desired putting location is off the dest image, then just return
  if (xd1<yd1 lt 0) then return
  if (xd0 ge sd(1)) then return
  if (yd0 ge sd(2)) then return

  if (xd0 lt 0) then begin & xs0=0-xd0 & xd0=0 & endif
  if (yd0 lt 0) then begin & ys0=0-yd0 & yd0=0 & endif

  if (xd1 ge sd(1)) then begin & xs1=xs1-(xd1-sd(1)+1) & xd1=sd(1)-1 & endif
  if (yd1 ge sd(2)) then begin & ys1=ys1-(yd1-sd(2)+1) & yd1=sd(2)-1 & endif

;  print,xd0,xd1,yd0,yd1
;  print,xs0,xs1,ys0,ys1
  destimg(xd0:xd1,yd0:yd1)=srcimg(xs0:xs1,ys0:ys1)

  return

end








