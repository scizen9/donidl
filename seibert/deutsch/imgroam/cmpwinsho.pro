pro cmpwinsho,image    ; image to be compressed only necessary when redoing

; This procedure handles various modes of display of the compressed image
; in the IMGroam environment.  It is not useful by itself.

  COMMON fparm,NAXIS1,NAXIS2,xsize,ysize,xcent,ycent,xll,yll,zoom,frtyp
  COMMON ANSI,cr,lf,ESC,up,clrscrn,bell
  COMMON IR_ENVIR,stat,itype
  COMMON cmpwin,COMPWIN,COMPZOOM,COMPIMG,COMPIMG1
  COMMON Windows,FrameWIN,CmpressWIN

  if (COMPWIN eq 3) then begin
    fac=0
    for i=0,5 do begin
      if (NAXIS1/2^i le 300) and (NAXIS2/2^i le 300) and $
        (fac eq 0) then fac=2^i
      endfor
    if (fac eq 0) then begin
      print,'Unable to compress image'
      COMPWIN=0
      return
      endif
    if (CmpressWIN eq -1) then begin
      win_alloc,CmpressWIN
      IR_GetWinPos,'CmpressWIN',x,y
      win_open,CmpressWIN,NAXIS1/fac,NAXIS2/fac,x-NAXIS1/fac,y-NAXIS2/fac,'Compressed Image'
      endif
    COMPZOOM=1/(fac*1.) & COMPWIN=2
    COMPIMG1=congrid(image,NAXIS1/fac,NAXIS2/fac)
    endif

  if (COMPWIN eq 2) then begin
    print,' [CMPWINSHO] Working...',up
    if (stat.AUTOPROC eq 1) then fpr,COMPIMG1,COMPIMG
    s=size(COMPIMG)
    if (s(3) ne 1) then COMPIMG=bytscl(COMPIMG)
    COMPWIN=1
    print,'                      ',up
    endif

  suppress=0
  wset,1
  tv,COMPIMG
  xs=xsize*COMPZOOM/zoom & ys=ysize*COMPZOOM/zoom
  if (ycent+ysize/zoom/2 ge NAXIS2) then suppress=1
  if (xcent+xsize/zoom/2 ge NAXIS1) then suppress=suppress+2
  IMGbord,xs,ys,xcent*COMPZOOM-xs/2,ycent*COMPZOOM-ys/2,255,1,suppress
  wset,0

  return
end
