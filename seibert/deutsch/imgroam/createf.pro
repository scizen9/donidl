pro createf,imgin,imgout,proc,nozoom

; This procedure handles the extraction of the desired image from the total
; image in the IMGroam environment.  It is not useful by itself.

  COMMON fparm,NAXIS1,NAXIS2,xsize,ysize,xcent,ycent,xll,yll,zoom,frtyp
  COMMON fparm2,xso,yso,xmin,ymin
  COMMON ANSI,cr,lf,ESC,up,clrscrn,bell
  COMMON IR_ENVIR,stat,itype

  if (n_params(0) lt 4) then nozoom=0

  print,' [CREATEF] Working....',up
  fac=fix(zoom) & key1=strn(fac)

  xfac=fix(xso/zoom+.99) & yfac=fix(yso/zoom+.99)
  xsize=xfac*fac & ysize=yfac*fac
  xmin=xcent-xfac/2
  ymin=ycent-yfac/2

  xpiece=0 & ypiece=0 & xoff=0 & yoff=0
  if (xmin lt 0) and (xsize+xmin gt NAXIS1) then begin
    xpiece=1
    xoff=-xmin
    endif
  if (ymin lt 0) and (ysize+ymin gt NAXIS2) then begin
    ypiece=1
    yoff=-ymin
    endif

  if (xpiece eq 0) and (ypiece eq 0) then begin
    if (fac eq 1) then begin
      zimg=extrac(imgin,xmin,ymin,xsize,ysize)
      frtyp='STD'
    endif else begin
      zimg=extrac(imgin,xmin,ymin,xfac,yfac)
      frtyp='EXPAND '+strn(fac)
      endelse
  endif else begin
    s=size(imgin) & typofim=s(3)
    if (fac eq 1) then begin
      zimg=make_array(xsize,ysize,type=typofim)
      zimg(xoff:xsize-1,yoff:ysize-1)= $
        extrac(imgin,xmin+xoff,ymin+yoff,xsize-xoff,ysize-yoff)
      frtyp='STD'
    endif else begin
      zimg=make_array(xfac,yfac,type=typofim)
      zimg(xoff:xfac-1,yoff:yfac-1)= $
        extrac(imgin,xmin+xoff,ymin+yoff,xfac-xoff,yfac-yoff)
      frtyp='EXPAND '+strn(fac)
      endelse
    endelse

  if (proc eq 1) then begin
    tmp=zimg
    fpr,tmp,zimg
    print,' [CREATEF] Working....             ',up
    endif
  if (fac eq 1) or (nozoom eq 1) then begin
    imgout=zimg
    goto,BRK
    endif
  if (stat.ZTYPE eq 2) and (fac gt 1) then begin
    imgout=congrid(zimg,xsize,ysize,/cubic)
    imgout=extrac(imgout,-fac/2,-fac/2,xsize,ysize)		; silly kluge
    endif
  if (stat.ZTYPE eq 1) and (fac gt 1) then imgout=congrid(zimg,xsize,ysize)
  if (stat.ZTYPE eq 0) and (fac gt 1) then begin
    imgout=rebin(zimg,xsize,ysize)
    imgout=extrac(imgout,-fac/2,-fac/2,xsize,ysize)		; silly kluge
    endif


BRK:
  print,'                      ',up
  return
end
