pro fpr,imgin,imgout,silent=silent

; This procedure processes the frame for display
; in the IMGroam environment.  It is not useful by itself.

  COMMON ANSI,cr,lf,ESC,up,clrscrn,bell
  COMMON frpc,scmin,scmax,rdtyp,satlim

  if (n_elements(silent) eq 0) then silent=0

  if (silent eq 0) then print,' [FPR] Working...       ',up

  if (strmid(rdtyp,0,2) eq 'TV') then begin
    imgout=imgin
    return
    endif
  imgout=(imgin-scmin)>0
  max=scmax-scmin

  if (strmid(rdtyp,0,4) eq 'ROOT') then begin
    fac=fix(strmid(rdtyp,5,1))
    if (silent eq 0) then print,' [FPR] Working... Taking Root ',strn(fac),'...       ',up
    if (fac eq 2) then imgout=sqrt(temporary(imgout)) $
      else imgout=temporary(imgout)^(1./fac)
    max=max^(1./fac)
    endif

  if (rdtyp eq 'SINE') then begin
    if (silent eq 0) then print,' [FPR] Working... Taking Sine...        ',up
    imgout=sin((temporary(imgout)<max)/(max/(!pi/2)))
    max=1
    endif

  if (rdtyp eq 'LOG') then begin
    if (silent eq 0) then print,' [FPR] Working... Taking Log...       ',up
    imgout=alog(temporary(imgout)+1)
    max=alog(max)
    endif

  if (strmid(rdtyp,0,2) eq 'TV') then imgout=temporary(imgout)<max $
  else begin
    imgout=bytscl(temporary(imgout),0,max,top=!d.n_colors-2<254)
    if (satlim ne 0) then begin
      tmp1=where(imgin ge satlim)
      if (tmp1(0) ne -1) then imgout(tmp1)=!d.n_colors-1<255
      endif
    endelse

  if (silent eq 0) then print,'                                            ',up

  return
end






