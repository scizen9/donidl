pro irdisp,image,img1,flag

; This procedure is the top level of the image display procedure set
; in the IMGroam environment.  It is not useful by itself.

  COMMON fparm,NAXIS1,NAXIS2,xsize,ysize,xcent,ycent,xll,yll,zoom,frtyp
  COMMON ANSI,cr,lf,ESC,up,clrscrn,bell
  COMMON IR_ENVIR,stat,itype
  COMMON cmpwin,COMPWIN,COMPZOOM,COMPIMG
  COMMON IR_ASTROM,astrom_type,hdr,astr,gsa
  COMMON frpc,scmin,scmax,rdtyp

  if (stat.AUTOD eq 0) then goto,BRK
  if ((flag and 2) eq 2) and (stat.AUTOPROC eq 1) then proc=1 else proc=0

  if ((flag and 1) eq 1) then begin
    if (zoom lt 1.) then zoom=1.
    createf,image,img1,proc
    endif

  s=size(img1) & wset,0
  if (s(3) eq 1) or (rdtyp eq 'TV') then begin
    print,' [TV] Working...',up
    tv,img1,xll,yll
    print,'                   ',up
  endif else begin
    print,' [TVSCL] Working...'
    tvscl,img1,xll,yll
    print,'                    ',up
    endelse

  if (stat.CBAUTOD) then Dispbar,stat.CBLLX,stat.CBLLY,stat.CBX,stat.CBY
  if (stat.AUTOB eq 1) then IMGbord,xsize,ysize,xll,yll,stat.BCOL,stat.BTHK
  if (stat.CBAUTOB eq 1) then IMGbord,stat.CBX,stat.CBY,stat.CBLLX,stat.CBLLY, $
    stat.CBBCOL,stat.CBBTHK
  if (COMPWIN eq 1) then cmpwinsho

  if (stat.NEARR eq 1) and (astrom_type ne 'NONE') then $
    arrows,hdr,40,50,charsize=1.3,arrowlen=2.5,/NotVertex

BRK:
  return
end
