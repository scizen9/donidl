pro disboth,filename
;+
; No formal header yet.  See the documentation in
;   http//www.astro.washington.edu/deutsch/apoinfo.html
;-

  if (n_params(0) ne 1) then begin
    print,"Call> disboth,rfilename"
    print,"e.g.> disboth,'im0008r'"
    print," Filename must correspond to the RED image"
    return
    endif

  tmp1=strpos(filename,'r')
  if (tmp1 eq -1) then begin
    print," Filename must correspond to the RED image"
    return
    endif

  disread,imgr,hr,filename,/image
  if (n_elements(imgr) lt 1000) then begin
    imgr=intarr(10,10)
    mkhdr,hr,imgr
    sxaddpar,hr,'ir_scmin',0
    sxaddpar,hr,'ir_scmax',1
    endif

  bfile=strmid(filename,0,tmp1)+'b'+strmid(filename,tmp1+1,99)
  disread,imgb,hb,bfile,/image
  if (n_elements(imgb) lt 1000) then begin
    imgb=intarr(10,10)
    mkhdr,hb,imgb
    sxaddpar,hb,'ir_scmin',0
    sxaddpar,hb,'ir_scmax',1
    endif

  window,6,xs=374,ys=271*2
  tp1=(!d.n_colors<256)-1

  tmp1=congrid(imgr,374,271)
  tmp2=bytscl(tmp1,sxpar(hr,'ir_scmin'),sxpar(hr,'ir_scmax'),top=tp1-1)
  sat=where(tmp1 gt 60000)
  if (sat(0) ne -1) then tmp2(sat)=tp1
  tv,tmp2,0,271

  tmp2=bytscl(imgb,sxpar(hb,'ir_scmin'),sxpar(hb,'ir_scmax'),top=tp1-1)
  sat=where(imgb gt 65000)
  if (sat(0) ne -1) then tmp2(sat)=tp1
  tv,tmp2,0,0

  xyouts,.02,.47,/norm,'B',charsize=1.5,charthick=2
  xyouts,.02,.97,/norm,'R',charsize=1.5,charthick=2

  arrow,.05,.01,.01,.01,/norm
  arrow,.05,.01,.05,.04,/norm


  return

end
