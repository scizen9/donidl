pro IR_Roam,awin,mx,my,button,image,img1,var10

; This procedure handles much of the interactive movement stuff
; in the IMGroam environment.  It is not useful by itself.

  COMMON fparm,NAXIS1,NAXIS2,xsize,ysize,xcent,ycent,xll,yll,zoom,frtyp
  COMMON fparm2,xso,yso,xmin,ymin
  COMMON cmpwin,COMPWIN,COMPZOOM,COMPIMG
  COMMON ANSI,cr,lf,ESC,up,clrscrn,bell
  COMMON IR_ENVIR,stat,itype
  COMMON frpc,scmin,scmax,rdtyp
  COMMON Windows,FrameWIN,CmpressWIN
  COMMON IR_ASTROM,astrom_type,hdr,astr,gsa
  COMMON Widgets,w

  if (button ge 100) then begin
    button=button-100 & goto,SKIP1 & endif

  if (awin eq CmpressWIN) then begin
    mx=mx/COMPZOOM
    my=my/COMPZOOM
    endif
  if (awin eq FrameWIN) then begin
    mx=xmin-xll-(zoom-1)/(2*zoom)+((mx-1)/zoom)
    my=ymin-yll-(zoom-1)/(2*zoom)+((my-1)/zoom)
    endif

  wait,.5

SKIP1:
  if (button eq 0) then begin
    if (mx lt -.5) or (my lt -.5) or (mx ge NAXIS1-.5) or (my ge NAXIS2-.5) then $
      DN=0. else DN=image(fix(mx+.5),fix(my+.5))
    IR_astdisp,mx,my,ra,dec,DN
    return
    endif

  if (button eq 1) then begin
    xcent=fix(mx+.5) & ycent=fix(my+.5)
    print,cr & irdisp,image,img1,3 & print,up,up,up
    win_set,FrameWIN & awin=FrameWIN
    tvcrs,(xcent-xmin+xll+(zoom-1)/(2*zoom))*zoom+1, $
      (ycent-ymin+yll+(zoom-1)/(2*zoom))*zoom+1,/device
    return
    endif

  if (button eq 2) and (awin eq FrameWIN) then begin
    FWHM=7
    print,cr,cr,'[CNTRD] Working....',up,up,up
    IR_cntrd,image,fix(mx+.5),fix(my+.5),cen1,cen2,FWHM
    print,cr,cr,'Centroid: ',vect([cen1,cen2]),up,up,up
    tvcrs,(cen1-xmin+xll+(zoom-1)/(2*zoom))*zoom+1, $
      (cen2-ymin+yll+(zoom-1)/(2*zoom))*zoom+1,/device
    mx=cen1 & my=cen2
    endif

  if (button eq 4) then begin
    IR_Widgets,w,'SetZoom',Ret_Val,zoom
    if (Ret_Val lt 1) or (Ret_Val gt 100) then return
    zoom=Ret_Val*1.
    print,cr & irdisp,image,img1,3 & print,up,up,up
    endif

  if (button eq 110) or (button eq 111) or $
     (button eq 1110) or (button eq 1111) then begin
    Ret_Val=-1 & option=button
    if (option eq 110) then IR_Widgets,w,'GetXY',Ret_Val
    if (option eq 111) and (astrom_type ne 'NONE') then IR_Widgets,w,'GetAD',Ret_Val
    if (option gt 1000) then begin
      option=option-1000 & Ret_Val=var10 & endif
    if (Ret_Val(0) eq -1) then return
    xtmp=xcent & ytmp=ycent
    if (option eq 110) then begin
      xcent=Ret_Val(0) & ycent=Ret_Val(1) & endif
    if (option eq 111) then begin
      a=Ret_Val(0) & d=Ret_Val(1)
      if (astrom_type eq 'STD') then ad2xy,a,d,astr,xcent,ycent
      if (astrom_type eq 'GSSS') then gsssadxy,gsa,a,d,xcent,ycent
      xcent=xcent(0) & ycent=ycent(0)
      endif
    if (xcent lt 0) or (xcent ge NAXIS1) or (ycent lt 0) or (ycent ge NAXIS2) $
      then begin
      ir_widgets,w,'MessageBox',Ret_Val,['Coordinates X,Y='+vect([xcent,ycent])+$
        'are off image.  Unable to move.','OK']
      xcent=xtmp & ycent=ytmp
      return & endif
    xtmp=xcent & ytmp=ycent
    IR_Roam,awin,xcent,ycent,101,image,img1
    xtmp=(xtmp-xmin+xll+(zoom-1)/(2*zoom))*zoom
    ytmp=(ytmp-ymin+yll+(zoom-1)/(2*zoom))*zoom
    plots,[-1,1,0,0,0]*30+xtmp,[0,0,0,1,-1]*30+ytmp,/device
    endif

  if (button eq 112) then IR_Roam,awin,xcent,ycent,104,image,img1

  if (button eq 113) or (button eq 213) then begin
SSTA:
    var1=[scmin,scmax] & var2=rdtyp
    IR_Widgets,w,'SetStretch',Ret_Val,var1,var2
    if (Ret_Val(0) eq -1) then return
    if (Ret_Val(0) eq 1) then begin
      scmin=var1(0) & scmax=var1(1) & rdtyp=var2 & stat.AUTOPROC=1
      endif
    if (Ret_Val(0) eq 200) then begin
      print,' [SKY_VALUE] Working....'
      sky_value,image,sxpar(hdr,'BITPIX'),10,scmin,scmax
      rdtyp='LOG' & stat.AUTOPROC=1
      tmp=sxpar(hdr,'AMDX1') & if (!ERR ge 0) then rdtyp='NONE'
      endif
    if (Ret_Val(0) eq 201) then begin
      if (strn(sxpar(hdr,'TELESCOP')) ne 'UIT') then goto,SSTA
      print,' [UIT_SKY_VALUE] Working....'
      UIT_Sky_Value,image,scmin,scmax
      rdtyp='LOG' & stat.AUTOPROC=1
      endif
    sxaddpar,hdr,'IR_SCMIN',scmin,' IMGRoam Frame Scaling Minimum'
    sxaddpar,hdr,'IR_SCMAX',scmax,' IMGRoam Frame Scaling Maximum'
    sxaddpar,hdr,'IR_RDTYP',rdtyp,' IMGRoam Frame Reduction Type'
    if (stat.AUTOPROC eq 1) and (button eq 113) then begin
      print,cr & irdisp,image,img1,3 & print,up,up,up
      COMPWIN=2 & cmpwinsho,image
      endif
    endif

  if (button eq 115) then IR_printframe,image

  return
end
