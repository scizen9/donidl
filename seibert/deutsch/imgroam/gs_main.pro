pro GS_MAIN,ss,image,img1,h

; This procedure handles all the functions of the astrometry package
; in the IMGroam environment.  It is not useful by itself.


  COMMON ANSI,CR,LF,esc,UP,CLRSCRN,BELL,DOWN,RIGHT,LEFT,NORMAL,BOLD, $
    REVERSE,BLINKING
  COMMON GSGET_PARAMS,GSfile,target_ra,target_dec,FOV,stars,star_labeling
  COMMON PLPAR,xmn,ymn,xmx,ymx,GuideStWIN,ssptr
  COMMON IR_ASTROM,astrom_type,hdr,astr,gsa
  COMMON fparm,NAXIS1,NAXIS2,xsize,ysize,xcent,ycent,xll,yll,zoom,frtyp
  COMMON Windows,FrameWIN,CmpressWIN
  COMMON IR_ENVIR,stat,itype
  COMMON Widgets,w

  arg=n_params(0)
  if (arg lt 3) then begin
    print,'[GS_MAIN] Not enough parameters'
    return
    endif

  ss={Selected_Stars1,stars:0,ID:intarr(1000),Name:strarr(1000),X:dblarr(1000), $
    Y:dblarr(1000),RA:dblarr(1000),DEC:dblarr(1000),MAG:fltarr(1000)}
  mxstr=1500
  ls={Loaded_Stars,stars:0,ID:intarr(mxstr),STRID:strarr(mxstr),X:fltarr(mxstr),Y:fltarr(mxstr), $
    RA:dblarr(mxstr),DEC:dblarr(mxstr),MAG:fltarr(mxstr)}

  command=0 & GSfile='NONE' & GuideStWIN=-1 & ssptr=0
  target_ra=1. & target_dec=1. & FOV=0. & stars=0 & star_labeling=0

  defansi
  win_init

  IR_Widgets2,w,'GS_SETUP' & UpdtXY=1 & UpdtAD=1
  IR_Widgets2,w,'SetSelStr',ssptr,ss.stars

  flag=0 & pwin=-1
  GS_Disp,ss
  print,'Move Mouse Pointer to a window or select command '
  print,'                                                 '
  print,'                                                 '

  while (flag eq 0) do begin
    Ret_Val=1 & win_mseread,awin,mx,my,button,Wid_Chk=Ret_Val
    key1=get_kbrd(0) & while (key1 ne '') do key1=get_kbrd(0)

; ***************************************************** Frame Window stuff ****
    if (awin eq FrameWIN) or (awin eq CmpressWIN) then begin
      if (pwin ne FrameWIN) then begin
        IR_Widgets2,w,'ButtonLab',Ret_Val,'OnWindow'
        pwin=FrameWIN
        endif

      IR_Roam,awin,mx,my,button,image,img1

      if (button eq 2) and (awin eq FrameWIN) then begin
        if (ss.id(ssptr) eq 0) then ss.id(ssptr)=ssptr
        if (ss.mag(ssptr) eq 0) then ss.mag(ssptr)=99.
        if (ss.mag(ssptr) eq '') then ss.Name(ssptr)=''

        if (mx lt 0) or (my lt 0) or (mx ge NAXIS1) or (my ge NAXIS2) then $
          DN=0. else DN=image(mx,my)
        IR_astdisp,mx,my,ra,dec,DN,x2=x2,y2=y2

        if (UpdtAD eq 1) and (astrom_type ne 'NONE') then begin
          ss.ra(ssptr)=ra & ss.dec(ssptr)=dec
          endif
        if (UpdtXY eq 1) then begin
          ss.x(ssptr)=x2 & ss.y(ssptr)=y2
          endif
        if (ssptr eq ss.stars) then ss.stars=ss.stars+1
        ssptr=ssptr+1

        IR_Widgets2,w,'GS_UPDATE',Ret_Val,ss
        IR_Widgets2,w,'SetSelStr',ssptr,ss.stars
        GS_Disp,ss,/NoList & pwin=-1 & print,cr,cr
        IR_astdisp,mx,my,ra,dec,0
        print,up,up,up,'Centroided Star Coordinates:'
        cursor,mx,my,2,/device
        endif
      endif
; *********************************************** Guide Stars Window stuff ****
    if (awin eq GuideStWIN) then begin
      if (pwin ne GuideStWIN) then begin
        IR_Widgets2,w,'ButtonLab',Ret_Val,'Left','Not Defined'
        IR_Widgets2,w,'ButtonLab',Ret_Val,'Middle','Select Star at Current Position'
        IR_Widgets2,w,'ButtonLab',Ret_Val,'Right','Set Zooming of Guide Stars Plot'
        pwin=GuideStWIN
        endif
      GS_Roam,ls,ss,starno
      if (starno ne -1) then begin
        print,cr,cr & pwin=-1
        endif
      endif

; *********************************************** Widget Stuff ***************
    if (awin eq -2) then begin
      IR_Widgets,w,'IR_CHECK',Ret_Val
      if (Ret_Val eq -1) then goto,M0
      IR_Roam,awin,xcent,ycent,200+Ret_Val,image,img1
      endif

    if (awin eq -3) then begin
      IR_Widgets2,w,'GS_CHECK',Ret_Val,event
      if (Ret_Val eq -1) then goto,M0

      if (event.ID eq w.GS_ExitBt) then flag=1
      if (event.ID eq w.GS_OpnGSC) then goto,OPENGS
      if (event.ID eq w.GS_LoadBt) then goto,IRSFLoad
      if (event.ID eq w.GS_SaveBt) then goto,IRSFSave
      if (event.ID eq w.GS_StrLst) then begin
        ssptr=event.INDEX & IR_Widgets2,w,'SetSelStr',ssptr,ss.stars & endif
      if (event.ID eq w.GS_DlAlBt) then begin
        ir_widgets,w,'MessageBox',Ret_Val,['Delete Whole List.  Are you sure?','Delete','Cancel']
        if (Ret_Val eq 1) then goto,M0
        ssptr=0 & ss.stars=0
        ss.X=ss.X*0 & ss.Y=ss.Y*0 & ss.RA=ss.RA*0 & ss.DEC=ss.DEC*0
        endif
      if (event.ID eq w.GS_DeleBt) then begin
        if (ssptr eq ss.stars) then goto,M0
        i=ssptr
        while (i lt ss.stars-1) do begin
          ss.id(i)=ss.id(i+1) & ss.ra(i)=ss.ra(i+1)
          ss.dec(i)=ss.dec(i+1) & ss.mag(i)=ss.mag(i+1)
          ss.x(i)=ss.x(i+1) & ss.y(i)=ss.y(i+1)
          i=i+1
          endwhile
        ss.stars=ss.stars-1
        if (ssptr gt ss.stars) then ssptr=ss.stars
        endif
      if (event.ID eq w.GS_DlAlBt) or (event.ID eq w.GS_DeleBt) then begin
        IR_Widgets2,w,'GS_UPDATE',Ret_Val,ss
        IR_Widgets2,w,'SetSelStr',ssptr,ss.stars
        endif
      if (event.ID eq w.GS_UpdtAD) then UpdtAD=-UpdtAD+1
      if (event.ID eq w.GS_UpdtXY) then UpdtXY=-UpdtXY+1
      if (event.ID eq w.GS_MEntBt) then begin
        if (ssptr eq ss.stars) then begin
          var2=strn(ssptr) & var1=['-1','','','','','','']
        endif else begin
          var2=ss.ID(ssptr)
          var1=[ssptr,ss.RA(ssptr),ss.DEC(ssptr),ss.X(ssptr),ss.Y(ssptr), $
            ss.MAG(ssptr)]
          endelse
        ir_widgets2,w,'EditStar',Ret_Val,var1,var2
        if (Ret_Val eq 1) then begin
          ss.ID(ssptr)=fix(var2) & ss.RA(ssptr)=var1(1) & ss.DEC(ssptr)=var1(2)
          ss.X(ssptr)=var1(3) & ss.Y(ssptr)=var1(4) & ss.MAG(ssptr)=var1(5)
          ssptr=ssptr+1
          if (ssptr gt ss.stars) then ss.stars=ssptr
          IR_Widgets2,w,'GS_UPDATE',Ret_Val,ss
          IR_Widgets2,w,'SetSelStr',ssptr,ss.stars
          endif
        endif
      if (event.ID eq w.GS_Mve2Bt) and (ssptr ne ss.stars) then begin
        MvTyp=0
        if (UpdtAD eq 1) and (UpdtXY eq 0) then MvTyp=1
        if (MvTyp eq 0) then $
          IR_Roam,awin,mx,my,1210,image,img1,[ss.X(ssptr),ss.Y(ssptr)]
        if (MvTyp eq 1) then $
          IR_Roam,awin,mx,my,1211,image,img1,[ss.RA(ssptr),ss.DEC(ssptr)]
        endif

      if (event.ID eq w.GS_AsPrBt) then begin
        if (ss.stars lt 3) then begin
          ir_widgets,w,'MessageBox',Ret_Val,['At least three must must be selected to use Astrometry procedures','OK']
          goto,M0
          endif
        ir_widgets2,w,'AsPrChoice',Ret_Val
        if (Ret_Val eq 0) then goto,StarAst
        if (Ret_Val eq 1) then goto,Astromit
        endif

      if (event.ID eq w.GS_AuCnBt) then begin
        if (ss.stars lt 1) then begin
          ir_widgets,w,'MessageBox',Ret_Val,['There are no stars in the Selected Star List','OK']
          goto,M0 & endif
        goto,AutoCent
        endif

      endif

M0: flag=flag
    endwhile

; ********************************************************* End of Program ****
BRK:
  if (GuideStWIN ne -1) then win_dele,GuideStWIN
  IR_Widgets2,w,'GS_DESTROY'
  w.GS_MnBase=0L
  return

; ************************************************************ Subroutines ****
; *****************************************************************************
; *****************************************************************************

; ************************************************** Load IMRoam Star File ++++
IRSFLoad:
  print,cr,cr,cr,cr,cr,cr,cr,cr,cr,'Please enter the Name of the file to load.'
  print,' (CANCEL to cancel or [RETURN] for GETFILE)  [sugg .EXT= .IRSF]'
  tmp='file'
  read,'Filename: ',tmp
  if (tmp eq 'CANCEL') or (tmp eq 'cancel') then goto,M0
  if (tmp eq '') then tmp=getfile(srchpath=dirpath,srchspec='*.irsf')
  if (tmp eq '+CANCEL') then goto,M0
  IRSFLoad,tmp,ss
  GS_Disp,ss,/NoList & pwin=-1 & print,cr,cr
  IR_Widgets2,w,'GS_UPDATE',Ret_Val,ss & ssptr=ss.stars
  IR_Widgets2,w,'SetSelStr',ssptr,ss.stars
  goto,M0

; ************************************************** Save IMRoam Star File ++++
IRSFSave:
  print,cr,cr,cr,cr,cr,cr,cr,'Please enter a Name for the Save File'
  print,' (CANCEL to cancel)  [sugg .EXT= .IRSF]'
  tmp='file'
  read,'Filename: ',tmp
  if (tmp eq 'CANCEL') or (tmp eq 'cancel') then goto,M0
  IRSFSave,tmp,ss
  GS_Disp,ss,/NoList & pwin=-1 & print,cr,cr
  goto,M0

; ****************** STARAST *******************************
StarAst:
  flag=0 & a=dblarr(3) & d=a & x=a & y=a & st=0 & i=0
  while (flag eq 0) do begin
    if (ss.ra(i)*ss.dec(i)*ss.x(i)*ss.y(i) ne 0.) and (st lt 3) then begin
      a(st)=ss.ra(i) & d(st)=ss.dec(i) & x(st)=ss.x(i) & y(st)=ss.y(i)
      st=st+1
      endif
    i=i+1
    if (i eq ss.stars) then flag=1
    endwhile
  flag=0
  if (st ne 3) then begin
    ir_widgets,w,'MessageBox',Ret_Val,['You need to have at least 3 selected stars with both RA,DEC and X,Y to use this procedure.','OK']
    goto,M0
    endif
  starast,a,d,x,y,cdtmp
  astr = {CD: double(cdtmp), CDELT: double([0,0]), $
		CRPIX: float([x(0),y(0)]+1), CRVAL:double([a(0),d(0)]), $
		CTYPE: string(['RA---TAN','DEC--TAN']), $
                LONGPOLE: float(0),  $
		PROJP1: float(0), PROJP2: float(0)}
  PUTAST,h,astr.cd,astr.crpix,astr.crval & extast,h,astr
  print,'switching Astrometry: ON' & astrom_type='STD' & stat.ASTR=1
  GS_Disp,ss,/NoList & pwin=-1 & print,cr,cr
  goto,M0

; ******************* ASTROMIT *************************
Astromit:
  i=ss.stars-1 & a=ss.ra(0:i) & d=ss.dec(0:i) & x=ss.x(0:i) & y=ss.y(0:i)
  i=0
  while (i lt ss.stars) do begin
    if (a(i)*d(i)*x(i)*y(i) eq 0.) then begin
      ir_widgets,w,'MessageBox',Ret_Val,['At least one of your stars contains a 0.000 for its coordinate.  Do you wish to continue?','Continue','Cancel']
      if (Ret_Val eq 1) then goto,M0
      endif
    i=i+1
    endwhile
  astromit,x,y,a,d,h & astrom_type='STD' & stat.ASTR=1
  extast,h,astr
  GS_Disp,ss,/NoList & pwin=-1 & print,cr,cr
  goto,M0

; ****************** AUTOMATED CENTROIDING *************************
AutoCent:
  Control=stat.ASTR
  IR_Widgets2,w,'CenTypChoice',Ret_Val,Control
  if (Ret_Val eq -1) then goto,M0
  PS=Control(3) & PSim=0

  win_alloc,StarWIN & cross=10 & delay=0
  win_open,StarWIN,20*12,20*12,200,604,'Star Window' & win_set,StarWIN
  i=0

  while (i lt ss.stars) do begin
    if (Control(0) eq 0) then begin
      if (astrom_type eq 'STD') then ad2xy,ss.RA(i),ss.DEC(i),astr,xcent,ycent
      if (astrom_type eq 'GSSS') then gsssadxy,gsa,ss.RA(i),ss.DEC(i),xcent,ycent
      endif
    if (Control(0) eq 1) then begin
      xcent=ss.X(i) & ycent=ss.Y(i) & endif

    if (xcent lt 1) or (ycent lt 1) or (xcent gt NAXIS1-2) or (ycent gt NAXIS2-2) then begin
      cen1=-1 & cen2=-1
      print,'Star '+strn(ss.ID(i))+' off image!'
      goto,TRY2
      endif

    tmp=extrac(image,fix(xcent+.5)-9,fix(ycent+.5)-9,20,20)
    fpr,tmp,tmp2,/silent
    plot,pos=[0,0,240,240],[0,240],[0,240],/device,xst=5,yst=5,/nodata
    tv,congrid(tmp2,20*12,20*12)
    xyouts,20,210,'#'+strn(ss.ID(i))
    drawcross,(xcent-fix(xcent+.5)+9)*12+6,(ycent-fix(ycent+.5)+9)*12+6,8,/top
    IR_cntrd,image,fix(xcent+.5),fix(ycent+.5),cen1,cen2,7
TRY2:
    drawcross,(cen1-fix(xcent+.5)+9)*12+6,(cen2-fix(ycent+.5)+9)*12+6,8

    if (PS eq 1) then begin
      tmp=255-tvrd(0,0,240,240)
      set_plot,'ps'
      if (PSim eq 24) then begin & PSim=0 & psclose & endif
      setps,1.5,1.5,.87+(PSim-PSim/4*4)*1.75,.45+8.75-(PSim/4)*1.75
      plot,[0,240],[0,240],xst=5,yst=5,/nodata
      tmp2=intarr(240) & tmp(0,*)=tmp2 & tmp(239,*)=tmp2
      tmp(*,0)=tmp2 & tmp(*,239)=tmp2 & tv,tmp
      set_plot,'x' & PSim=PSim+1
      endif

    IR_AstDisp,cen1,cen2,ra1,dec1,0,x2=x2,y2=y2 & cen1=x2 & cen2=y2

    print,'Star ',strn(i),' Centroided at ',vect([cen1,cen2])
    if (Control(2) eq 1) then begin ss.X(i)=cen1 & ss.Y(i)=cen2 & endif
    if (Control(1) eq 1) then begin
      if (astrom_type eq 'GSSS') then gsssxyad,gsa,cen1,cen2,ra,dec
      if (astrom_type eq 'STD') then begin
        xy2ad,cen1,cen2,astr,ra,dec
        endif
      ss.RA(i)=ra & ss.DEC(i)=dec
      endif

    if (PS eq 0) then begin
      if (delay eq 0) then delay=wmenu(['Next Star','Auto: 1 second','Auto: 4 second','Manual Position','Cancel'])
      if (delay eq 3) then begin
        print,'Click on star' & delay=0
        cursor,x,y,/device
        x=fix(xcent+.5)-9+x/12. & y=fix(ycent+.5)-9+y/12.
        IR_cntrd,image,x,y,cen1,cen2,7
        goto,TRY2
        endif
      if (delay eq 4) then begin & i=ss.stars & delay=0 & endif
      if (delay gt 0) then wait,delay^2
      endif

    i=i+1
    endwhile

  if (PS eq 1) then begin & set_plot,'ps' & psclose & endif
  GS_Disp,ss,/NoList & pwin=-1 & print,cr,cr & win_dele,StarWIN
  IR_Widgets2,w,'GS_UPDATE',Ret_Val,ss
  goto,M0








; *************************************************** Load Guide Star File ++++
OPENGS:
    tmp=pickfile(filter='*.*',title='Read Guide Stars File')
    if (tmp eq '') then goto,M0
    GSfile=tmp
    GSC_Read,GSfile,ls,t1
    if (ls(0).ID eq -1) then goto,M0
    target_ra=t1(0) & target_dec=t1(1)
    if (GuideStWIN eq -1) then begin
      win_alloc,GuideStWIN
      win_open,GuideStWIN,470,470,1,373,'Guide Stars in file'
      endif
    GS_FOVplot,ls,ss,1
    GS_Disp,ss & pwin=-1 & print,cr,cr
    FOV=abs(ymn-ymx)
    goto,M0

; ************************************************ Guide Star Window Stuff ++++
GSWINSTUFF:
  if (GuideStWIN eq -1) then begin
    xmessage,'You have not loaded a Guide Star File yet',['OK'],tmp
    goto,M0
    endif
  tmpmenu=strarr(5)
  tmpmenu(0)='Flip RA axis of plot'
  tmpmenu(1)='Flip DEC axis of plot'
  tmpmenu(2)='Set Star Labeling Parameter'
  tmpmenu(3)='Send Guide Star Plot to PostScript'
  tmpmenu(4)='Cancel'
  choice=wmenu(tmpmenu)
  if (choice eq 4) then goto,M0
  win_set,GuideStWIN
; ********************************************************** FLIP/ROTATE AXES *
  if (choice eq 0) then begin
    tmp=xmn & xmn=xmx & xmx=tmp
    GS_FOVplot,ls,ss,0
    endif
  if (choice eq 1) then begin
    tmp=ymn & ymn=ymx & ymx=tmp
    GS_FOVplot,ls,ss,0
    endif
; ************************************************************ PRINT FIELD ****
  if (choice eq 3) then begin
    setps
    GS_FOVplot,ls,ss,0
    psclose,1
    wait,1
    endif
; *********************************************************** STAR LABELING ***
  if (choice eq 2) then begin
    if (star_labeling eq 0) then star_labeling=1
    if (star_labeling eq 2) then star_labeling=0
    if (star_labeling eq 1) then star_labeling=2
    GS_FOVplot,ls,ss,0
    endif
  goto,M0

end
