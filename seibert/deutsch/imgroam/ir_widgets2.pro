pro IR_Widgets2,w,Command,Ret_Val,var1,var2,var3

; This procedure handles many widget tasks
; in the IMGroam environment.  It is not useful by itself.

; **** Setup Button/Coordinate Display Widget ******************************
  if (Command eq 'DS_SETUP') then begin
    COMMON DS_Comm,DS_LftBut,DS_MidBut,DS_RgtBut,DS_CenCrd,DS_CrsCrd,DS_CrdNme,DS_ButLbls,DS_DefLbls
    IR_GetWinPos,'CoordWid',x,y
    DS_MnBase=widget_base(title='IMGRoam Coordinate Display',/column, $
      xoffset=x,yoffset=y)
;    font='LucidaSans-Typewriter'
    font='-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1'
;    wb1=widget_base(DS_MnBase,/column,/frame)
    wb1=widget_base(DS_MnBase,/column)								; IDL 5.0 patch
    DS_LftBut=widget_label(wb1,value='[LEFT MOUSE BUTTON]:  Set  new Frame Center Position')
    DS_MidBut=widget_label(wb1,value='[CENTER MOUSE BUTTON]:  Centroid Star at  Current Position')
    DS_RgtBut=widget_label(wb1,value='[RIGHT MOUSE BUTTON]:  Set  Zoom of Frame')
    w1=widget_label(DS_MnBase,value='Frame Center Coordinates:')
    w2=widget_label(DS_MnBase,font=font,value='X        Y         RA       DEC           RA           DEC           DN')
    DS_CenCrd=widget_label(DS_MnBase,font=font,value='    0.00     0.00     0.0000    0.0000   00 00  0.000   00 00  0.00       0.000')
    DS_CrdNme=widget_label(DS_MnBase,value='Cursor Coordinates:')
    w4=widget_label(DS_MnBase,font=font,value='X        Y         RA       DEC           RA           DEC           DN')
    DS_CrsCrd=widget_label(DS_MnBase,font=font,value='    0.00     0.00     0.0000    0.0000   00 00  0.000   00 00  0.00       0.000')
    widget_control,DS_MnBase,/realize,group_leader=w.IR_MnBase
    return
    endif

; **** Change Button Function Labels ******************************
  if (Command eq 'ButtonLab') then begin
    COMMON DS_Comm,DS_LftBut,DS_MidBut,DS_RgtBut,DS_CenCrd,DS_CrsCrd,DS_CrdNme,DS_ButLbls,DS_DefLbls
    Label=['Left','Middle','Right'] & Lines=[DS_LftBut,DS_MidBut,DS_RgtBut]
    Line=0
    for i=0,2 do if (var1 eq Label(i)) then Line=Lines(i)
    if (Line eq 0) then begin
      if (var1 eq 'OffWindow') or (var1 eq 'SaveDef') then begin
        DS_ButLbls=strarr(3) & if (var1 eq 'SaveDef') then DS_DefLbls=DS_ButLbls
        for i=0,2 do begin
          widget_control,Lines(i),get_value=tmp & DS_ButLbls(i)=tmp
          if (var1 eq 'SaveDef') then DS_DefLbls(i)=tmp
          endfor
        if (var1 eq 'SaveDef') then return
        for i=0,2,2 do widget_control,Lines(i),set_value=' '
        widget_control,Lines(1),set_value='Select Menu Item or Pushbutton'
        endif
      if (var1 eq 'OnWindow') then begin
        for i=0,2 do widget_control,Lines(i),set_value=DS_ButLbls(i)
        endif
      return
      endif
    widget_control,Line,set_value='['+strupcase(var1)+' MOUSE BUTTON]:  '+var2
    return
    endif

; **** Setup Astrometry Package Widget ****************************************
  if (Command eq 'GS_SETUP') then begin
    w.GS_MnBase=widget_base(title='IMGRoam Astrometry Package',/row)
    w.GS_LstCol=widget_base(w.GS_MnBase,/column)
    w.GS_OptCol=widget_base(w.GS_MnBase,/column)

    tmp='' & Lst='Idx  ID #     RA        DEC     MAG      X      Y      '
    for i=0,strlen(Lst)-1 do begin
      tmp=tmp+strmid(Lst,i,1)
      if (strmid(Lst,i,1) eq ' ') then tmp=tmp+' '
      endfor
    GS_StrTtl=widget_label(w.GS_LstCol,value=tmp,/frame)
    Lst='  '
    w.GS_StrLst=widget_list(w.GS_LstCol,ysize=10,value=Lst)

    w.GS_DeleBt=widget_button(w.GS_OptCol,value='Delete Selected Star')
    w.GS_DlAlBt=widget_button(w.GS_OptCol,value='Delete Whole List')
    w.GS_LoadBt=widget_button(w.GS_OptCol,value='Load Star File')
    w.GS_SaveBt=widget_button(w.GS_OptCol,value='Save This Star List')
    w.GS_MEntBt=widget_button(w.GS_OptCol,value='Manually Edit Star')
    w.GS_Mve2Bt=widget_button(w.GS_OptCol,value='Move to Selected Star')

    dummy=widget_label(w.GS_OptCol,value='When Selecting Stars:')
    GS_Update=widget_base(w.GS_OptCol,/column,/nonexclusive)
    w.GS_UpdtAD=widget_button(GS_Update,value='Update RA,DEC')
    w.GS_UpdtXY=widget_button(GS_Update,value='Update X,Y')

    GS_Selbse=widget_base(w.GS_LstCol,/row)
    GS_Sellbl=widget_label(GS_Selbse,value='Selected Star: ')
    w.GS_SelStr=widget_text(GS_Selbse,xsize=12,ysize=1,value='')

    GS_OtOpt1=widget_base(w.GS_LstCol,/row)
    w.GS_OpnGSC=widget_button(GS_OtOpt1,value='Open GSC Window')
    w.GS_AsPrBt=widget_button(GS_OtOpt1,value='Astrometry Procedures')
    w.GS_AuCnBt=widget_button(GS_OtOpt1,value='Automated Centroiding')
    GS_OtOpt2=widget_base(w.GS_LstCol,/row)
    w.GS_HelpBt=widget_button(GS_OtOpt2,value='HELP')
    w.GS_OthPBt=widget_button(GS_OtOpt2,value='Other Procedures')
    w.GS_SettBt=widget_button(GS_OtOpt2,value='Change Settings')
    w.GS_ExitBt=widget_button(GS_OtOpt2,value='Exit Astrometry Package')

    widget_control,w.GS_MnBase,/realize
    widget_control,w.GS_UpdtXY,set_button=1
    widget_control,w.GS_UpdtAD,set_button=1

    return
    endif

; **** Destroy Astrometry Package Widget **************************************
  if (Command eq 'GS_DESTROY') then begin
    widget_control,w.GS_MnBase,/destroy
    return
    endif

; **** Check GS Screen ******************************************************
  if (Command eq 'GS_CHECK') then begin
    s=size(Ret_Val)
    if (s(2) eq 8) then event=Ret_Val $
    else event=WIDGET_EVENT(w.GS_MnBase,/nowait)
    Ret_Val=-1
    if (event.ID eq 0) then return
    Ret_Val=1 & var1=event
    return
    endif

; **** Set Selected Star Field ************************************************
  if (Command eq 'SetSelStr') then begin
    if (Ret_Val eq var1) then tmp='End of List' else tmp=strn(Ret_Val)
    widget_control,w.GS_SelStr,set_value=tmp
    return
    endif

; **** Update Stars List ******************************************************
  if (Command eq 'GS_UPDATE') then begin
    ss=var1
    if (ss.stars eq 0) then begin
      widget_control,w.GS_StrLst,set_value=[' ']
      return & endif
    widget_control,w.GS_StrLst,set_value=['Please Wait...']
    Lst=strarr(ss.stars+1)
    for i=0,ss.stars-1 do begin
      tmp2=''
      tmp=string(format='(i3,2x,i4,1x,2f10.5,2x,f5.2,1x,2f8.2,a1)',i,ss.id(i), $
        ss.ra(i),ss.dec(i),ss.mag(i),ss.x(i),ss.y(i),' ')
      for j=0,strlen(tmp)-1 do begin
        tmp2=tmp2+strmid(tmp,j,1)
        if (strmid(tmp,j,1) eq ' ') then tmp2=tmp2+' '
        endfor
      Lst(i)=tmp2
      endfor
    Lst(i)='    '
    widget_control,w.GS_StrLst,set_value=Lst

    endif

; **** Edit Star Information *************************************************
  if (command eq 'EditStar') then begin
    w1=widget_base(/column,title=' ',xoffset=200,yoffset=400)
    w10=widget_base(w1,/row)
    if (strn(var1(0)) eq '-1') then tmp='New' else tmp=strn(fix(var1(0)))
    w2=widget_label(w10,value='Edit Information for Star: '+tmp)
    w11=widget_label(w10,value='')

    w13=widget_base(w1,/row)
    w14=widget_label(w13,value='Star ID Number: ')
    w15=widget_text(w13,/editable,xsize=40,ysize=1,value=strn(fix(var2(0))))

    w23=widget_base(w1,/row)
    w24=widget_label(w23,value='RA,DEC: ')
    w25=widget_text(w23,/editable,xsize=40,ysize=1,value=strn(var1(1))+' '+ $
      strn(var1(2)))

    w33=widget_base(w1,/row)
    w34=widget_label(w33,value='X,Y: ')
    w35=widget_text(w33,/editable,xsize=40,ysize=1,value=strn(var1(3))+' '+ $
      strn(var1(4)))

    w43=widget_base(w1,/row)
    w44=widget_label(w43,value='Magnitude: ')
    w45=widget_text(w43,/editable,xsize=40,ysize=1,value=strn(var1(5)))

    w6=widget_base(w1,/row)
    w9=widget_button(w6,value=strn('OK',length=10,padtype=2))
    w7=widget_button(w6,value=strn('CANCEL',length=10,padtype=2))
    w8=widget_button(w6,value=strn('CLEAR',length=10,padtype=2))
    widget_control,w1,/realize,/show
    
    Ret_Val=-599
    while (Ret_Val(0) eq -599) do begin
      event=WIDGET_EVENT(w1)
      if (event.ID eq w7) then Ret_Val=-1
      if (event.ID eq w8) then begin
        widget_control,w15,set_value='' & widget_control,w25,set_value=''
        widget_control,w35,set_value='' & widget_control,w45,set_value=''
        endif
      if (event.ID eq w9) then begin
        Ret_Val=1
        widget_control,w15,get_value=tmp & Coord=strnumber(tmp(0),var2)

        widget_control,w25,get_value=tmp
        Coord=tmp(0) & Coord=getopt(Coord,'F')
        case (n_elements(Coord)) of
          2: begin & var1(1)=coord(0) & var1(2)=coord(1) & end
          6: begin & var1(1)=ten(Coord(0:2)*15.) & var1(2)=ten(Coord(3:5)) & end
          else: begin & ir_widgets,w,'MessageBox',Ret_Val,['RA,DEC value is not valid.','OK']
            Ret_Val=-599 & end
          endcase

        widget_control,w35,get_value=Coord
        Coord=Coord(0) & Coord=getopt(Coord,'F')
        case (n_elements(Coord)) of
          2: begin & var1(3)=coord(0) & var1(4)=coord(1) & end
          else: begin & ir_widgets,w,'MessageBox',Ret_Val,['X,Y value is not valid.','OK']
            Ret_Val=-599 & end
          endcase

        widget_control,w45,get_value=tmp
        if (strnumber(tmp(0),Coord) eq 0) then begin
          ir_widgets,w,'MessageBox',Ret_Val,['Magnitude value is not valid.','OK']
          Ret_Val=-599
        endif else var1(5)=Coord

        endif

      endwhile
    widget_control,w1,/destroy
    return
    endif

; **** Choose Astrometry Procedure Type ***************************************
  if (command eq 'AsPrChoice') then begin
    w1=widget_base(/column,title=' ',xoffset=200,yoffset=400)
    w2=lonarr(4)
    w2(0)=widget_button(w1,value='Create Initial Solution using STARAST and first 3 stars w/[RA,DEC,X,Y])')
    w2(1)=widget_button(w1,value='Improve Solution with ASTROMIT (several stars w/ [RA,DEC,X,Y])')
    w2(2)=widget_label(w1,value=' ')
    w2(3)=widget_button(w1,value='  Cancel  ')

    widget_control,w1,/realize & Ret_Val=-1 & event=WIDGET_EVENT(w1)
    for i=0,3 do $
      if (event.ID eq w2(i)) then Ret_Val=i
    widget_control,w1,/destroy
    return
    endif

; **** Choose Automated Centroiding Type *************************************
  if (command eq 'CenTypChoice') then begin
    w1=widget_base(/column,title=' ',xoffset=200,yoffset=400)
    w17=widget_label(w1,value='For Each Star in the Star List:')
    w2=widget_base(w1,/row,/frame)
    w3=widget_label(w2,value='Centroid at coordinate type')
    w4=widget_base(w2,/column,/exclusive)
    if (var1 eq 1) then w5=widget_button(w4,value='RA,DEC') else w5=0
    w6=widget_button(w4,value='X,Y')

    w7=widget_base(w1,/row,/frame)
    w8=widget_label(w7,value='Record Centroided value as')
    w9=widget_base(w7,/column,/nonexclusive)
    if (var1 eq 1) then w10=widget_button(w9,value='RA,DEC') else w10=0
    w11=widget_button(w9,value='X,Y')

    w12=widget_base(w1,/row,/nonexclusive)
    w13=widget_button(w12,value='Create a PostScript Record')

    w14=widget_base(w1,/row)
    w15=widget_button(w14,value='  OK  ')
    w16=widget_button(w14,value='  CANCEL  ')

    widget_control,w1,/realize
    if (var1 eq 1) then begin & widget_control,w5,set_button=1 & InTyp=0 & endif
    if (var1 eq 0) then begin & widget_control,w6,set_button=1 & InTyp=1 & endif
    widget_control,w11,set_button=1 & RecRADEC=0 & RecXY=1
    PSRec=0

    Ret_Val=-599
    while (Ret_Val eq -599) do begin
      event=WIDGET_EVENT(w1)
      if (event.ID eq w5) then InTyp=0
      if (event.ID eq w6) then InTyp=1
      if (event.ID eq w10) then RecRADEC=abs(RecRADEC-1)
      if (event.ID eq w11) then RecXY=abs(RecXY-1)
      if (event.ID eq w13) then PSRec=abs(PSRec-1)
      if (event.ID eq w16) then Ret_Val=-1
      if (event.ID eq w15) then Ret_Val=1
      endwhile
    widget_control,w1,/destroy
    
    if (Ret_Val eq -1) then return
    var1=[InTyp,RecRADEC,RecXY,PSRec]
    return
    endif

; **** Color Tables *****************************************************
  if (command eq 'ColorTables') then begin

    w1=widget_base(title='Color Table Adjustments',xoffset=200,yoffset=400,/column)

    wbut1=lonarr(11)
    wbut1(0)=widget_button(w1,value='           XLOADCT (WIDGETLIB)          ')
    wbut1(1)=widget_button(w1,value='XPALETTE (WIDGETLIB)')
    wbut1(2)=widget_button(w1,value='ADJCT (USERLIB)')
    wbut1(3)=widget_button(w1,value='CONTRAST (UIT LIB)')
    wbut1(4)=widget_button(w1,value='C_EDIT (USERLIB)')
    wbut1(5)=widget_button(w1,value='COLOR_EDIT (USERLIB)')
    wbut1(6)=widget_button(w1,value='TVBALL (UIT LIB)')
    wbut1(7)=widget_button(w1,value='Black Sky (EWD)')
    wbut1(8)=widget_button(w1,value='White Sky (EWD)')
    wbut1(9)=widget_button(w1,value='LOADCT (USERLIB)')
    wbut1(10)=widget_button(w1,value='DONE')

    widget_control,w1,/realize,/show

    Ret_Val=-599
    while (Ret_Val eq -599) do begin
      event=WIDGET_EVENT(w1)
      if (event.ID eq wbut1(0)) then xloadct,/modal
      if (event.ID eq wbut1(1)) then xpalette
      if (event.ID eq wbut1(2)) then Adjct
      if (event.ID eq wbut1(3)) then begin
        wset,0
        contrast
        endif
      if (event.ID eq wbut1(4)) then c_edit
      if (event.ID eq wbut1(5)) then color_edit
      if (event.ID eq wbut1(6)) then tvball
      if (event.ID eq wbut1(7)) then blacksky
      if (event.ID eq wbut1(8)) then whitesky,reserve='RED'
      if (event.ID eq wbut1(9)) then loadct
      if (event.ID eq wbut1(10)) then Ret_Val=-1
      endwhile

    widget_control,w1,/destroy
    return
    endif


; **** Color Tables *****************************************************
  if (command eq 'PostCentroid') then begin

    w1=widget_base(title='Post-Centroid Options',xoffset=800,yoffset=600,/column)

    wbut1=lonarr(11)
    wbut1(0)=widget_button(w1,value='         Radial Profile (radprof)        ')
    wbut1(1)=widget_button(w1,value='Contour (starcont)')
    wbut1(2)=widget_button(w1,value='DIS Slit Calculator')
    wbut1(3)=widget_button(w1,value='DONE')

    widget_control,w1,/realize,/show

    Ret_Val=-599 & allocflag=0
    while (Ret_Val eq -599) do begin
      event=WIDGET_EVENT(w1)
      if (event.ID eq wbut1(0)) then begin
        if (allocflag eq 0) then begin & win_alloc,PSFWIN & allocflag=1 & endif
        win_open,PSFWIN,500,500,70,200,'Radial Plot'
        ir_radprof,var1,var2,var3
        endif
      if (event.ID eq wbut1(1)) then begin
        if (allocflag eq 0) then begin & win_alloc,PSFWIN & allocflag=1 & endif
        win_open,PSFWIN,500,500,70,200,'Contour Plot'
        starcontour,var1,var2,var3
        endif
      if (event.ID eq wbut1(2)) then discalc,var1,var2,var3
      if (event.ID eq wbut1(3)) then Ret_Val=-1
      endwhile

    if (allocflag eq 1) then win_dele,PSFWIN

    widget_control,w1,/destroy
    return
    endif



  return
end
