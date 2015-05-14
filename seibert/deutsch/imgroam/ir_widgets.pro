pro IR_Widgets,w,Command,Ret_Val,var1,var2

; This procedure handles many widget tasks
; in the IMGroam environment.  It is not useful by itself.

; **** IMGRoam Widget System Setup ********************************************
  if (Command eq 'SETUP') then begin
    w={IR_MnBase:0L, IR_LftCol:0L, IR_RgtCol:0L, $
      IR_MnMenu:0L, IR_MenuOp:lonarr(8), IR_MnFunc:0L, IR_FuncBt:lonarr(7), $
      GS_MnBase:0L, GS_LstCol:0L, $
      GS_OptCol:0L, GS_StrLst:0L, GS_DeleBt:0L, GS_DlAlBt:0L, $
      GS_LoadBt:0L, GS_SaveBt:0L, GS_MEntBt:0L, GS_Mve2Bt:0L, $
      GS_UpdtAD:0L, GS_UpdtXY:0L, GS_OpnGSC:0L, GS_AsPrBt:0L, $
      GS_AuCnBt:0L, GS_HelpBt:0L, GS_OthPBt:0L, GS_SettBt:0L, $
      GS_ExitBt:0L, GS_SelStr:0L}
    return
    endif

; **** Setup Main Menu ********************************************************
  if (Command eq 'IR_SETUP') then begin
    IR_GetWinPos,'ButtnWid',x,y
    w.IR_MnBase=widget_base(title='Image Roam  Widget Version 3.2',/row, $
      xoffset=x,yoffset=y)
    w.IR_LftCol=widget_base(w.IR_MnBase,/column,xsize=260)
    w.IR_RgtCol=widget_base(w.IR_MnBase,/column)
    tmpmenu=[' Define ROI ', $
      ' Color Table Procedures',' Astrometry Package',' Program Settings/Defaults', $
      ' Annotate Frame Window ',' Exit IMGRoam']
    w.IR_MnMenu=widget_base(w.IR_LftCol,/column)
    for i=0,n_elements(tmpmenu)-1 do $
      w.IR_MenuOp(i)=widget_button(w.IR_MnMenu,value=tmpmenu(i))
    tmpmenu=['Move to X,Y','Move to RA,DEC','Set Zoom Factor','Set Stretch', $
      'Refresh Text','Print Frame']
    w.IR_MnFunc=widget_base(w.IR_RgtCol,/column)
    for i=0,n_elements(tmpmenu)-1 do $
      w.IR_FuncBt(i)=widget_button(w.IR_MnFunc,value=tmpmenu(i))
    widget_control,w.IR_MnBase,/realize
    return
    endif

; **** Clear all Events *******************************************************
  if (Command eq 'IR_CLEAR') then begin
    widget_control,w.IR_MnBase,/clear_events
    return
    endif

; **** Destroy Main Menu ******************************************************
  if (Command eq 'IR_DESTROY') then begin
    widget_control,w.IR_MnBase,/destroy
    return
    endif

; **** Check Main Screen ******************************************************
  if (Command eq 'IR_CHECK') then begin
    s=size(Ret_Val)
    if (s(2) eq 8) then event=Ret_Val $
    else event=WIDGET_EVENT(w.IR_MnBase,/nowait)
    Ret_Val=-1 & choice=-1
    if (event.ID eq 0) then return
    for i=0,7 do if (event.ID eq w.IR_MenuOp(i)) then choice=i
    for i=0,5 do if (event.ID eq w.IR_FuncBt(i)) then choice=i+10
    if (choice eq 5) then IR_Widgets,w,'EXIT?',choice
    Ret_Val=choice
    return
    endif

; **** Confirm the EXIT command ***********************************************
  if (Command eq 'EXIT?') then begin
    w1=widget_base(/column,title=' ',xoffset=200,yoffset=400)
    w2=widget_label(w1,value='Do you want to EXIT IMGRoam?')
    w3=widget_base(w1,/column)
    w4=widget_button(w3,value='  EXIT  (Leave image windows open)  ') 
    w5=widget_button(w3,value='  EXIT  (Close image windows)  ') 
    w6=widget_button(w3,value='  CANCEL  ')
    widget_control,w1,/realize & Ret_Val=0 & event=WIDGET_EVENT(w1)
    Ret_Val=-1
    if (event.ID eq w4) then Ret_Val=7
    if (event.ID eq w5) then Ret_Val=107
    widget_control,w1,/destroy
    return
    endif

; **** Confirm the EXIT command ***********************************************
  if (Command eq 'MessageBox') then begin
    w1=widget_base(/column,title=' ',xoffset=200,yoffset=400)
    w2=widget_label(w1,value=var1(0))
    if (n_elements(var1) eq 2) then w3=w1 else w3=widget_base(w1,/row)
    w4=lonarr(n_elements(var1))
    for i=1,n_elements(var1)-1 do $
      w4(i)=widget_button(w3,value='  '+var1(i)+'  ')
    widget_control,w1,/realize & Ret_Val=-1 & event=WIDGET_EVENT(w1)
    for i=1,n_elements(var1)-1 do $
      if (event.ID eq w4(i)) then Ret_Val=i-1
    widget_control,w1,/destroy
    return
    endif

; **** Get either X,Y or RA,DEC information **********************************
  if (command eq 'GetXY') or (command eq 'GetAD') then begin
    if (command eq 'GetXY') then begin & mode=1 & str1='X,Y' & endif
    if (command eq 'GetAD') then begin & mode=2 & str1='RA,DEC' & endif
    w1=widget_base(/column,title=' ',xoffset=200,yoffset=400)
    w10=widget_base(w1,/row)
    w2=widget_label(w10,value='Enter New Frame '+str1+' Coordinates')
    w11=widget_label(w10,value='')
    w3=widget_base(w1,/row)
    w4=widget_label(w3,value=str1+': ')
    w5=widget_text(w3,/editable,xsize=40,ysize=1,value='')
    w6=widget_base(w1,/row)
    w9=widget_button(w6,value=strn('OK',length=10,padtype=2))
    w7=widget_button(w6,value=strn('CANCEL',length=10,padtype=2))
    w8=widget_button(w6,value=strn('CLEAR',length=10,padtype=2))
    widget_control,w1,/realize,/show
    
    Ret_Val=-599
    while (Ret_Val(0) eq -599) do begin
      event=WIDGET_EVENT(w1)
      if (event.ID eq w7) then Ret_Val=-1
      if (event.ID eq w8) then widget_control,w5,set_value=''
      if (event.ID eq w5) or (event.ID eq w9) then begin
        widget_control,w5,get_value=Coord
        Coord=Coord(0)
        Coord=getopt(Coord,'F')
        case (n_elements(Coord)) of
          2: Ret_Val=Coord
          6: begin & Ret_Val=[ten(Coord(0:2)*15.),ten(Coord(3:5))] & end
          else: begin & widget_control,w11,set_value='     Invalid'
            wait,1 & widget_control,w11,set_value='' & end
          endcase
        endif
      endwhile
    widget_control,w1,/destroy
    return
    endif

; **** Set Zoom Factor ********************************************************
  if (command eq 'SetZoom') then begin
    w1=widget_base(title=' ',xoffset=400,yoffset=300,/column)
    w9=widget_label(w1,value='Current Zoom: '+strn(fix(var1)))
    w10=widget_base(w1,/row)
    w2=widget_base(w10,/column)
    w3=widget_base(w10,/column)

    zoomstr=strarr(20)
    zoomstr(0:7)=string(indgen(8)+1)+'     '
    zoomstr(8:12)=string(indgen(5)*2+10)+'     '
    zoomstr(13:19)=string(indgen(7)*5+20)+'     '

    w4=widget_list(w2,ysize=20,value=zoomstr)

    w7=widget_button(w3,value=strn('Zoom = 1',length=13,padtype=2))
    w5=widget_button(w3,value=strn('Increase 1',length=13,padtype=2))
    w6=widget_button(w3,value=strn('Decrease 1',length=13,padtype=2))
    w8=widget_button(w3,value=strn('Cancel',length=13,padtype=2))

    w99=widget_label(w3,value=' ')
    w11=widget_label(w3,value='Enter Zoom:')
    w12=widget_text(w3,/editable,xsize=10,ysize=1,value='')

    widget_control,w1,/realize,/show

    Ret_Val=-599
    while (Ret_Val(0) eq -599) do begin
      event=WIDGET_EVENT(w1)
      if (event.ID eq w7) then Ret_Val=1
      if (event.ID eq w5) then Ret_Val=var1+1
      if (event.ID eq w6) then Ret_Val=var1-1>1
      if (event.ID eq w8) then Ret_Val=-1
      if (event.ID eq w12) then begin
        widget_control,w12,get_value=Ret_Val & Ret_Val=Ret_Val(0)
        if (strnumber(Ret_Val,tmp) eq 1) then Ret_Val=fix(tmp) $
          else Ret_Val=0
        if (Ret_Val lt 1) or (Ret_Val gt 100) then begin
          widget_control,w12,set_value='Invalid'
          wait,1 & widget_control,w12,set_value='' & Ret_Val=-599
          endif
        endif
      if (event.ID eq w4) then Ret_Val=fix(zoomstr(event.INDEX))
      endwhile

    widget_control,w1,/destroy
    return
    endif

; **** Set Stretch ********************************************************
  if (command eq 'SetStretch') then begin
    ScMin=var1(0) & ScMax=var1(1) & SCTyp=var2

    w1=widget_base(title=' ',xoffset=200,yoffset=400,/row)
    w20=widget_base(w1,/column)
    w2=widget_base(w1,/column)
    w3=widget_base(w1,/column)

    w21=widget_label(w20,value='Auto Determination:')
    w22=widget_base(w20,/column,/exclusive)
    wbut1=lonarr(9)
    wbut1(0)=widget_button(w22,value='SKY_VALUE (DJL)')
    wbut1(1)=widget_button(w22,value='UIT_Sky_Value (EWD)')
    wbut1(2)=widget_button(w22,value='User Function 1')
    wbut1(3)=widget_button(w22,value='User Function 2')

    w4=widget_label(w2,value='Reduction Type:')
    w5=widget_base(w2,/column,/exclusive)
    wbut=lonarr(9)
    wbut(0)=widget_button(w5,value='LINEAR')
    wbut(1)=widget_button(w5,value='Logarithmic')
    wbut(2)=widget_button(w5,value='Square Root')
    wbut(3)=widget_button(w5,value='Cube Root')
    wbut(4)=widget_button(w5,value='Fifth Root')
    wbut(5)=widget_button(w5,value='Sine Curve')
    wbut(6)=widget_button(w5,value='Plain TVSCL')
    wbut(7)=widget_button(w5,value='Plain TV')

    w12=widget_label(w3,value='Scaling Minimum:')
    w13=widget_text(w3,/editable,xsize=10,ysize=1,value=strn(ScMin))

    w99=widget_label(w3,value='')
    w14=widget_label(w3,value='Scaling Maximum:')
    w15=widget_text(w3,/editable,xsize=10,ysize=1,value=strn(ScMax))

    w99=widget_label(w3,value=' ')
    w16=widget_button(w3,value=strn('OK',length=6,padtype=2))
    w17=widget_button(w3,value=strn('CANCEL',length=6,padtype=2))

    rdtypes=['NONE','LOG','ROOT 2','ROOT 3','ROOT 5','SINE','TVSCL','TV'] & i=0
    while (ScTyp ne rdtypes(i)) do i=i+1
    widget_control,wbut(i),/set_button

    widget_control,w1,/realize,/show

    Ret_Val=-599 & butval=i
    while (Ret_Val(0) eq -599) do begin
      event=WIDGET_EVENT(w1)
      if (event.ID eq w17) then Ret_Val=-1

      for i=0,3 do if (event.ID eq wbut1(i)) then Ret_Val=200+i
      for i=0,7 do if (event.ID eq wbut(i)) then butval=i

      if (event.ID eq w16) then begin
        widget_control,w13,get_value=tmp1 & tmp1=tmp1(0)
        if (strnumber(tmp1,tmp) eq 1) then tmp1=float(tmp) $
        else begin
          widget_control,w13,set_value='Invalid'
          wait,1 & widget_control,w13,set_value='' & tmp1='Bad'
          endelse

        widget_control,w15,get_value=tmp2 & tmp2=tmp2(0)
        if (strnumber(tmp2,tmp) eq 1) then tmp2=float(tmp) $
        else begin
          widget_control,w15,set_value='Invalid'
          wait,1 & widget_control,w15,set_value='' & tmp2='Bad'
          endelse

        sz=size(tmp1)>size(tmp2)
        if (sz(1) ne 7) then begin
          if (tmp2 le tmp1) then begin
            widget_control,w15,set_value='too small'
            wait,1 & widget_control,w15,set_value='' & tmp2='Bad' & endif $
          else begin & var1=[tmp1,tmp2] & Ret_Val=1 & endelse
          endif
        endif
      endwhile

    widget_control,w1,/destroy
    var2=rdtypes(butval)
    return
    endif

; **** Main Screen Settings Menu *********************************************
  if (command eq 'IR_Settings') then begin
    stat=var1
    w0=widget_base(/row,title='IMGRoam Settings/Defaults',xoffset=200,yoffset=200)
    w1=widget_base(w0,/column)
    w100=widget_base(w0,/column)

    w2=widget_base(w1,/column,/frame)
    w25=widget_base(w2,/column,/nonexclusive)
    w3=widget_button(w25,value='Automatic Frame Display after Actions')
    w4=widget_button(w25,value='Frame Processing')
    w38=widget_base(w2,/row)
    w39=widget_label(w38,value='Frame Size: ')
    w40=widget_text(w38,/editable,value='Not Changeable Yet')
    w41=widget_base(w2,/row)
    w42=widget_label(w41,value='Frame LowerLeft Offset: ')
    w43=widget_text(w41,/editable,value='Not Changeable Yet')
    w26=widget_button(w25,value='Frame Border Display')
    w27=widget_base(w2,/row)
    w28=widget_label(w27,value='Frame Border Color: ')
    w29=widget_text(w27,/editable,value=strn(stat.BCOL))
    w30=widget_base(w2,/row)
    w31=widget_label(w30,value='Frame Border Thickness: ')
    w32=widget_text(w30,/editable,value=strn(stat.BTHK))

    w5=widget_base(w100,/column,/frame)
    w10=widget_base(w5,/column,/nonexclusive)
    w6=widget_button(w10,value='Automatic Color Bar Display')
    w7=widget_base(w5,/row)
    w8=widget_label(w7,value='Color Bar Length: ')
    w9=widget_text(w7,/editable,value=strn(stat.CBX))
    w11=widget_base(w5,/row)
    w12=widget_label(w11,value='Color Bar Height: ')
    w13=widget_text(w11,/editable,value=strn(stat.CBY))
    w14=widget_base(w5,/row)
    w15=widget_label(w14,value='Color Bar X,Y Offset: ')
    w16=widget_text(w14,/editable,value=strn(stat.CBLLX)+'  '+strn(stat.CBLLY))
    w17=widget_base(w5,/column,/nonexclusive)
    w18=widget_button(w17,value='Color Bar Border')
    w19=widget_base(w5,/row)
    w20=widget_label(w19,value='Color Bar Border Color: ')
    w21=widget_text(w19,/editable,value=strn(stat.CBBCOL))
    w22=widget_base(w5,/row)
    w23=widget_label(w22,value='Color Bar Border Thickness: ')
    w24=widget_text(w22,/editable,value=strn(stat.CBBTHK))

    w33=widget_base(w1,/column,/frame)
    w34=widget_label(w33,value='Zoom Type')
    w37=widget_base(w33,/column,/exclusive)
    w35=widget_button(w37,value='Bilinear Interpolation [IDL REBIN]')
    w36=widget_button(w37,value='Standard Block Expansion [IDL CONGRID]')
    w60=widget_button(w37,value='Cubic Interpolation [IDL CONGRID(,/CUBIC)]')

    w47=widget_base(w1,/column,/frame)
    w48=widget_base(w47,/column,/nonexclusive)
    w49=widget_button(w48,value='Automatic NE Arrow Display')

    w44=widget_base(w1,/row)
    w45=widget_button(w44,value='   OK   ')
    w46=widget_button(w44,value='   CANCEL   ')

    w50=widget_base(w100,/row)
    w51=widget_button(w50,value='Centroider...')
    w52=widget_button(w50,value='   ????   ')

    widget_control,w0,/realize
    if (stat.AUTOD eq 1) then widget_control,w3,set_button=1
    if (stat.AUTOPROC eq 1) then widget_control,w4,set_button=1
    if (stat.AUTOB eq 1) then widget_control,w26,set_button=1
    if (stat.CBAUTOD eq 1) then widget_control,w6,set_button=1
    if (stat.CBAUTOB eq 1) then widget_control,w18,set_button=1
    if (stat.ZTYPE eq 0) then widget_control,w35,set_button=1
    if (stat.ZTYPE eq 1) then widget_control,w36,set_button=1
    if (stat.ZTYPE eq 2) then widget_control,w60,set_button=1
    if (stat.NEARR eq 1) then widget_control,w49,set_button=1

    Ret_Val=-599
    while (Ret_Val eq -599) do begin
      event=WIDGET_EVENT(w0)
      if (event.ID eq w3) then stat.AUTOD=abs(stat.AUTOD-1)
      if (event.ID eq w4) then stat.AUTOPROC=abs(stat.AUTOPROC-1)
      if (event.ID eq w26) then stat.AUTOB=abs(stat.AUTOB-1)
      if (event.ID eq w6) then stat.CBAUTOD=abs(stat.CBAUTOD-1)
      if (event.ID eq w18) then stat.CBAUTOB=abs(stat.CBAUTOB-1)
      if (event.ID eq w35) then stat.ZTYPE=0
      if (event.ID eq w36) then stat.ZTYPE=1
      if (event.ID eq w60) then stat.ZTYPE=2
      if (event.ID eq w49) then stat.NEARR=abs(stat.NEARR-1)
      if (event.ID eq w51) then IR_cntrd,mode=1
      if (event.ID eq w45) then Ret_Val=1
      if (event.ID eq w46) then Ret_Val=-1
      endwhile
    if (Ret_Val eq -1) then begin & widget_control,w0,/destroy & return & endif

    widget_control,w29,get_value=tmp
    if (strnumber(tmp(0),tmp2) eq 1) then stat.BCOL=fix(tmp2)<255>0
    widget_control,w32,get_value=tmp
    if (strnumber(tmp(0),tmp2) eq 1) then stat.BTHK=fix(tmp2)<10>1
    widget_control,w21,get_value=tmp
    if (strnumber(tmp(0),tmp2) eq 1) then stat.CBBCOL=fix(tmp2)<255>0
    widget_control,w24,get_value=tmp
    if (strnumber(tmp(0),tmp2) eq 1) then stat.CBBTHK=fix(tmp2)<10>1
    widget_control,w9,get_value=tmp
    if (strnumber(tmp(0),tmp2) eq 1) then stat.CBX=fix(tmp2)<512>0
    widget_control,w13,get_value=tmp
    if (strnumber(tmp(0),tmp2) eq 1) then stat.CBY=fix(tmp2)<512>0
    widget_control,w16,get_value=tmp & tmp2=getopt(tmp(0))
    if (n_elements(tmp2) eq 2) then begin 
      stat.CBLLX=fix(tmp2(0))<512>0
      stat.CBLLY=fix(tmp2(1))<512>0
      endif

    widget_control,w0,/destroy
    var1=stat
    return
    endif

  return
end
