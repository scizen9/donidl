pro IR_PrintFrame,image

; This procedure handles printing of the current frame
; in the IMGroam environment.  It is not useful by itself.

  COMMON fparm,NAXIS1,NAXIS2,xsize,ysize,xcent,ycent,xll,yll,zoom,frtyp
  COMMON fparm2,xso,yso,xmin,ymin
  COMMON IR_ENVIR,stat,itype
  COMMON frpc,scmin,scmax,rdtyp
  COMMON Windows,FrameWIN,CmpressWIN
  COMMON IR_ASTROM,astrom_type,hdr,astr,gsa

  w1=widget_base(title='Print Frame (PostScript)',xoffset=200,yoffset=400,/row)
  w2=widget_base(w1,/column)
  w3=widget_base(w1,/column)
  w4=widget_base(w1,/column)

  w10=widget_label(w2,value='Color Type:')
  w11=widget_base(w2,/column,/exclusive)
  w1but=lonarr(5)
  w1but(0)=widget_button(w11,value='White Sky')
  w1but(1)=widget_button(w11,value='Black Sky')
  w1but(2)=widget_button(w11,value='Color Table')

  w20=widget_label(w3,value='Image Type:')
  w21=widget_base(w3,/column,/exclusive)
  w2but=lonarr(5)
  w2but(0)=widget_button(w21,value='Image Only')
  w2but(1)=widget_button(w21,value='As on Screen')
  w2but(2)=widget_button(w21,value='With Header Portions')

  w30=widget_label(w4,value='Output:')
  w31=widget_base(w4,/column,/exclusive)
  w3but=lonarr(5)
  w3but(0)=widget_button(w31,value='Default Printer')
  w3but(1)=widget_button(w31,value='Select Printer')
  w3but(2)=widget_button(w31,value='File Only')

  w90=widget_base(w1,/column)
  w91=widget_button(w90,value=strn('OK',length=6,padtype=2))
  w92=widget_button(w90,value=strn('CANCEL',length=6,padtype=2))

  widget_control,w1but(0),/set_button & ctype=0
  widget_control,w2but(2),/set_button & itype=2
  widget_control,w3but(0),/set_button & otype=0

  widget_control,w1,/realize,/show

  Ret_Val=-599
  while (Ret_Val(0) eq -599) do begin
    event=WIDGET_EVENT(w1)
    if (event.ID eq w92) then Ret_Val=-1
    if (event.ID eq w91) then Ret_Val=0
    for i=0,2 do if (event.ID eq w1but(i)) then ctype=i
    for i=0,2 do if (event.ID eq w2but(i)) then itype=i
    for i=0,2 do if (event.ID eq w3but(i)) then otype=i
    endwhile

  widget_control,w1,/destroy
  if (Ret_Val eq -1) then return

  if (itype eq 0) then createf,image,tmp,1,1
  if (itype eq 1) or (itype eq 2) then begin
    wset,FrameWIN & tmp=tvrd()
    endif
  inv=0 & if (ctype eq 0) then inv=1
  auto=0 & if (otype eq 0) then auto=1
  if (otype eq 1) then auto=2
  colr=0 & if (ctype eq 2) then colr=1
  dontclose=0 & if (itype eq 2) then dontclose=1

  if (colr eq 0) then tmp=bytscl(tmp,0,!d.n_colors-1<255)

  psout,tmp,inv=inv,auto=auto,dontclose=dontclose,color=colr & tmp=1

  if (dontclose eq 1) then begin
    pfont=!p.font
    !p.font=0 & device,/helv

    xyouts,.5,1.3,/norm,'IMGroam: '+getenv('USER')+' '+!stime & i=2
    xyouts,.5,1.3-i*.03,/norm,'Stretch: '+rdtyp+vect([scmin,scmax]) & i=i+1
    xyouts,.5,1.3-i*.03,/norm,'Center: '+vect([xcent,ycent]) & i=i+1
    if (astrom_type ne 'NONE') then begin
      Coords=1 & IR_AstDisp,xcent,ycent,ra,dec,0,RetCoords=Coords
      tmpCoords=strmid(Coords(1),40,29)
      if (sxpar(hdr,'EQUINOX') ne 0) then $
        tmpCoords=tmpCoords+'   '+vect([sxpar(hdr,'EQUINOX')])
      xyouts,.5,1.3-i*.03,/norm,'Coords: '+tmpCoords & i=i+1
      if (astrom_type eq 'STD') then $
        tmp11=sqrt(astr.cd(0,0)^2+astr.cd(0,1)^2)*3600 $
        else tmp11=1.7
      xyouts,.5,1.3-i*.03,/norm,'Pixel Size: '+ $
	strn(tmp11,format='(f10.3)')+"''" & i=i+1
      tmp11=tmp11*512/zoom
      if (tmp11 lt 60) then begin
	xyouts,.5,1.3-i*.03,/norm,'Field Size: '+ $
	  strn(tmp11,format='(f10.2)')+"''" & i=i+1 & $
      endif else begin
	xyouts,.5,1.3-i*.03,/norm,'Field Size: '+ $
	  strn(tmp11/60,format='(f10.3)')+"'" & i=i+1
	endelse
      endif
    xyouts,.5,1.3-i*.03,/norm,'Zoom: '+strn(fix(zoom))+'x' & i=i+1

    interest=['OBJECT','DATE-OBS','ORIGIN','RA','DEC', $
      'EPOCH','DETECTOR','EXPTIME','IRAFNAME','UT']

    tmp=sxpar(hdr,'PEP_EXPO')		; HST WF/PC Images
    if (!ERR ge 0) then begin
      interest=['TARGNAME','RA_TARG','DEC_TARG','PEQUINOX','FILTNAM1', $
        'ROOTNAME','DATE-OBS','EXPTIME','PHOTMODE']
      endif

    tmp=sxpar(hdr,'JOTFID')		; UIT Images
    if (!ERR ge 0) then begin
      interest=['OBJECT','OBJECT2','CRVAL1','CRVAL2','ROLL', $
        'IMAGE','DATE-OBS','EXPTIME','BSCALE','JOTFID']
      endif

    tmp=sxpar(hdr,'PPO1')		; GSSS Plates
    if (!ERR ge 0) then begin
      interest=['OBJECT','DATE','ORIGIN','MAGLIM','SEEING', $
        'EPOCH','EXPOSURE','PLATEID','REGION','PLTLABEL']
      endif

    tmp=sxpar(hdr,'XS-SEQPI')		; ROSAT Images
    if (!ERR ge 0) then begin
      interest=['OBJECT','DATE-END','TELESCOP','INSTRUME','EQUINOX', $
        'XS-OBSID','XS-SEQPI','XS-LIVTI','XS-MINCH','XS-MAXCH']
      endif

    tmp=sxpar(hdr,'GRATING4')		; DIS Images
    if (!ERR ge 0) then begin
      interest=['OBJNAME','DATE-OBS','TELESCOP','INST','UT', $
        'EXPOSURE','FILTER1','FILTER2','TURRET','RA','DEC']
      endif

    hght=0
    for i=0,n_elements(interest)-1 do $
      xyouts,0,1.3-.03*i,interest(i)+'='+strn(sxpar(hdr,interest(i))),/norm

    psclose,auto=auto
    !p.font=pfont
    endif


  return
end
