pro disinfo,img,h,infostruct

  infostruct={DISinfostruct2,NAXIS1:-1,NAXIS2:-1,topbadrows:1,botbadrows:10, $
    leftspecedge:-1,rightspecedge:-1,grating:'',chip:'',gstep:0,glambda:0.0}

  s=size(img)
  if (s(0) ne 2) then begin
    print,'ERROR: disinfo: img not 2D'
    return
    endif

  infostruct.NAXIS1=s(1)
  infostruct.NAXIS2=s(2)
  infostruct.chip='RED' & chipno=2
  if (infostruct.NAXIS1 eq 586) then begin
    infostruct.chip='BLUE'
    chipno=1
    endif

  bluegratings=strarr(3)
  redgratings=strarr(3)
  bluegratings(1)=strn(sxpar(h,'GRATING1'))
  redgratings(1)=strn(sxpar(h,'GRATING2'))
  bluegratings(2)=strn(sxpar(h,'GRATING3'))
  redgratings(2)=strn(sxpar(h,'GRATING4'))
  turretstr=strn(sxpar(h,'TURRET'))
  turret=fix(strmid(turretstr,strlen(turretstr)-1,1))

  infostruct.gstep=fix(sxpar(h,'GSTEP'+strn((turret-1)*2+chipno)))
  infostruct.glambda=fix(sxpar(h,'GLAMBDA'+strn((turret-1)*2+chipno)))

  if (bluegratings(turret) eq '300:1') then infostruct.grating='low'
  if (redgratings(turret) eq '150:1') then infostruct.grating='low'
  if (bluegratings(turret) eq '600:1') then infostruct.grating='medium'
  if (redgratings(turret) eq '300:1') then infostruct.grating='medium'
  if (bluegratings(turret) eq '1200:1') then infostruct.grating='high'
  if (redgratings(turret) eq '830.8:1') then infostruct.grating='high'
  if (infostruct.grating eq '') then begin
    print,'Unable to identify grating type.  Gratings are '+ $
      bluegratings(turret)+' and '+redgratings(turret)
    print,'Assuming low res...'
    infostruct.grating='low'
    endif $
  else begin
    print,'Gratings used: '+infostruct.grating
    endelse


  if (infostruct.chip eq 'BLUE') then begin
    infostruct.leftspecedge=170
    infostruct.rightspecedge=450
    infostruct.topbadrows=1
    infostruct.botbadrows=10
    endif
  if (infostruct.chip eq 'RED') then begin
    infostruct.leftspecedge=180
    infostruct.rightspecedge=655
    infostruct.topbadrows=1
    infostruct.botbadrows=21
    endif


  return
end



