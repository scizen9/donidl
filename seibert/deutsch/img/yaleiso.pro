pro yaleiso,filecode,age,data,listfiles=listfiles,listages=listages,plot=plot

  if (n_elements(listfiles) eq 0) then listfiles=0
  if (n_elements(listages) eq 0) then listages=0
  if (n_elements(plot) eq 0) then plot=0

  if (listfiles ne 0) then begin
    files=findfile('/host/dione/u2/deutsch/ngc6441/isochrones/iso.y*',count=count)
    if (count lt 2) then begin
      print,'Error finding /host/dione/u2/deutsch/ngc6441/isochrones/iso.y*'
      return
      endif
    for i=0,count-1 do begin
      code=strmid(files(i),46,7)
      if (strmid(code,0,1) ne 'y') then begin
        print,'Error parsing code'
        return
        endif
      y=fix(strmid(code,1,2))
      z=fix(strmid(code,4,1))*10^(-1.0*fix(strmid(code,6,1)))
      print,code,'  Y=0.',strn(y),'  Z=',strn(z,format='(f10.4)'), $
        '  [Fe/H]=',strn(alog10(z/0.02),format='(f5.2)')
      endfor
    return
    endif

  if (listages ne 0) then begin
    if (n_params(0) lt 1) then begin
      print,'Must give filecode to list ages'
      goto,SHOWCALL
      endif
    filename='/host/dione/u2/deutsch/ngc6441/isochrones/iso.'+filecode
    if (not exist(filename)) then begin
      print,'Unable to open file ',filename
      goto,SHOWCALL
      endif
    openr,1,filename
    lin=''
    while not EOF(1) do begin
      readf,1,lin
      while not EOF(1) and (strmid(lin,0,1) ne '#') do readf,1,lin
      if (strmid(lin,0,1) eq '#') then begin
        tmp1=fltarr(10)
        reads,strmid(lin,1,99),tmp1
        y=tmp1(5)
        z=tmp1(6)
        age1=tmp1(4)/1000
        print,filecode,'  Y=0.',strn(y),'  Z=',strn(z,format='(f10.4)'), $
          '  [Fe/H]=',strn(alog10(z/0.02),format='(f5.2)'),'  Age(Gyr)=', $
          strn(age1,format='(f10.2)')
        endif
      endwhile
    close,1
    return
    endif

  if (n_params(0) lt 3) then begin
SHOWCALL:
    print,'Call> yaleiso,filecode,age,data,[/listfiles,/listages,/plot]
    print,"e.g.> yaleiso,/listfiles		; list filecodes, abundances"
    print,"e.g.> yaleiso,'y25z1m2',/listages	; list ages in file y25z1m2"
    print,"e.g.> yaleiso,'y25z1m2',10.0,data	; get 10Gyr isochrone in file y25z1m2"
    print,"See /host/dione/u2/deutsch/ngc6441/isochrones/iso.doc for isochrone details"
    print,""
    return
    endif


  filename='/host/dione/u2/deutsch/ngc6441/isochrones/iso.'+filecode
  if (not exist(filename)) then begin
    print,'Unable to open file ',filename
    goto,SHOWCALL
    endif

  openr,1,filename
  lin=''
  while not EOF(1) do begin
    readf,1,lin
    while not EOF(1) and (strmid(lin,0,1) ne '#') do readf,1,lin
    if (strmid(lin,0,1) eq '#') then begin
      tmp1=fltarr(10)
      reads,strmid(lin,1,99),tmp1
      y=tmp1(5)
      z=tmp1(6)
      age1=tmp1(4)/1000
      if (age eq age1) then begin
        print,filecode,'  Y=',strn(y),'  Z=',strn(z,format='(f10.4)'), $
          '  [Fe/H]=',strn(alog10(z/0.02),format='(f5.2)'),'  Age(Gyr)=', $
          strn(age,format='(f10.2)'),'  Nrows=',strn(fix(tmp1(1)))
        data=dblarr(13,tmp1(1))
        readf,1,data,format='(i3,3g10.8,g11.9,3g15.13,5g7.5)'
        close,1
        if (plot ne 0) then begin
          Mv=data(8,*)
          BmV=data(10,*)
          plot,BmV,Mv,yr=[max(Mv),min(Mv)],psym=-4,xtit='B - V',ytit='M!DV!N', $
            tit='Yale 1996 Isochrone for Y='+strn(y)+'  Z='+strn(z,format='(f10.4)')+ $
            '  [Fe/H]='+strn(alog10(z/0.02),format='(f5.2)')+'  Age(Gyr)='+ $
            strn(age,format='(f10.2)')
          endif
        return
        endif
      endif
    endwhile
  close,1
  print,'Could not find the specified age(Gyr)=',strn(age)
  return


end



;163  1.017469-0.1085160 3.4754522  3.4032762 0.000000000E+0 0.000000000E+0 0.000000000E+0 -9.999 -9.999 -9.999 -9.999 -9.999
;1231234567890123456789012345678901234567890112345678901234512345679890123412345678901234512345671234567123456712345671234567

