
  month=1
  day=1

  year=1996 & NDAYS=366
  year=1997 & NDAYS=365
  year=1998 & NDAYS=365
  year=1999 & NDAYS=365
  year=2000 & NDAYS=366
  year=2001 & NDAYS=366

  latitude=32.78036d
  longitude=-105.82042d
  mnthTZ=[0,-7,-7,-7,-6,-6,-6,-6,-6,-6,-7,-7,-7]
  aprflag=0 & octflag=0

  data=fltarr(7,NDAYS)
  eventZD=[90.83,101,108,108,101,90.83]

  for iday=0,NDAYS-1 do begin
    jul_date=julday(month,day,year)
    timezone=mnthTZ(month)
    if (month eq 4) and (aprflag eq 0) then begin
      wd=weekday(year,month,day,nwd)
      if (nwd eq 1) then begin
        aprflag=1
        print,'Switching from timezone -7 to -6 on ',wd,' ',vect([year,month,day])
        endif $
      else timezone=-7
      endif
    if (month eq 10) and (octflag eq 0) then begin
      wd=weekday(year,month,day,nwd)
      if (nwd eq 1) and (day ge 25) then begin
        octflag=1
        print,'Switching from timezone -6 to -7 on ',wd,' ',vect([year,month,day])
        endif $
      else timezone=-6
      endif

    NPTS=18*2
    sZDs=fltarr(NPTS) & UTs=sZDs
    for j=0,NPTS-1 do begin
      UTs(j)=-2+j/2.0
;      ct2lst,lst,-1.0*longitude,timezone,jul_date-0.5d + UTs(j)/24.0d
      ct2lst,lst,longitude,timezone,jul_date-0.5d + UTs(j)/24.0d
      sunpos,jul_date-0.5d + UTs(j)/24.0d,sra,sdec
      calc_ZDHA,lst,latitude,longitude,sra,sdec,sZD,sHA,sAZ
      sZDs(j)=sZD
      endfor

    for event=0,2 do data(event,iday)=interpol(UTs(0:NPTS/2),sZDs(0:NPTS/2),[eventZD(event)])
    for event=3,5 do data(event,iday)=interpol(reverse(UTs(NPTS/2:NPTS-1)), $
      reverse(sZDs(NPTS/2:NPTS-1)),[eventZD(event)])
    data(6,iday)=timezone

    jul_date=jul_date+1
    caldat,jul_date,month,day,year
    endfor

    plot,data(0,*),xsty=1,yr=[-0.5,15],ysty=1
    oplot,data(1,*)
    oplot,data(2,*)

    oplot,data(3,*)
    oplot,data(4,*)
    oplot,data(5,*)


    if (1 eq 1) then begin
      openw,1,'suntwilight'+strn(year-1)+'.dat'
      printf,1,data,format='(6f8.3,i4)'
      close,1
      endif

    plot,data(0,*)+data(6,*)+24,xsty=1,yr=[15,35],ysty=1
    oplot,data(5,*)+data(6,*)+24

end


