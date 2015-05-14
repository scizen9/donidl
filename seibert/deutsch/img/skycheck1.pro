
  year=1997
  month=5
  day=23
  UT=[1,7.0,13]
  UT2=[1.0,3,5,7,9,11,13]
  ndays=3
;  radec='06 01 47.287  +23 24 54.16'	; 0558+234
;  radec='05 11 32.621  +38 09 13.31'	; 0508+380
;  radec='11 19 00      +30 00 00'	; WD1
;  radec='14 00 00      +30 00 00'	; WD2
;  radec='15 13 22.8    -09 10 08'	; GRISM C star (and odd spec)
;  radec='08 42  4.456  +33 42 58.83'	; KUV 08389+3354
;  radec='10 0   0      +33 0  0'	; KUV 08389+3354
  radec='13 30 0 +29 0 0'	; "nicht rot" objects
  radec='17 28 0 -25 0 0'	; GX 1+4
  stringad,radec,ra,dec

  latitude=32.78036d
  longitude=-105.82042d
  timezone=7.0d

;  print,'     UT       Moon  Obj-Moon Dist             Object Airmass'
;  print,'Year  Mn  Dy  Ill%  6pm  12m  6am   6pm   8pm  10pm   12m   2am   4am   6am'
;  print,'----  --  --  ----  ---  ---  ---  ----  ----  ----  ----  ----  ----  ----'
;        1997   1   3   38%  113  116  119  2.70  1.38  1.06  1.03  1.26  2.19  9.99
  print,''
  print,''

  for i=0,ndays-1 do begin
    jul_date=julday(month,day,year)
    moonpos,jul_date-0.5d +UT/24.0d,mra,mdec
    mphase,jul_date-0.5d +UT(1)/24.0d,phmoon
    gcirc,0,mra/!radeg,mdec/!radeg,ra/!radeg,dec/!radeg,dist

    AMs=fltarr(13) & mZDs=AMs & sZDs=AMs & UTs=AMs
    for j=0,12 do begin
      UTs(j)=j+1
;      ct2lst,lst,-1.0*longitude,timezone,jul_date-0.5d +(j+1)/24.0d
      ct2lst,lst,longitude,timezone,jul_date-0.5d +(j+1)/24.0d
      calc_ZDHA,lst,latitude,longitude,ra,dec,ZD,HA,AZ
      sec_z=1/cos((ZD<87d)/!radeg)
      AMs(j) = sec_z - 0.0018167d * ( sec_z - 1 ) - 0.002875 * $
           ( sec_z - 1 )^2 - 0.0008083d * ( sec_z - 1 )^3
      moonpos,jul_date-0.5d +(j+1)/24.0d,mra,mdec
      calc_ZDHA,lst,latitude,longitude,mra,mdec,mZD,mHA,mAZ
      mZDs(j)=mZD
      sunpos,jul_date-0.5d +(j+1)/24.0d,sra,sdec
      calc_ZDHA,lst,latitude,longitude,sra,sdec,sZD,sHA,sAZ
      sZDs(j)=sZD
      endfor

    print,format='(i4,i4,i4,"  Lunar Illumination:",i5,"%   Object-Moon Distance:",3i5)', $
      year,month,day,round(phmoon*100),dist*!radeg
    print,format='("      UT Time:",13f5.1)',UTs
    print,format='("       Sun ZD:",13i5)',round(sZDs)
    print,format='("      Moon ZD:",13i5)',round(mZDs)
    print,format='(" Object Airms:",13f5.2)',AMs<9.99
    print,''

    jul_date=jul_date+1
    caldat,jul_date,month,day,year

    endfor


end











