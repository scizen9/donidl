;+
; NAME:
;   SKYHORIZ
;
; PURPOSE:
;   This program is designed to show how a given object (i.e. RA, DEC)
;   tracks across the sky during a specified night.  The program gives
;   two forms of output: 1) a table listing Zenith Distance, Airmass,
;   Hour Angle, UT, local time, and positions of the moon as well during
;   a specified night; and 2) a plot of how the object and the moon
;   track across the sky.  Time interval is always 6pm to 6am local time.
;
; CALLING SEQEUNCE:
;   SkyPos
;
; INPUT:
;   None.  All required information is prompted for.
;
; OUTPUT:
;   A table and a plot.  See description above
;
; EXAMPLE:
;
; HISTORY:
;   17-FEB-94 Version 1 written by Eric W. Deutsch
;-

pro get_dbl,prompt1,dblval

GD_AGAIN:
  inp=' '
  if (n_elements(dblval) eq 0) then dblval=0.0d
  dblval=dblval*1.0d
  prompt=prompt1+': ['+strn(dblval)+']'
  read,prompt+'  ',inp
  if (inp eq '') then return
  if (strnumber(inp) eq 0) then begin
    print,'  ** Invalid number ** ' & goto,GD_AGAIN & endif
  dblval=double(inp)
  return

end


; =========================================================================
; The calculations are from "Astronomical Photometry" by Henden & Kaitchuck

pro calc_ZDHA,lst,latitude,longitude,ra,dec,ZD,HA,AZ

  H = 15.0d * ( lst - ra/15.0d )

  sin_h1 = sin(latitude/!radeg) * sin(dec/!radeg) + $
	   cos(latitude/!radeg) * cos(dec/!radeg) * cos(H/!radeg)
  h1 = asin(sin_h1)*!radeg

  cos_A = ( sin(dec/!radeg) - sin(latitude/!radeg) * sin(h1/!radeg) ) / $
	  ( cos(latitude/!radeg) * cos(h1/!radeg) )
  AZ = acos(cos_A)*!radeg

  HA=H/15 & if (HA lt -12) then HA=HA+24
  if (HA gt 12) then HA=HA-24

  if ( HA gt 0 ) then AZ = 360.0 - AZ

  ZD = 90 - h1

  return
end

; ==========================================================================

pro skyhoriz,dummy

  print,'WARNING: This program was intended for star position observer estimation.'
  print,'         Accuracy may not be sufficient for calibration purposes.'
  print,' '

  COMMON AST_POS_COMM,latitude,longitude,timezone,year,month,day,radec,equinox

  if (n_elements(latitude) eq 0) then latitude=32.780d
  if (n_elements(longitude) eq 0) then longitude=-105.820d
  if (n_elements(timezone) eq 0) then timezone=6.0d
  if (n_elements(year) eq 0) then year=1995.0d
  if (n_elements(month) eq 0) then month=1.0d
  if (n_elements(day) eq 0) then day=1.0d
  if (n_elements(radec) eq 0) then radec='0.00 0.00'
  if (n_elements(equinox) eq 0) then equinox=2000.0d

; ========= Select Observatory =====================================================
  print,'Selected Observatories: (from Almanac)'
  print,'  MRO              lat= 46.952   lon=-120.723    timezone=7'
  print,'  DAO              lat= 48.520   lon=-123.417    timezone=7'
  print,'  APO              lat= 32.780   lon=-105.820    timezone=6'
  print,'  KPNO             lat= 31.963   lon=-111.600    timezone=6'
  print,'  CTIO             lat=-30.165   lon= -70.815    timezone=4'
  print,' '

  get_dbl,'Enter Latitude',latitude
  get_dbl,'Enter Longitude',longitude
  get_dbl,'Enter Timezone',timezone

  get_dbl,'Enter Year',year
  get_dbl,'Enter Month(1-12)',month
  get_dbl,'Enter UT Day beginning during Obs. Night(1-31)',day

RADECAGAIN:
  inp=radec
  print,'You may use or Sexigesimal or Decimal format'
  read,'  Enter RA and DEC: ['+strn(radec)+']  ',inp
  if (inp ne '') then radec=inp
  Coord=getopt(radec,'F')
  case (n_elements(Coord)) of
    2: begin & ra=Coord(0) & dec=Coord(1) & end
    6: begin & stringad,radec,ra,dec & end
    else: begin & print,'  ** Invalid **' & goto,RADECAGAIN & end
    endcase
  ra=ra*1.0d & dec=dec*1.0d

  get_dbl,'Enter Equinox of these coordinates',equinox
  eqcur=year+(month-1)/12+day/365
  precess,ra,dec,equinox,eqcur

  jdcnv,year,month,day,0.0d,jul_date

  print,' ' & print,' '
  print,'Object at '+adstring(ra,dec,2)+'   (Eq '+strn(eqcur,format='(f15.4)')+')'

  print,'Julian date at 0 UT:  ',strn(jul_date,format='(f15.4)')
  print,' '

  print,'Locl Tim    UT       ST     Zen Dst  Airmass  Hr Angl  Azimuth'
  print,'--------  ------  --------  -------  -------  -------  -------'

  posdata=fltarr(4,13)

  data=fltarr(12,13)

for dd=0,11 do begin
  i=0
  for localtime=-6.0d,6.0d do begin

    UT=localtime+timezone

;    ct2lst,lst,-1.0*longitude,timezone,jul_date+UT/24.0d
    ct2lst,lst,longitude,timezone,jul_date+UT/24.0d

    calc_ZDHA,lst,latitude,longitude,ra,dec+dd*10,ZD,HA,AZ
    sec_z=1/cos((ZD<88.0d)/!radeg)
    AM = sec_z - 0.0018167d * ( sec_z - 1 ) - 0.002875 * ( sec_z - 1 )^2 - $
	 0.0008083d * ( sec_z - 1 )^3

    lt1=localtime & if (lt1 lt 0) then lt1=24+lt1

    print,format='(f8.2,f8.2,f10.4,f9.3,f9.4,f9.3,f9.3)', $
      lt1,UT,lst,ZD,AM,HA,AZ

    data(dd,i)=AZ

    i=i+1
    endfor

  print,' '
  endfor


  plot,indgen(10),/nodata,xr=[-7,7],yr=[-40,90],xsty=1,ysty=1, $
    tit='Direction to horizon',xtit='Hour Angle',ytit='Declination'

  for i=0,11 do begin
    for j=0,12 do begin
      x0=j-6 & y0=-30+i*10
      ang=data(i,j)
      if (abs(data(i,6)-180) lt 5) then ang=ang-180
      if (abs(data(i,6)-360) lt 5) then begin
        ang=ang-360
        ang=ang+180
        endif

      ang=ang-90
      print,ang
      ang=ang/!radeg

      arrow,x0,y0,x0+cos(ang),y0+sin(ang)*10,/data

      ang=ang*!radeg
      if (ang lt -180) then ang=ang+360
      xyouts,x0,y0,align=0.5,charsize=.7,strn(fix(ang+.5))
      endfor
    endfor

  return

end
