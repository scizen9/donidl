pro calc_ZDHA,lst,latitude,longitude,ra,dec,ZD,HA,AZ
;+
; NAME:
;   calc_ZDHA
; PURPOSE:
;   Calculate Zenith distance and Hour Angle and Azimuth given local standard
;   time, latitude, longitude, ra, and dec.
; CALLING SEQEUNCE:
;   calc_ZDHA,lst,latitude,longitude,ra,dec,ZD,HA,AZ
;-

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
