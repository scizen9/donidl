function airmass,file,ra,dec
;
; ra and dec in radians
;
common SITE, lat, lng
;
if N_elements(lat) eq 0 then read, $
       'Enter latitude and longitude (in degrees): ',lat,lng

mjd = getmjd(file,/silent)
jd = 2400000.d0 + mjd

; get LST
ct2lst, lst, lng, dummy, jd

d2rad = !DPI / 180.D0
h2rad = !DPI / 12.D0

; zenith coords
zra  = lst * h2rad	; in radians
zdec = lat * d2rad	; in radians

; get zenith distance
gcirc,0,zra,zdec,ra,dec,dist

return,1.d0/cos(dist)
end	; function airmass
