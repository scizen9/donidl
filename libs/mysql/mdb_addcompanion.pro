;+
; NAME: mdb_addcompanion 
;
;		Add a companion star to the database. 
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-08-03 17:37:02 by Marshall Perrin 
;-

PRO mdb_addcompanion,primary,secondary,separation, PA, band=band,dmag=delta_mag,date=date,$
	comment=comment, dx=dx,dy=dy,pixelscale=pixelscale,reference=reference

	if keyword_set(dx) and keyword_set(dy) then begin
		if ~(keyword_set(pixelscale)) then pixelscale=1.0
		PA = -!radeg*atan(dx,dy)
		separation = sqrt(dx^2+dy^2)*pixelscale
	endif

	if PA lt 0 then PA += 360.

	common mysql, SQLhandle
	mysqlcheck,SQLhandle
	if ~(keyword_set(date)) then date='1000-01-01' ; can't be null since this is now a key column
	if ~(keyword_set(comment)) then comment="NULL"
	if ~(keyword_set(band)) then band="NULL"
	if ~(keyword_set(delta_mag)) then delta_mag="NULL"
		c=","
	
	q = "replace into companions (name, secondary, reference, separation, pa,band, delta_mag, date,comment) values ("+$
		quote(primary)+c+quote(secondary)+c+quote(reference)+c+strc(separation)+c+strc(PA)+c+quote(band)+c+strc(delta_mag)+c+date+c+quote(comment)+");"
	mysqlcmd,SQLhandle,q,/debug


end
