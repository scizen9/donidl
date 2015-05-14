;+
; NAME: mdb_addflux
;
;	Add a flux for an object into the database
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-04-14 15:37:20 by Marshall Perrin 

;-

PRO mdb_addflux,name,bandname,mag,mag_error,flux,flux_err,Reference,date=date,$
		comment=comment


	common mysql, SQLhandle
	mysqlcheck,SQLhandle

	if ~(keyword_set(date)) then date='1000-01-01' ; can't be null since this is now a key column
	if ~(keyword_set(comment)) then comment="NULL"

		c=","
		q = "replace into photometry (name,band,magnitude,mag_error,flux,flux_error,Reference,date,comment) values ("+$
		;q = "insert ignore into photometry (name,band,magnitude,mag_error,flux,flux_error,Reference,date,comment) values ("+$
			quote(name)+c+quote(bandname)+c+strc(mag,/nan)+c+strc(mag_error,/nan)+c+$
			strc(flux,/nan)+c+strc(flux_err,/nan)+c+quote(Reference)+c+quote(date)+c+comment+");"
			;print,q
		mysqlcmd,SQLhandle,q,/debug

end
