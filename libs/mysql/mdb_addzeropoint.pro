;+
; NAME: mdb_addzeropoint 
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-05-26 13:57:27 by Marshall Perrin 
;-

PRO mdb_addzeropoint, name,inst,UTC,band,zp,zp_err,comment=comment
	
	common mysql, SQLhandle
	mysqlcheck,SQLhandle

	if ~(keyword_set(comment)) then comment="NULL"

	date_utc = strepex(UTC,"T"," ") ; convert to Mysql-accepted format

	q = "replace into zeropoints (name,datetime,band,instrument,comment,zeropoint,zeropoint_err)"+$
		"values ("+quote(name)+","+quote(date_utc)+","+quote(band)+","+quote(inst)+","+quote(comment)+","+strc(zp,/nan)+","+strc(zp_err,/nan)+");"
	mysqlcmd,sqlhandle,q

	message,/info,"Recorded zeropoint for "+inst+", "+band+" on date "+date_utc+"."


end
