;+
; NAME:  
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-05-25 17:17:33 by Marshall Perrin 
;-

PRO mdb_recordobs,name,inst,date,band,exptime=exptime,comment=comment,dir=dir

	common mysql, SQLhandle
	mysqlcheck,SQLhandle

	if ~(keyword_set(exptime)) then exptime="NULL"
	if ~(keyword_set(comment)) then comment="NULL"
	if ~(keyword_set(dir)) then dir="NULL"

	q = "replace into observations (name,date,band,instrument,comments,exptime,directory)"+$
		"values ("+quote(name)+","+quote(date)+","+quote(band)+","+quote(inst)+","+quote(comment)+","+strc(exptime,/nan)+","+quote(dir)+");"
	mysqlcmd,sqlhandle,q



end
