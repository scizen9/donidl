;+
; NAME: mdb_addspectraltype
;
;	Add a spectraltype for an object into the database
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-04-14 15:37:20 by Marshall Perrin 
;	2006-04-26 00:56:40  forked from mdb_addflux
;
;-

PRO mdb_addspectraltype,name,spectraltype,spectraltype_error=spectraltype_error,reference,date=date,$
		comment=comment



	common mysql, SQLhandle
	mysqlcheck,SQLhandle

	if ~(keyword_set(spectraltype_error)) then spectraltype_error="NULL"
	if ~(keyword_set(date)) then date="NULL"
	if ~(keyword_set(comment)) then comment="NULL"

		c=","
		q = "replace into spectraltypes (name,spectraltype,spectraltype_error,Reference,comment) values ("+$
			quote(name)+c+quote(spectraltype)+c+strc(spectraltype_error)+c+$
			quote(reference)+c+comment+");"
			;print,q
		mysqlcmd,SQLhandle,q,/debug

end
