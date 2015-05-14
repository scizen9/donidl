;+
; NAME: mdb_addproperty
;
;	Add a property for an object into the database
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-04-14 15:37:20 by Marshall Perrin 
;	2006-04-26 00:56:40  forked from mdb_addflux
;
;-

PRO mdb_addproperty,name,property,value,value_error=value_error,reference,date=date,$
		comment=comment



	common mysql, SQLhandle
	mysqlcheck,SQLhandle

	; for the input values:
	;    if present and string, we need to quote them
	;    if present and non-string, we need to make them strings
	;    if not present, set to null
	;
	; To pass in a NULL parameter from outside, the easiest way is to pass in
	; !values.f_nan

	if n_elements(value) eq 0  then value1="NULL" else $
	if size(value,/TNAME) ne "STRING" then value1 = strc(value,/nan) else value1=quote(value)
	if n_elements(value_error) eq 0  then value_error1="NULL" else $
	if size(value_error,/TNAME) ne "STRING" then value_error1 = strc(value_error,/nan) else value_error1=quote(value_error)
	if ~(keyword_set(date)) then date1="NULL" else $
	if size(date,/TNAME) ne "STRING" then date1 = strc(date,/nan) else date1=quote(date)
	if ~(keyword_set(comment)) then comment1="NULL" else $
	if size(comment,/TNAME) ne "STRING" then comment1 = strc(comment,/nan) else comment1=quote(comment)


	
		c=","
		q = "replace into otherproperties (name,property,value,value_error,Reference,comment,date) values ("+$
			quote(name)+c+quote(property)+c+value1+c+value_error1+c+$
			quote(reference)+c+comment1+c+date1+");"
;			print,q
		mysqlcmd,SQLhandle,q,/debug

end
