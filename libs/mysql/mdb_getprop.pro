;+
; NAME: mdb_getprop 
;
;	get a property from the database.
;
; INPUTS:
; 	name		name of object
; 	property	what property to search for (e.g. "Distance","Spectral Type",
; 				etc.)
; KEYWORDS:
; 	/recent		sort by most recent first (approximately)
; 	/one		only return one value, no matter how many are in the DB
; 	/log		Also check for log of the property; i.e. Teff will also find
; 				logTeff. Answers will all be converted into linear units
; 	/nan		return NaN in event of an error or not finding any match
; OUTPUTS:
; 	returns the value
; 	ERROR=		output keyword for error in value
; 	REFERENCE=	output keyword for reference for value
;
; HISTORY:
; 	Began 2006-05-10 00:57:03 by Marshall Perrin 
; 	2006-05-24	added /log option
;-


FUNCTION mdb_getprop,name0,property,error=error,reference=reference,$
	recent=recent,nostop=nostop,one=one,logtoo=logtoo,nan=nan

	common mysql, SQLhandle
	mysqlcheck,SQLhandle

	name = mdb_nameresolve(name0)

	if keyword_set(logtoo) then logstr = ' or property='+quote("log"+property) else logstr=''

	q = "select name,property,value,value_error,reference from otherproperties "+$
		"where name = "+quote(name0)+" and (property="+quote(property)+logstr+");"
		;print,q
	mysqlquery,SQLhandle,q,name2,property2,value,error,reference,ngood=count
	
	if count eq 0 then begin
		if keyword_set(nostop) then if keyword_set(nan) then return, !values.f_nan else return,'0'
		message,"Couldn't find any "+property+" for "+name+"!"
	endif 
;	forprint,name,property,value

	if keyword_set(logtoo) then begin
		wlog = where(property2 eq "log"+property,wcnt)
		value0 = value
		if wcnt gt 0 then begin 
			value[wlog] = 10.^value[wlog]
			property2[wlog] = property
			;TODO deal with errors
		endif

	endif


	if keyword_set(recent) then begin 
		; sort things by year of reference
		years = intarr(count)
		for i=0L,count-1 do begin
			year = stregex(reference[i],"[1-2][0-9][0-9][0-9]",/extract)
			if year eq "" then year = "9999"
				years[i] = year
		endfor 
		s = reverse(sort(years))
		value = value[s]
		error = error[s]
		reference=reference[s]
	end

	if keyword_set(one) then begin
		value=value[0]
		error=error[0]
		reference=reference[0]
	endif

	return,value
	

end
