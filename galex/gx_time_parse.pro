function gx_time_parse,tstr
;+
;	gx_time_parse	- parse a string from galex timing files
;
; INPUTS:
;	tstr	- a string containing a line from the timing file
;
; RETURNS:
;	4 element double precision array with:
;	1) - timestamp
;	2) - avtec time
;	3) - spacecraft time
;	4) - spacecraft, avtec offset
; HISTORY:
;	written by jdn 14-JAN-2010
;-
istr = tstr	; copy to avoid changing input string
;
timestamp 	= gettok(istr,',')
avtec		= gettok(istr,',')
spacecraft	= gettok(istr,',')
toff		= gettok(istr,',')
if strlen(strtrim(toff,2)) gt 0 then $
	toff	= double(toff) $
else	toff	= !values.d_nan
;
; process timestamp
yr	= fix(gettok(timestamp,':'))
dn	= fix(gettok(timestamp,':'))
ydn2md, yr, dn, mo, dy
hr	= fix(gettok(timestamp,':'))
mn	= fix(gettok(timestamp,':'))
sc	= double(gettok(timestamp,':'))
ts	= julday(mo,dy,yr,hr,mn,sc)
;
; process avtec
if strlen(strtrim(avtec,2)) gt 0 then begin
	yr	= fix(gettok(avtec,':')) + 2000
	dn	= fix(gettok(avtec,':'))
	ydn2md, yr, dn, mo, dy
	hr	= fix(gettok(avtec,':'))
	mn	= fix(gettok(avtec,':'))
	sc	= double(gettok(avtec,':'))
	at	= julday(mo,dy,yr,hr,mn,sc)
endif else at	= !values.d_nan
;
; process spacecraft
if strlen(strtrim(spacecraft,2)) gt 0 then begin
	yr	= fix(gettok(spacecraft,':')) + 2000
	dn	= fix(gettok(spacecraft,':'))
	ydn2md, yr, dn, mo, dy
	hr	= fix(gettok(spacecraft,':'))
	mn	= fix(gettok(spacecraft,':'))
	sc	= double(gettok(spacecraft,':'))
	sc	= julday(mo,dy,yr,hr,mn,sc)
endif else sc	= !values.d_nan
;
return,[ts,at,sc,toff]
end
