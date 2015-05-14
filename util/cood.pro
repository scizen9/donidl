pro cood,str,rad,decd,silent=silent,plcstr=plcstr
;+
;	COOD - read in ra, dec and return decimal degrees
;
; INPUTS:
;
;	str	- input string containing coordinates, expects three tokens 
;		  per coord, but can be delimited by either spaces or colons
;
; OUTPUTS:
;
;	rad,decd- ra, dec in decimal degrees
;
; KEYWORDS:
;
;	silent	- set this to prevent printing results
;	plcstr	- position as an spoint string with
;			format: "(<ra_radians>, <dec_radians>)"
;
; PROCEDURE:
;
; HISTORY:
;
;	28sep07 jdn	- Initial Revision
;
;-
;
; initialize
rad=-9.0
decd=-99.0
plcstr=''
;
; check inputs
if n_params(0) le 0 then begin
	print,'Usage - cood,coo_str,radeg,decdeg'
	return
endif
;
; make local copy
tmp=str
;
; trim leading designators
p=0
while p lt strlen(tmp) do begin
	if strmatch(strmid(tmp,p,1),'[0-9]') then break
	p=p+1
endwhile
tmp = strmid(tmp,p)
;
; loop through tokens
d1=''
if strpos(tmp,':') ge 0 then $
	d1=':' $
else	if strpos(tmp,' ') ge 0 then $
	d1=' '
if strpos(tmp,'-') ge 0 then $
	sgn = -1.d0 $
else	sgn = 1.d0
if strlen(d1) eq 1 then begin
	rah  = fix(gettok(tmp,d1))
	ram  = fix(gettok(tmp,d1))
	ras  = float(gettok(tmp,' '))
	dd   = abs(fix(gettok(tmp,d1)))
	dm   = fix(gettok(tmp,d1))
	ds   = float(gettok(tmp,' '))
endif else begin
;
; split ra and dec
	p	= stregex(tmp,'[-+]')
	if p lt 0 then begin
		if not keyword_set(silent) then $
			print,'Error: badly formed coord string: ',str
		return
	endif
	rastr	= strmid(tmp,0,p)
	decstr	= strmid(tmp,p+1)
;
; process RA
	ram	= 0
	ras	= 0.
	rah	= fix(strmid(rastr,0,2))
	rastr	= strmid(rastr,2)
	if strlen(rastr) ge 2 then begin
		ram	= fix(strmid(rastr,0,2))
		rastr	= strmid(rastr,2)
		ras	= float(rastr)
	endif
;
; process Dec
	dm	= 0
	ds	= 0.
	dd	= fix(strmid(decstr,0,2))
	decstr	= strmid(decstr,2)
	if strlen(decstr) ge 2 then begin
		dm	= fix(strmid(decstr,0,2))
		decstr	= strmid(decstr,2)
		ds	= float(decstr)
	endif
endelse
rad  = ten(rah,ram,ras)*15.d0
decd = ten(dd,dm,ds) * sgn
dtor = !DPI/180.d0
plcstr='('+strtrim(string(rad*dtor,format='(f17.14)'),2)+', '+ $
	   strtrim(string(decd*dtor,format='(f18.15)'),2)+')'
rstr = strtrim(string(rad, form='(f13.8)'),2)
dstr = strtrim(string(decd,form='(f13.8)'),2)
if not keyword_set(silent) then $
	print,'  ',rstr,',',dstr
if keyword_set(plcstr) and not keyword_set(silent) then $
	print,plcstr
;
return
end
