function search_ned,radeg,decdeg,srchrad,silent=silent,altid=altid
;+
;	search_ned - search for object in ned and return canonical name
;-
; search output file
sfil = !NED_CACHE+'ned_search.html'
; do search
name=''
altid=''
;
; check inputs
if radeg lt 0. or decdeg lt -90 or decdeg gt 90. then begin
	if not keyword_set(silent) then $
		print,'No object found in NED'
	return,name
endif
srad = srchrad > 0.05 < 0.5	; between 3 and 30 arcsec
cmd='wget "http://ned.ipac.caltech.edu/cgi-bin/objsearch?search_type=Near+Position+Search & in_csys=Equatorial & in_equinox=J2000.0 & lon='+ $
	strtrim(string(radeg,form='(f13.8)'),2) + 'D & lat=' + $
	strtrim(string(decdeg,form='(f13.8)'),2) + 'D & radius=' + $
	strtrim(string(srad,form='(f6.2)'),2) + ' & out_csys=Equatorial & out_equinox=J2000.0 & obj_sort=Distance+to+search+center & of=pre_text & zv_breaker=30000.0 & list_limit=0 & img_stamp=NO" -O '+sfil
spawn,cmd,res,eres
;
; read in file
openr,il,sfil,/get_lun
rec=''
page=''
while not eof(il) do begin
	readf,il,rec
	page=[page,rec]
endwhile
page=page(1:*)
free_lun,il
;
; check results
t=where(strpos(page,'No object found') ge 0, n)
if n gt 0 then begin
	if not keyword_set(silent) then $
		print,'No object found in NED'
endif else begin
	w = where(strpos(page,'Obj_id') ge 0, nw)
	if nw gt 0 then begin
	    w = w[0]				; get closest object
	    rec = page[w]
	    p0 = strpos(rec,'/A> ') + 4
	    istr = strmid(rec,p0,32)
	    istr = repstr(istr,'*','')		; remove asterisks
	    istr = strcompress(istr,/remove)	; remove spaces
	    ;
	    ; check for supernova
	    if strpos(istr,'SN') ne 0 then begin
		;
		; check for unusable ids (there are some)
		if strpos(istr,'[') ge 0 or strpos(istr,']') ge 0 or $
		   strpos(istr,'(') ge 0 or strpos(istr,')') ge 0 then begin
		   	altid = istr	; return as an alternate id
			if not keyword_set(silent) then $
			    print,'Unusable NED id: ',istr
		endif else name = istr
	    endif else if not keyword_set(silent) then $
		    print,'Unusable NED id: ',istr
	endif else if not keyword_set(silent) then $
		print,'No object found in NED'
endelse
;
return,name
end
