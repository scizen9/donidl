function getmjd,file,silent=silent,obsdate=obsdate
;
h = headfits(file)
;
;
det = sxpar(h,'DETECTOR')
if strtrim(det,2) eq '0' then det = sxpar(h,'CCD_ID')
req = sxpar(h,'REQUEST')
jd = sxpar(h,'JD')
scope = sxpar(h,'TELESCOP')
;
if strpos(det,'8K') ge 0 then begin	; MDM 8K?
	date = sxpar(h,'HSTTIME')
	obsdate = date
	mos  = gettok(date,' ') & mos  = gettok(date,' ')
	months = 'JanFebMarAprMayJunJulAugSepOctNovDec'
	mo = strpos(months,mos)/3 + 1
	day = fix(gettok(date,' '))
;
	hour = fix(gettok(date,':'))
	minu = fix(gettok(date,':'))
	sec  = fix(gettok(date,' '))
;
	zone = gettok(date,' ') 
	if zone eq 'MDT' then $
		hour = hour + 6 $
	else	print,'Time zone warning: ',zone
;
	year = fix(date)
;
endif else if strpos(det,'WFPC2') ge 0 then begin	; Mt. Wilson
	date = sxpar(h,'UDATE')
	obsdate = date
	time = sxpar(h,'UTIME')
	day = fix(gettok(date,'-'))
	mos  = gettok(date,'-')
	months = 'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'
	mo = strpos(months,mos)/3 + 1
	year = fix('20'+date)
	if year gt 2075 then year = year - 100
;
	hour = fix(gettok(time,':'))
	minu = fix(gettok(time,':'))
	sec  = fix(gettok(time,' '))
;
;
endif else if jd gt 0 then begin	; APT
	date = sxpar(h,'DATE-OBS')
	obsdate = date
endif else if strtrim(req,2) ne '0' then begin	; tenagra
	date = sxpar(h,'DATE-OBS')
	obsdate = date
	time = sxpar(h,'TIME-OBS')
	year = fix(gettok(date,'-'))
	mo   = fix(gettok(date,'-'))
	day  = fix(date)
;
	hour = fix(gettok(time,':'))
	minu = fix(gettok(time,':'))
	sec  = fix(time)
endif else if strpos(scope,'Automated') ge 0 then begin
	date = sxpar(h,'DATE-OBS')
	obsdate = date
	time = sxpar(h,'TIME-BEG')
	day  = fix(gettok(date,'/'))
	mo   = fix(gettok(date,'/'))
	year = fix(date)
;
	hour = fix(gettok(time,':'))
	minu = fix(gettok(time,':'))
	sec  = fix(time)
endif else begin			; Calypso
	date = sxpar(h,'DATE-OBS')
	obsdate = date
	time = sxpar(h,'UTSTART')
	if strtrim(time,2) eq '0' then $
		time = sxpar(h,'UT')
;
	day = fix(gettok(date,'/'))
	mo  = fix(gettok(date,'/'))
	year = fix('20'+date)
	if year gt 2075 then year = year - 100
;
	hour = fix(gettok(time,':'))
	minu = fix(gettok(time,':'))
	sec  = fix(time)
endelse
;
if jd gt 0 then $
	jd = jd - 2400000.d0 $
else 	juldate,[year,mo,day,hour,minu,sec],jd
if not keyword_set(silent) then $
	print,'JD: ',jd, form = '(a,f12.4)'
;
;
return,jd
end
