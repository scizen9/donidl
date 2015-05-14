pro mag2cnts,jd,mag,merr,good,time,counts,cerrs,test=test
;
; get good points
if good(0) lt 0 then begin
	time = jd
	magg = mag
	merrg = merr
endif else begin
	time = jd(good)
	magg = mag(good)
	merrg = merr(good)
endelse
;
; convert to counts
counts = 10.d0^(10.d0 - 0.4*magg)
cerrs = counts - 10.d0^(10.d0 - 0.4*(magg+merrg))
;
if keyword_set(test) then return
;
; get a unique list of days
days = fix(time)
udays = days(uniq(days))
ndays = n_elements(udays)
;
; loop over each day
mave = 10.0^10.0	; minimum average
for i = 0, ndays-1 do begin
	day = udays(i)
	t = where(days eq day, count)
	if ( count gt 0 ) then begin
		avg = total(counts(t)) / count
		counts(t) = counts(t) - avg	; subtract average for day
		if avg lt mave then mave = avg
	endif else print,'MAG2CNTS - ERROR: day problem: ',day
endfor
;
counts = counts + mave
;
return
end
