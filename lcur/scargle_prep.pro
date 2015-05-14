pro scargle_prep,jd,mag,merr,good,time,counts,cerrs,test=test
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
nsam = n_elements(time)
day = fix(time(0))
n = 1
mave = 10.0^10.0
for i=1,nsam-1 do begin
	if fix(time(i)) ne day or i+1 eq nsam then begin
		p0 = i - n
		p1 = i - 1
		if i+1 ge nsam then $
			p1 = p1 + 1
		avg = total(counts(p0:p1)) / n
		counts(p0:p1) = counts(p0:p1) - avg
		if avg lt mave then mave = avg
		day = fix(time(i))
		n = 1
	endif else begin
		n = n + 1
	endelse
endfor
;
counts = counts + mave
;
return
end
