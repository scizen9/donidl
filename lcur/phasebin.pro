pro phasebin,phase,val,err,nbins,weight=weight
;
; outputs
nph = fltarr(nbins)
nv = fltarr(nbins)
nerr = fltarr(nbins)
;
; bin size
sz = 1.0/nbins
;
; loop over bins
for i=0,nbins-1 do begin
	bin0 = i * sz
	bin1 = bin0 + sz
	t = where(phase gt bin0 and phase le bin1, n)
	if n gt 1 then begin
		nph(i) = mean(phase(t))
		if keyword_set(weight) then begin
			nv(i) = wmean(val(t),err(t))
			nerr(i) = wstdev(val(t),err(t))
		endif else begin
			nv(i) = mean(val(t))
			nerr(i) = stddev(val(t))
		endelse
	endif else begin
		nph(i) = mean([bin0,bin1])
		nv(i) = -99.999
		nerr(i) = -9.999
	endelse
endfor
;
phase = nph
val = nv
err = nerr
;
return
end	; pro phasebin
