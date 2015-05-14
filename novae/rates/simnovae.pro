pro simnovae,n,mags,dr,real=real
;
; generate simulated nova light curves for n novae over a year
;
mags = fltarr(365,n)

if not keyword_set(real) then begin
    dr = randomu(seed,n)*0.2

    for i=0,n-1 do $
	for j=0,364 do $
    	    mags(j,i) = -6.05 + float(j - 14) * dr(i)

endif else begin

rdr = [0.09, 0.125, 0.04, 0.22, 0.10, 0.13, 0.13, 0.14, 1.02, 1.02, $
	0.38, 0.20, 0.17, 0.23, 0.15, 0.23, 0.174, 0.29, 0.18, 0.077, $
	0.163, 0.126, 0.156, 0.069, 0.058, 0.07, 0.06, 0.075, 0.067, $
	0.046, 0.059, 0.061, 0.043, 0.019, 0.017, 0.017]
rmm = [16.5, 16.35, 16.95, 14.95, 17.3, 17.0, 16.2, 16.1, 15.7, 15.7, $
	15.9, 18.2, 15.9, 16.0, 15.9, 16.0, 16.0, 16.0, 16.1, 17.0, $
	16.2, 16.4, 16.7, 17.2, 17.5, 17.6, 17.2, 17.4, 17.6, $
	17.4, 17.8, 17.6, 18.0, 17.8, 18.0, 18.1]

    pick = fix( randomu(seed,n) * float(n_elements(rdr)) )
    dr = fltarr(n)
    mm = fltarr(n)

    for i=0,n-1 do begin
	dr(i) = rdr(pick(i))
	mm(i) = rmm(pick(i)) - 24.4
    	for j=0,364 do $
	    mags(j,i) = mm(i) + float(j) * dr(i)
    endfor
	    
endelse
;
return
end
