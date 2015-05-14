pro wstats, vec, errs, mean, sigma, weights=weights
;
w = intarr(n_elements(vec))+1
if keyword_set(weights) then w = weights

g = where(w eq 1, count)

if count ge 3 then begin
	mean = total(vec(g)/errs(g)^2) / total(1./errs(g)^2)
	sigma = sqrt( 1. / total(1./errs(g)^2) )
endif else begin
	print,'WSTATS: ERROR - not enough points'
	mean = 0.
	sigma = 0.
endelse

return
end
