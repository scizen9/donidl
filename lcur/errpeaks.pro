pro errpeaks,nu,pow,pkis,npks,delnus
;

delnus = fltarr(npks)
nif = n_elements(nu)

for i=0,npks-1 do begin

	thresh = pow(pkis(i)) * 0.85	; 15% down from peak
	nu0 = nu(pkis(i))
;
; upper half width
	u = pkis(i)
	while ( pow( (u < (nif-1) ) ) ge thresh and u lt nif ) do u = u + 1
;
; lower half width
	l = pkis(i)
	while ( pow(l>0) ge thresh and l ge 0 ) do l = l - 1

	if u lt nif then $
		delu = nu(u) - nu0 $
	else	delu = 999.0

	if l ge 0 then $
		dell = nu0 - nu(l) $
	else	dell = 999.0

	delnus(i) = (delu + dell) / 2.0

endfor
;
return
end
