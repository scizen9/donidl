function muz,zl,h0=h0,om=om,ov=ov,zhel=zhel,zcmb=zcmb
;
; check inputs
if n_params(0) lt 1 then begin
	print,'Usage: mu = muz( z, h0=h0, om=om, ov=ov, zhel=zhel, zcmb=zcmb)'
	return,-1.
endif
;
if n_elements(h0) le 0 then h0=73.d0
if n_elements(om) le 0 then om=0.27d0
if n_elements(ov) le 0 then ov=0.73d0
if n_elements(zhel) le 0 or n_elements(zcmb) le 0 then $
	zfac = 1.0 $
else	zfac = (1.+zhel) / (1.+zcmb)
;
lumd = zfac * sullivanlumdist(zl,omega_l=ov,omega_m=om,h0=h0,/silent)
;
mu = -5. + 5. * alog10( lumd * 10.^6 )
;
return,mu
end
