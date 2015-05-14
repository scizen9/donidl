pro cnts2mag,counts,mag,cerrs,merr,zeropoint=zeropoint
;
; convert to mags
if keyword_set(zeropoint) then $
	zp = zeropoint $
else	zp = 25.d0
mag = zp - 2.5 * alog10( counts )

if n_params(0) ge 3 then $
	merr = 1.0857362d0 * (cerrs/counts)
;
return
end
