function variability2, in_mags1, in_merrs1, in_mags2, in_merrs2, $
	dmag1, dmag2, avmag1, avmag2, np, grange=grange, merrlim=merrlim
;+
;	variability - calculate variability
;-
var = -99.0
dmag1 = -99.0
dmag2 = -99.0
avmag1 = -99.0
avmag2 = -99.0
if keyword_set(merrlim) then $
	melim = merrlim $
else	melim = 99.
if keyword_set(grange) then begin
	if n_elements(grange) ne 2 then begin
		print,'VARIABILITY: Error - good range must be two elements'
		return,var
	endif
	if grange[0] lt grange[1] then begin
		print,'VARIABILITY: Error - good range must be [faint, bright]'
		return,var
	endif
	grng = grange
endif else grng = [90.,-90.]
;
good = where(in_mags1 le grng[0] and in_mags1 ge grng[1] and $
	     in_merrs1 ge 0. and in_merrs1 le melim and $
	     in_mags2 le grng[0] and in_mags2 ge grng[1] and $
	     in_merrs2 ge 0. and in_merrs2 le melim, np)
if np le 0 then return,var
;
mags1 = in_mags1[good]
merrs1= in_merrs1[good]
mags2 = in_mags2[good]
merrs2= in_merrs2[good]
;
dmag1 = max(mags1) - min(mags1)
avmag1 = wmean(mags1, merrs1)
dmag2 = max(mags2) - min(mags2)
avmag2 = wmean(mags2, merrs2)

if np ge 2 then begin

	wt1 = (1.d0 / merrs1^2) / (1.d0 + (abs(mags1-avmag1)/2.d0)^2)
	avmag1 = total(mags1*wt1) / total(wt1)
	wt2 = (1.d0 / merrs2^2) / (1.d0 + (abs(mags2-avmag2)/2.d0)^2)
	avmag2 = total(mags2*wt2) / total(wt2)

	var = sqrt(1./ ( np * (np-1.))) * $
	      total( ((mags1-avmag1)/merrs1) * ((mags2-avmag2)/merrs2) )

endif
;
return,var
end
