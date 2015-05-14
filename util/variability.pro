function variability, in_mags, in_merrs, dmag, avmag, np, $
	grange=grange, merrlim=merrlim
;+
;	variability - calculate variability
;-
var = -99.0
dmag = -99.0
avmag = -99.0
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
good = where(in_mags le grng[0] and in_mags ge grng[1] and $
	     in_merrs ge 0. and in_merrs le melim, np)
if np le 0 then return,var
;
mags = in_mags[good]
merrs= in_merrs[good]
;
dmag = max(mags) - min(mags)
avmag = wmean(mags, merrs)
if np ge 2 then begin

	wt = (1.d0 / merrs^2) / (1.d0 + (abs(mags-avmag)/2.d0)^2)
	avmag = total(mags*wt) / total(wt)

	var = sqrt(1./ ( np * (np-1.))) * total( (mags-avmag)^2/merrs^2 )
endif
;
return,var
end
