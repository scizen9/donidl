function gx_xphtsim,t0,ext,rate,period=period,amplitude=amplitude
;+
;	gx_xphtsim - simulate pure Poisson noise in photon arrival times
;
;	times = gx_xphtsim(t0, exposure_time, rate)
;
; INPUTS:
;	t0	- beginning of exposure in GALEX spacecraft time
;	ext	- exposure time of observation in seconds
;	rate	- average rate over observation in counts/second
;
; RETURNS:
;	Simulated photon arrival times covering the exposure with the expected
;	Poisson noise characteristics
;
; HISTORY:
;	21-oct-2010: Initial Revision
;-
	; number of seconds
	nsec = fix(ext+0.5)
	; rate
	if keyword_set(period) then begin
		if keyword_set(amplitude) then $
			amp = amplitude $
		else	amp = 1.0
		pdst = rate + sin(2.*!pi*findgen(nsec)/period)*amp
	endif  else	pdst = randomn(seed,nsec,poisson=rate)

	; loop over seconds
	tlist = [t0]
	for i = 0l, nsec-1l do begin
		if pdst[i] gt 0. then begin	; skip seconds with no counts
			ti0 = t0 + double(i)
			if keyword_set(period) then $
				ts = findgen(fix(pdst[i]+0.5))/fix(pdst[i]+0.5)$
			else	ts = randomu(dees,pdst[i])
			tlist = [tlist,ts+ti0]
		endif
	endfor
	tlist = tlist[1:*]

	return,tlist
end
