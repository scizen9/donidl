function gx_xphtsims,t0s,exts,rate,outfile=outfile,delt=delt, $
	period=period,amplitude=amplitude
;+
;	gx_xphtsims - calling routine for gx_xphtsim for multiple observations
;
;	times = gx_xphtsims(t0s, exposure_times, rate)
;
; INPUTS:
;	t0s	- beginnings of exposures in GALEX spacecraft time
;	exts	- exposure times of observations in seconds
;	rate	- average rate over all observations in counts/second
;
; RETURNS:
;	Simulated photon arrival times covering the exposures with the expected
;	Poisson noise characteristics
;
; HISTORY:
;	21-oct-2010: Initial Revision
;-
	; number of seconds
	nobs = n_elements(t0s)
	if n_elements(exts) ne nobs then begin
		print,'GX_XPHTSIMS: Error - t0s and exposure times must have same dimension'
		return,-1
	endif

	; output if requested
	if keyword_set(outfile) then begin
		if keyword_set(delt) then $
			dt = delt $
		else	dt = 1.
		openw,ol,outfile,/get_lun
		printf,ol,'# '+outfile+', '+systime(0)
	endif

	; loop over observations
	tlist = [t0s[0]]
	for i = 0l, nobs-1l do begin
		tl = gx_xphtsim(t0s[i],exts[i],rate,period=period,amplitude=amplitude)
		if keyword_set(outfile) then begin
			np = fix(exts[i]/dt) - 1l	; skip last interval
			tb = t0s[i] + dt
			tf = tb + dt
			for j=0l,np-1l do begin
				c = where(tl ge tb and tl lt tf, nc)
				printf,ol,(tb+dt*0.5d0),(float(nc)/dt), $
					format='(f19.4,f9.3)'
				tb = tf
				tf = tb + dt
			endfor
		endif
		tlist = [tlist,tl]
	endfor
	tlist = tlist[1:*]

	if keyword_set(outfile) then free_lun,ol

	return,tlist
end
