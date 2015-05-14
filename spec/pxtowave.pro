function pxtowave, px, wavezero, deltawave, refpix
	;
	; WCS linear dispersion
	if n_params(0) ge 4 then $
		px0 = refpix $
	else	px0 = 0
	return, wavezero + (px - px0) * deltawave
end
