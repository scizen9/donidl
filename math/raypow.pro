function raypow,time,f
;+
;	raypow - compute the Rayleigh Power for a list of individual
;		photon arrival times
;
; INPUTS:
;	time	- a list of arrival times
;	f	- the frequency for calculating the Rayleigh Power
;
; RETURNS:
;	The Rayleigh Power, see eqn. 2, Kruger, Laredo, & Wasserman (2002)
;-
	t0 = min(time)
	t  = time - t0
	n  = n_elements(time)
	;
	ph = 2.*!PI*f*t
	r = (total(sin(ph))^2 + total(cos(ph))^2) / double(n)
	return,r
end
