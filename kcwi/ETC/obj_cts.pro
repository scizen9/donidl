	function obj_cts, w, fo, grat, exposure_time

;	fo = flux in ph / cm^2 /s/A
;	extinction vs. airmass not included yet
;
	A_geo = !PI/4.*(10.e2)^2
	eff = inst_throughput(w, grat)
	cts = eff*A_geo*exposure_time*fo
	return,cts
	end
