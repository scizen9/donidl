	function sky_cts, w, grat, exposure_time, airmass=airmass, area=area
	airmass0 = 1.2
	if keyword_set(airmass) then airmass0 = airmass
	area0 = 1.0
	if keyword_set(area) then area0 = area
	A_geo = !PI/4.*(10.e2)^2
	eff = inst_throughput(w, grat)
	cts = eff*A_geo*exposure_time*sky_mk(w)*airmass0*area0 
	return,cts
	end
