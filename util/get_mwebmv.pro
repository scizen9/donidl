function get_mwebmv, ra, dec
;+
; get_mwebmv - return Milky Way E(B-V) at ra and dec (degrees, J2000)
;
; converts ra and dec to galactic coords and calls dust_getval
;-
	glactc, ra, dec, 2000., gall, galb, 1, /degree
	ebmv = dust_getval(gall, galb)
	return, ebmv
end
