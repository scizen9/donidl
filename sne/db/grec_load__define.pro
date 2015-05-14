pro grec_load__define
	tmp = { grec_load, $
		id:0, $		; galaxy id
		name:'', $	; standard name
		ra:0.d0, $	; ra J2000 (decimal degrees)
		dec:0.d0, $	; dec J2000 (decimal degrees)
		scid:0, $	; index of pos catalog source in gcats
		gbt:0., $	; integrated B mag
		z:0.d0, $	; redshift
		d25:0., $	; diameter in arcsec
		dmod:0., $	; distance modulus
		linscl:0., $	; Linear scale at galaxy in pc/arcsec
		mwebmv:0., $	; Milky Way E(B-V)
		inc:0., $	; inclination: 0 is face on
		pa:0., $	; pos. angle (degrees E of N)
		logab:0., $	; log axial ratio (a/b)
		log10d:0., $	; giso diam (log 10d)
		type:'', $	; galaxy type
		qidt:0, $	; pointer to type of uncertainty
		tyn:0., $	; numerical type code of galaxy
		e_t:0.}		; error in type code
end

