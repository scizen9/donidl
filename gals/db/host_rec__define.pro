pro host_rec__define
	tmp = { host_rec, $
		key:0, $	; id
		name:'', $	; standard name
		name2:'', $	; alternate name
		ra:0.d0, $	; ra J2000 (decimal degrees)
		dec:0.d0, $	; dec J2000 (decimal degrees)
		z:0.d0, $	; redshift
		dmod:0., $	; Distance modulus
		linscl:0., $	; linear scale at galaxy in pc/arcsec
		mwebmv:0., $	; Milky Way E(B-V) in magnitudes
		inc:0., $	; Host inclination in degrees: 0 = face on
		pa:0., $	; Position angle
		axrat:0., $	; axial ratio: b/a
		majdiam:0., $	; Major diameter (a) in arcseconds
		type:'', $	; Morphological type
		qidt:0, $	; Uncertainty in morphological type
		tyn:0., $	; Numerical code for galaxy type
		e_t:0., $	; error in Numerical code for galaxy type
		int_mags:replicate({int_mag}, 25), $ ; integrated magnitudes
		sed_fits:replicate({sed_fit}, 5) $	; SED fits
		}
end

