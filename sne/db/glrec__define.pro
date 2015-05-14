pro glrec__define
	tmp = { glrec, $
		id:0, $		; id
		name:'', $	; standard name
		ra:0.d0, $	; ra J2000 (decimal degrees)
		dec:0.d0, $	; dec J2000 (decimal degrees)
		qpid:0, $	; uncertainty of SN position
		scid:0, $	; index of pos catalog source in sncats
		mag:0., $	; Magnitude
		bandid:0, $	; index of band in table band
		hid:0, $	; index of host in hostmeas table
		z:0.d0, $	; redshift
		d25:0., $	; Diameter of galaxy in arcseconds
		dmod:0., $	; Distance modulus
		linscl:0., $	; linear scale at galaxy in pc/arcsec
		mwebmv:0., $	; Milky Way E(B-V)
		inc:0., $	; Host inclination: 0. -face on
		pa:0., $	; Position angle
		logab:0., $	; Decimal log of axial ratio
		log10d:0., $	; Decimal log of apparent isophotal major diam in units of 0.1 arcmin
		type:'', $	; Morphological type
		qidt:0, $	; Uncertainty in morphological type
		tyn:0., $	; Numerical code for galaxy type
		e_t:0., $	; error in Numerical code for galaxy type
		comment:''}	; comments on SN
end

