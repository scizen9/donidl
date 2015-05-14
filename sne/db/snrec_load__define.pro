pro snrec_load__define
	tmp = { snrec_load, $
		id:'', $	; YYYY.id format
		name:'', $	; standard name without preceding sn 
		tyn:0, $	; index of type in sntypes table
		type:'', $	; sn type
		cz:0., $	; recession velocity of sn (km/s)
		ra:0.d0, $	; ra J2000 (decimal degrees)
		dec:0.d0, $	; dec J2000 (decimal degrees)
		off_ew:0., $	; sn host offset E/W (arcsec)
		off_ns:0., $	; sn host offset N/S (arcsec)
		snmag:0., $	; sn mag
		snfilt:'', $	; filter of sn mag
		mtyp:'', $	; type of sn mag: * -discovery, else max
		ddate:'', $	; discovery date
		discoverer:'', $; names of sn discoverers
		scid:0, $	; index of pos catalog source in sncats
		gid:0 }		; index of host in galaxies table
end

