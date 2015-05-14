pro snrec__define
	tmp = { snrec, $
		id:'', $	; YYYY.id format
		name:'', $	; standard name without preceding SN 
		tyn:0, $	; index of type in sntypes table
		cz:0., $	; recession velocity of SN (km/s)
		ra:0.d0, $	; ra J2000 (decimal degrees)
		dec:0.d0, $	; dec J2000 (decimal degrees)
		scid:0}		; index of pos catalog source in sncats
;		scid:0, $	; index of pos catalog source in sncats
;		gid:0, $	; index of host in galaxies table
;		sid:0, $	; index of sites data in table sitemeas
;		lcid:0}		; index of light curve fit in lcfit table
end

