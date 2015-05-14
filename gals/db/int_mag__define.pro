pro int_mag__define
	tmp = { int_mag, $
		mag:0., $	; integrated magnitude
		magerr:0., $	; 1-sigma error in magnitude
		filter:'', $	; filter
		filter_id:0, $	; index into filter table
		source:'', $	; source of magnitude
		source_id:0, $	; index into catalog table
		ab:(1 eq 0) $	; AB boolean 0 - no, 1 - yes
		}
end

