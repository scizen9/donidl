pro sedfit_data__define
;+
; sedfit_data__define - define sed fit database structure
;
; NOTE: remember to keep structure name on a line by itself 
;	(see struct_init.pro)
;
;-
tmp = { sedfit_data, $
	id:'', $		; Galaxy ID
	gind:0L, $		; index into galdat structure
	lep_file:'', $		; LePhare output file with data
	lep_file_ts:0.d0, $	; Lephare output file time stamp (unix secs)
	lep_zfix:0, $		; was z fixed? (0 - no, 1 - yes)
	lep_zinterp:0, $	; z interpolated? (0 - no, 1 - yes)
	lep_zlib:'', $		; Galaxy stellar library used for fit
	lep_firfree:0, $	; free scaling of FIR (0 - no, 1 - yes)
	lep_firlmin:0., $	; Lambda min for FIR analysis (microns)
	lep_bands:'', $		; Comma separated list of bands
	lep_bdscale:0L, $	; Context for band scaling (bit pattern)
	lep_bdmabs:0L, $	; Context for absolute mags (bit pattern)
	lep_zspec:0., $		; z from spectroscopy
	lep_zphot:0., $		; z from photometry
	lep_zmod:'', $		; Galaxy stellar model used for fit
	lep_firlib:'', $	; FIR library used for fit
	lep_firmod:0, $		; FIR model used for fit
	mod_time:0.d0 $		; time stamp in seconds systime(1)
	}
end
