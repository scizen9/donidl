pro sed_fit__define
	tmp = { sed_fit, $
		source:'', $	; SED fitting software
		source_id:0, $	; index into SED fitter table
		mass:0., $	; log of solar masses
		mass_lo:0., $	; lower 1-sigma mass bound
		mass_hi:0., $	; upper 1-sigma mass bound
		age:0., $	; age in years
		age_lo:0., $	; lower 1-sigma age bound
		age_hi:0., $	; upper 1-sigma age bound
		sfr:0., $	; SFR in solar masses per year
		sfr_lo:0., $	; lower 1-sigma SFR bound
		sfr_hi:0., $	; upper 1-sigma SFR bound
		ssfr:0., $	; Specific SFR in inverse years
		ssfr_lo:0., $	; lower 1-sigma sSFR bound
		ssfr_hi:0., $	; upper 1-sigma sSFR bound
		ebmv:0., $	; internal color excess E(B-V) in magnitudes
		ebmv_err:0., $	; error on E(B-V) in magnitudes
		chi2:0., $	; chi^2 of SED fit
		npts:0 $	; number of photometry points fit
		}
end
