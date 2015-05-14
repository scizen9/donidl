pro get_linscale,z,sscl,mu
;+
;	calculate linear scale given redshift
;-
	;
	; get linear scale at galaxy
	lumd = sullivanlumdist(z,omega_l=!COSMO_OL, $
				omega_m=!COSMO_OM,h0=!COSMO_H0, $
							   /silent)
	dmpc = lumd / (1.+z)^2	; convert to angular size dist.
	sscl = ( dmpc / 206265.d0 ) * 1.d6      ; pc / arcsec
	;
	; get distance modulus
	mu = -5. + 5. * alog10( lumd * 10.^6 )
	;
	return
end
