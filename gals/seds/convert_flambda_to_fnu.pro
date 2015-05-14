function convert_flambda_to_fnu,lambda,flambda
;+ 
; Converts a flux density flambda in erg/s/A to fnu in
; erg/s/Hz
; lambda must be in angstrom !
;-

c=3.d8
flux=flambda*1e10                  ; erg/s/cm^2/m
flux=flux*(lambda*1d-10)^2./c   ; to Fnu (Hz^-1)
return,flux
end
