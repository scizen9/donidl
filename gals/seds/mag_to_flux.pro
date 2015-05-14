function mag_to_flux,mag,zp,absolue=absolue
;+
;if absolue is set, then the output is the luminosity insted of the flux
;-
temp=10d0^(-0.4*(mag-ZP))
if (keyword_set(absolue)) then begin
    return,temp*1.19d40
endif else begin
    return,temp
endelse
end

