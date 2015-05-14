pro pegase_compute_mag_filter,filter,lambda,flux,lambdaline,fluxline,$
                              mag,fluxfilter,error=error
;+
;--------------------------------------------------
; inputs: 
; - filter: filter structure. 
;                 must be calibrated
;  - lambda, flux, lambdaline, fluxline : spectrum
; outputs: 
;  - mag: calibration is set by filter.calib
;  - fluxfilter (*not* a flux density, but an
;                          power (erg/s))
;--------------------------------------------------
;-

;***** Reading of the transmission of the filters
if (filter.calibtype eq -1) then begin
    print,'ERROR: filter not calibrated in call to pegase_compute_mag_filter'
    print,'aborting'
    error=1
    return
endif


error=0
fluxfilter=0.
calculflux,filter.lambda(0:filter.nlambda-1),$
  filter.trans(0:filter.nlambda-1),filter.transtype,filter.area,$
  lambda,flux,lambdaline,fluxline,$
  fluxfilter,error=error


if (fluxfilter gt 0. and error eq 0) then begin
    mag=-2.5d0*alog10(fluxfilter/1.1965d40)+filter.calib
    fluxfilter=fluxfilter*filter.area ;pour avoir le flux integere dans le filtre
endif else begin
    mag=0.
endelse

end

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
