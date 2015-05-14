;+
;NAME
; is_sed_evenly_sampled
;PURPOSE
; To make sure that a SED (or anything else) has relatively even
;  wavelength sampling.  This is a companion to filter_integ,
;  particularly relating to the /NODOUBLESAMPLE flag
;USAGE
; boolval = is_sed_evenly_sampled( filternumber, wave, redshift )
;INPUTS
; filternumber         The number of the filter used to determine
;                       what region to check the sampling in
; wave                 The wavelength vector to check
; redshift             The redshift to do the check at
;RETURNS
; True (1b) if the SED is evenly sampled, 0b if not
;ALGORITHM
; If the median delta wavelength is more than a factor of 2
; away from the extremes, return false
;FUNCTIONS CALLED
; extract_filter
;MODIFICATION HISTORY
; Author: Alex Conley, May 2006
;-
FUNCTION is_sed_evenly_sampled, filternumber, wave, redshift

;;Get the filter curve
extract_filter,filternumber,filter_wave,filter_response

;;Get the region of overlap
opz = 1.0d0 + redshift
nfilterpoints = N_ELEMENTS( filter_wave )
ilower=VALUE_LOCATE(wave,filter_wave[0]/opz)
iupper=VALUE_LOCATE(wave,filter_wave[nfilterpoints-1]/opz)
ilower>=0

IF iupper - ilower LE 1 THEN RETURN,1b ;;Technically true

delta_sed_wave = wave[ilower+1:iupper] - wave[ilower:iupper-1]

med_delta = MEDIAN( delta_sed_wave )
IF MAX( delta_sed_wave ) GT 2.0*med_delta OR $
  MIN( delta_sed_wave ) LT 0.5*med_delta THEN RETURN,0b ELSE RETURN,1b

END
