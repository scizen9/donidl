
;+
;NAME 
; flux_to_apparent_mag
;PURPOSE
; convert physical flux to apparent magnitude. Do no confuse this
; routine with counts_to_mag, which has nothing to do with physical fluxes.
;INPUTS
; flux - flux to convert (can be array).  Assumes that fluxes are in 
;         erg/cm^2/s/A or erg/cm^2/s/Hz depending on PHOTSYS i.e. this
;         should not be the straight output from filter_integ() !
;OPTIONAL INPUTS
; IFILTER - filter number (used for VEGASYS only). REQUIRED FOR
;           VEGASYS. If fluxes is an array, this must be too.
; VEGASYS - switch to force vega magnitudes. DEFAULT.
; ABSYS - switch to force AB magnitudes
; PHOTSYS - alternative way to choose the filter system 1 - vega  2 -
;           AB. PHOTSYS overrides absys or vegasys.
; ZEROFLUXMAG - what to return for zero or negative fluxes, 
;                 default !Values.D_NAN
; FLUX_ERROR - error in the input flux (for mag error return)
;
;OPTIONAL OUTPUTS
; APPARENT_MAG_ERROR - error in the apparent mag (requires FLUX_ERROR input)
;KEYWORDS
; REVERSE - do the reverse transformation i.e. give physical flux from
;           apparent magnitude.
;RETURNS
; The apparent magnitude(s), array if fluxes is, scalar if not
;COMMON BLOCKS
; filter_info         Not modified
;MODIFICATION HISTORY
; Author: Mark Sullivan
;-

FUNCTION flux_to_apparent_mag,fluxes,IFILTER=ifilter,VEGASYS=vegasys,ABSYS=absys,PHOTSYS=photsys,ZEROFLUXMAG=zerofluxmag,$
  FLUX_ERROR=flux_error,APPARENT_MAG_ERROR=apparent_mag_error,REVERSE=reverse

COMMON filter_info,master_filter

IF(N_ELEMENTS(fluxes) EQ 0)THEN BEGIN
   PRINT,'ERROR in flux_to_apparent_mag - you must pass some fluxes'
   RETURN,!Values.D_NAN
ENDIF

IF(N_ELEMENTS(zerofluxmag) EQ 0)THEN zerofluxmag=!Values.D_NAN
IF(N_ELEMENTS(reverse) EQ 0)THEN reverse=0b

IF(N_ELEMENTS(photsys) NE 0)THEN BEGIN
   IF(photsys EQ 1)THEN vegasys=1
   IF(photsys EQ 2)THEN absys=1
ENDIF

IF(N_ELEMENTS(vegasys) EQ 0 AND N_ELEMENTS(absys) EQ 0)THEN BEGIN
   vegasys=1
   absys=0
ENDIF

IF(N_ELEMENTS(absys) EQ 0)THEN absys=0
IF(N_ELEMENTS(vegasys) EQ 0)THEN vegasys=0

IF(vegasys) THEN BEGIN 
    IF N_ELEMENTS(ifilter) EQ 0 THEN BEGIN
        PRINT,'ERROR in flux_to_apparent_mag - ifilter must be specified for VEGASYS'
        RETURN,!Values.D_NAN
    ENDIF
    IF SIZE(fluxes,/DIMENSIONS) NE SIZE(ifilter,/DIMENSIONS) THEN BEGIN
        PRINT,"ERROR in flux_to_apparent_mag: ifilter and fluxes are not ",$
              "the same dimension"
        RETURN,!Values.D_NAN
    ENDIF
ENDIF

nfluxes=N_ELEMENTS(fluxes)
apparent_mag=DBLARR(nfluxes)
apparent_mag_error=DBLARR(nfluxes)*!Values.D_NAN
FOR i=0L,nfluxes-1 DO BEGIN
   IF(fluxes[i] LE 0. AND NOT reverse)THEN BEGIN
      apparent_mag[i]=zerofluxmag
   ENDIF ELSE BEGIN
      ;; for vegasys, vegafluxfac is set to the real flux of VEGA
      IF(vegasys) THEN BEGIN
         fluxfac=master_filter[ifilter[i]].vegazp / $
                 master_filter[ifilter[i]].area_lambda
      ENDIF ELSE fluxfac=3.6308d-20   ;;3630.78 Jy in cgs units
      IF(reverse)THEN BEGIN
         ;; mag is a flux, flux is a mag :)
         ;fluxes[i]=-2.5*ALOG10(apparent_mag[i]/fluxfac)
         apparent_mag[i]=10^(-0.4*fluxes[i])*fluxfac
        IF(KEYWORD_SET(flux_error))THEN BEGIN
            apparent_mag_error[i]=apparent_mag[i]/1.0857362d0*flux_error[i]
        ENDIF
      ENDIF ELSE BEGIN
         apparent_mag[i]=-2.5*ALOG10(fluxes[i]/fluxfac)
        IF(KEYWORD_SET(flux_error))THEN BEGIN
            apparent_mag_error[i]=1.0857362d0 * (flux_error[i]/fluxes[i])
        ENDIF
      ENDELSE
   ENDELSE
ENDFOR   

IF ( SIZE(fluxes,/DIMENSION) EQ 0 ) THEN RETURN,apparent_mag[0] ELSE RETURN,apparent_mag
END
