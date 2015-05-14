;+
;NAME
; effective_wavelength
;PURPOSE
; Returns the effective wavelength of the filter
;USAGE
; efwav = effective_wavelength(filternum, sed_wave, sed_flux)
;INPUTS
; filternum         Number of filter in usual system (i.e., index
;                    of filter in filter_info:master_filter). Can be
;                    an array.
; sed_wave          SED wavelength vector
; sed_flux          SED flux vector at sed_wave wavelengths
;OPTIONAL INPUTS
; redshift          Redshift to place SED at (def: 0.0)
;RETURNS
; Effective wavelength  int( F(l)*S(l)*l*dl ) / int ( F(l)*S(l)*dl ). Returns
; array if array was passed.
;PROCEDURES CALLED
; filter_integ
;AUTHOR
; Mark Sullivan
;-
FUNCTION effective_wavelength,filternum,sed_wave,sed_flux,$
  REDSHIFT=redshift

COMMON filter_info,master_filter

IF (N_ELEMENTS(redshift) EQ 0)THEN  redshift=0.d0
IF(N_ELEMENTS(filternum) EQ 0)THEN BEGIN
   PRINT,'ERROR in effective_wavelength - you must pass some filters'
   RETURN,!Values.D_NAN
ENDIF
IF(N_ELEMENTS(sed_wave) EQ 0 OR N_ELEMENTS(sed_flux) EQ 0)THEN BEGIN
   PRINT,'ERROR in effective_wavelength - you must pass sed_wave and sed_flux'
   RETURN,!Values.D_NAN
ENDIF
IF SIZE(sed_wave,/DIMENSIONS) NE SIZE(sed_flux,/DIMENSIONS) THEN BEGIN
   PRINT,'ERROR in effective_wavelength - sed_wave and sed_flux should be the same length'
   RETURN,!Values.D_NAN
ENDIF

opz=1.0d0+redshift

sed_z=sed_wave*sed_flux*opz ; i.e.  = l*F(l)

n_wavelengths=N_ELEMENTS(filternum)
eff_l=DBLARR(n_wavelengths)
FOR i=0L,n_wavelengths-1 DO BEGIN
    evensamp = is_sed_evenly_sampled( filternum[i],sed_wave, redshift )
    upper=filter_integ(filternum[i],sed_wave,sed_z,redshift,$
                       NODOUBLESAMPLE=evensamp)
    lower=filter_integ(filternum[i],sed_wave,sed_flux,redshift,$
                       NODOUBLESAMPLE=evensamp)
    
    IF(upper LE 0. OR lower LE 0.)THEN BEGIN
        PRINT,'WARNING in EFFECTIVE_WAVELENGTH:'
        PRINT,'Negative or zero fluxes integrated on spectrum for filter '+STRN(filternum)+' ('+master_filter[filternum].shortid+').'
        stop
    ENDIF
    
    IF(lower NE 0.d0)THEN eff_l[i]=upper/lower ELSE eff_l[i]=!Values.D_NAN
    
ENDFOR
IF ( SIZE(filternum,/DIMENSION) EQ 0 ) THEN RETURN, eff_l[0] ELSE RETURN,eff_l
END
