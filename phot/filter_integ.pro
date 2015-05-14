;+
;NAME
; filter_integ
;PURPOSE
; Integrates a sed through a filter
;USAGE
; flux = filter_integ( filternumber, sed_wave, sed_flux, redshift)
;INPUTS
; filternumber       Filter identifier in usual system
; sed_wave           Input SED wavelength array (rest frame) 
; sed_flux           Input SED flux array (rest frame)
; redshift           Redshift of object
;KEYWORDS
; nodoublesample     If you are certain that one of either the
;                     SED or the filter curve is everywhere better
;                     better sampled than the other, this flag will
;                     make the computation faster (roughly by a factor
;                     of 2 for most cases).
; fnu                Convert to f_nu units, assuming that sed_flux
;                     is in f_lambda units by multiplying by
;                     lambda^2/c.  Note that you probably only think
;                     you need to use this, and you shouldn't use it
;                     unless you are absolutely sure what you are doing.
; flambda            Convert to f_lambda units, assuming that sed_flux
;                     is f_nu by multiplying by c/lambda^2
; truncate_filters     This adjusts the filter response to neglect all
;                     parts of the filter where the throughput is
;                     <truncate_pc%. Some filters have very wide definitions
;                     even where there is essentially no throughput. Not
;                     recommended for light-curve fitting, but great
;                     for integrating real-life spectra! Default: NO
; truncate_pc         Default 1: percentage throughput at whihc to
;                      truncate the filter responses.  This is defined
;                      as the percentage relative to the maximum
;                      throughput
; nochecknan          Don't check input vector for NaN
; verbose             Print warnings if necessary. Errors are always printed.
; OPTIONAL OUTPUTS
; flag                This is set to a positive value if something
;                      weird happens:
;                       1-Filter was either bluer/redder than SED
;                       (flux still returned)
;                       2-Filter and SED did not overlap (no flux
;                       returned)
;                       3-Flux was negative (zero flux returned)
;                       4-Input spectrum contained non-finite
;                       numbers. These are interpolated across.
;RETURNS
; flux at same sampling as sed_wave
;COMMON BLOCKS
; filter_info        Not modified.
;NOTES
; How to interpret the flux is not a question that can be answered
; simply.  It depends on the photometric system in use, and exactly
; how the filter functions are set up and normalized with respect to
; that system.  This will return the integral of whatever you pass in,
; so if you pass in f_nu, expect f_nu back (sans filter
; normalization).
;
; Having said that, in almost all instances you should be passing
; in f_lambda, which is the way the rest of our code-base is set
; up -- even if you are computing AB mags.  The difference between
; AB and Vegamax mags is handled by what you divide the number
; filter_integ returns by -- the area of the filter (area_lambda)
; or the abzp (which isn't really a zeropoint, so be careful).
;
; It is assumed that filter reponses are positive.
;
; Note that if you use truncate_filters it is up to you to handle
; the filter zeropoints, etc.
;AUTHOR
; Mark Sullivan
;
; MS 30-06-2005 Changed so that if flux<0, returned flux =0. Added
; verbose flag.
;
;-

;;Version that assumes that there are no funny changes in
;; sampling in the middle of the interval
FUNCTION filter_integ_quick,filter_wave,filter_response,$
  sed_wave,sed_flux,opz,nfilterpoints,iupper,ilower, FNU=fnu,$
  FLAMBDA=flambda,INTEGWAVE=wavelengths_to_interpolate,$
  INTEGFLUX=flux_transmitted

;;If the filter function is this much better sampled than the
;; SED, then we resample the SED.  Otherwise we try to resample
;; the filter function
resample_limit = 2 

nsedpoints = iupper - ilower ;;Number of SED points in range

;;Figure out if we are going to resample the SED or the filter
;; function
IF nfilterpoints GT resample_limit * nsedpoints THEN BEGIN
    ;;Resampling the SED instead of the filter, which should
    ;; be the less common case
    wavelengths_to_interpolate = filter_wave
    sed_inside_filter = INTERPOL( sed_flux, sed_wave, filter_wave/opz )
    interpolated_filter_response = filter_response
ENDIF ELSE BEGIN
    ;;Resampling the filter function
   ;;Figure out if any SED wavelengths lie outside the filter
   ;;response. This should usually be the case for ilower and never the
   ;;case for iupper
    offlower = sed_wave[ilower]*opz LT filter_wave[0]
    offupper = sed_wave[iupper]*opz GT filter_wave[nfilterpoints-1]
    IF ( offupper AND offlower ) THEN BEGIN
        ;;We have points at both edges in the SED that stick out
        ;; past the end of the filter response
        ;;This case may not actually be allowed to happen because of
        ;; the way that value_locate works, but it's worth including
        ;; just to be safe
        wavelengths_to_interpolate = $
          [ filter_wave[0], sed_wave[ilower+1:iupper-1]*opz, $
            filter_wave[nfilterpoints-1] ]
        sedflux_low = INTERPOL( sed_flux, sed_wave, filter_wave[0]/opz )
        sedflux_high = INTERPOL( sed_flux, sed_wave, $
                                 filter_wave[nfilterpoints-1]/opz )
        sed_inside_filter = [ sedflux_low, sed_flux[ilower+1:iupper-1], $
                              sedflux_high ]
    ENDIF ELSE IF ( offlower ) THEN BEGIN
        ;;Drifted off the lower end, so clip off the lowest point
       ;; and replace it with lowest filter point. This should be what
       ;; usually happens.
        wavelengths_to_interpolate = $
          [ filter_wave[0], sed_wave[ilower+1:iupper]*opz ]
        sedflux = INTERPOL( sed_flux, sed_wave, filter_wave[0]/opz )
        sed_inside_filter = [ sedflux, sed_flux[ilower+1:iupper] ]
    ENDIF ELSE IF ( offupper ) THEN BEGIN
       ;;Drifted off upper end.  Same procedure, other side. This
       ;;should never happen.
        wavelengths_to_interpolate = $
          [ sed_wave[ilower:iupper-1]*opz, filter_wave[nfilterpoints-1] ]
        sedflux = INTERPOL( sed_flux, sed_wave, $
                            filter_wave[nfilterpoints-1]/opz )
        sed_inside_filter = [ sed_flux[ilower:iupper-1], sedflux ]
    END ELSE BEGIN
        ;;The SED is completely contained within the filter response
        ;;This should sometimes happen.
        wavelengths_to_interpolate=sed_wave[ilower:iupper]*opz
        sed_inside_filter=sed_flux[ilower:iupper]
    ENDELSE
    interpolated_filter_response=INTERPOL(filter_response,filter_wave,$
                                          wavelengths_to_interpolate)
ENDELSE

;;One potential weakness of the interpolation/extrapolation is that it can
;; do funny things if filters have sharp edges.  In particular,
;; it may result in negative fluxes.  We don't want this, so
;; set those to zero
interpolated_filter_response >= 0.0d0

;;Uncomment to get a plot of the interpolation
;PLOT,filter_wave,filter_response
;OPLOT,wavelengths_to_interpolate,interpolated_filter_response,$
;      COLOR=colordex('red')
;OPLOT,wavelengths_to_interpolate,interpolated_filter_response,PSYM=4,$
;      COLOR=colordex('red')
;OPLOT,wavelengths_to_interpolate,sed_inside_filter/MAX(sed_inside_filter),$
;      color=colordex('blue')
;OPLOT,wavelengths_to_interpolate,sed_inside_filter/MAX(sed_inside_filter),$
;      color=colordex('blue'),psym=4
;PRINT,"Npoints: ",N_ELEMENTS( wavelengths_to_interpolate )

;; get transmitted flux at each point
flux_transmitted=interpolated_filter_response*sed_inside_filter

;;Convert to f_nu if requested
IF KEYWORD_SET( fnu ) THEN BEGIN
    c_light = 2.99792458d18 ;;In angstroms
    flux_transmitted *= wavelengths_to_interpolate^2/c_light
ENDIF ELSE IF KEYWORD_SET( flambda ) THEN BEGIN
    c_light = 2.99792458d18 ;;In angstroms
    flux_transmitted *= c_light/wavelengths_to_interpolate^2
ENDIF

;; trapezoidal integration.  TSUM is from astrolib
;;TSUM doesn't handle single values
IF N_ELEMENTS( wavelengths_to_interpolate ) EQ 1 THEN BEGIN
    filter_integral = wavelengths_to_interpolate * flux_transmitted
    IF KEYWORD_SET( verbose ) THEN $
      PRINT,"FILTER_INTEG WARNING: ultra-narrow filter "
ENDIF ELSE $
  filter_integral=TSUM(wavelengths_to_interpolate,flux_transmitted)

;;Now divide by 1+z
filter_integral /= opz

RETURN,filter_integral

END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Version that takes the more cautious route of resampling both
;; the SED and the filter response onto the combined grid of
;; wavelengths.  Necessary if the sampling density changes
;; significantly somewhere in the middle of the region of interest
FUNCTION filter_integ_dblsamp,filter_wave,filter_response,$
  sed_wave,sed_flux,opz,nfilterpoints,iupper,ilower, FNU=fnu,$
  FLAMBDA=flambda,INTEGWAVE=wavelengths_to_interpolate,$
  INTEGFLUX=flux_transmitted

;;We would like to try to avoid extrapolation.  With this in mind,
;; we calculate if any of the SED points stick out past the end
;; of the filter function
offlower = sed_wave[ilower]*opz LT filter_wave[0]
offupper = sed_wave[iupper]*opz GT filter_wave[nfilterpoints-1]
IF ( offupper AND offlower ) THEN BEGIN
    ;;We have points at both edges in the SED that stick out
    ;; past the end of the filter response
    wavelengths_to_interpolate = [ filter_wave, $
                                   sed_wave[ilower+1:iupper-1]*opz ]
ENDIF ELSE IF ( offlower ) THEN BEGIN
    wavelengths_to_interpolate = [ filter_wave, sed_wave[ilower+1:iupper]*opz ]
ENDIF ELSE IF ( offupper ) THEN BEGIN
    wavelengths_to_interpolate = [ filter_wave, sed_wave[ilower:iupper-1]*opz ]
ENDIF ELSE BEGIN
    wavelengths_to_interpolate = [ filter_wave, sed_wave[ilower:iupper]*opz ]
ENDELSE

;; UNIQ with SORT is way faster than rem_dup
wavelengths_to_interpolate = $
  wavelengths_to_interpolate[UNIQ(wavelengths_to_interpolate,$
                                  SORT(wavelengths_to_interpolate))]

interpolated_filter_response=INTERPOL( filter_response, filter_wave,$
                                       wavelengths_to_interpolate )
sed_inside_filter = INTERPOL( sed_flux, sed_wave, $
                              wavelengths_to_interpolate/opz )

interpolated_filter_response >= 0.0d0

;;Uncomment to get a plot of the interpolation
;PLOT,filter_wave,filter_response
;OPLOT,wavelengths_to_interpolate,interpolated_filter_response,$
;      COLOR=colordex('red')
;OPLOT,wavelengths_to_interpolate,interpolated_filter_response,PSYM=4,$
;      COLOR=colordex('red')
;mval = MAX( sed_flux[ilower:iupper] )
;OPLOT,sed_wave[ilower:iupper]*opz,sed_flux[ilower:iupper]/mval,$
;      color=colordex('blue')
;OPLOT,wavelengths_to_interpolate,sed_inside_filter/mval,$
;      color=colordex('blue'),psym=4
;
;PRINT,"Npoints: ",N_ELEMENTS( wavelengths_to_interpolate )

;; get transmitted flux at each point
flux_transmitted=interpolated_filter_response*sed_inside_filter

;;Convert to f_nu if requested
IF KEYWORD_SET( fnu ) THEN BEGIN
    c_light = 2.99792458d18 ;;In angstroms
    flux_transmitted *= wavelengths_to_interpolate^2/c_light
ENDIF ELSE IF KEYWORD_SET( flambda ) THEN BEGIN
    c_light = 2.99792458d18 ;;In angstroms
    flux_transmitted *= c_light/wavelengths_to_interpolate^2
ENDIF

;; trapezoidal integration.  TSUM is from astrolib
;;TSUM doesn't handle single values
IF N_ELEMENTS( wavelengths_to_interpolate ) EQ 1 THEN BEGIN
    filter_integral = wavelengths_to_interpolate * flux_transmitted
    IF KEYWORD_SET( verbose ) THEN $
      PRINT,"FILTER_INTEG WARNING: ultra-narrow filter"
ENDIF ELSE $
  filter_integral=TSUM(wavelengths_to_interpolate,flux_transmitted)

;;Now divide by 1+z
filter_integral /= opz

RETURN,filter_integral

END

FUNCTION filter_integ,filternumber,sed_wave,sed_flux,redshift,FNU=fnu,$
  FLAMBDA=flambda,VERBOSE=verbose,TRUNCATE_FILTERS=truncate_filters,$
  TRUNCATE_PC=truncate_pc,NODOUBLESAMPLE=nodoublesample,FLAG=flag,$
  INTEGWAVE=integwave,INTEGFLUX=integflux,NOCHECKNAN=nochecknan

COMMON filter_info, master_filter

IF KEYWORD_SET( verbose ) THEN verbose=1b ELSE verbose=0b
IF(N_ELEMENTS(truncate_filters) EQ 0)THEN truncate_filters=0b
IF(N_ELEMENTS(truncate_pc) EQ 0)THEN truncate_pc=1.
flag=0

truncate_pc=truncate_pc/100.

integfilternumber=filternumber

; truncate_filters assumes filter response are normalised to a peak
; throughput of 1 !
IF(truncate_filters)THEN BEGIN
      extract_filter,integfilternumber,thisfilter_wave,thisfilter_response
      
      ;;Find max and location thereof
      max_response = MAX( thisfilter_response, wpeak )
      
      trunc_value = max_response * truncate_pc

      ;; find lower edge
      FOR j=wpeak[0],0,-1 DO BEGIN
         IF thisfilter_response[j] LE trunc_value THEN BEGIN
            lower_edge=j+1
            BREAK
         ENDIF
      ENDFOR
      ;; find upper edge
      FOR j=wpeak[0],master_filter[integfilternumber].npoints-1,1 DO BEGIN
          IF thisfilter_response[j] LE trunc_value THEN BEGIN
              upper_edge=j-1
              BREAK
          ENDIF
      ENDFOR
      nfilters_defined=N_ELEMENTS(master_filter)
      newfilter_index=nfilters_defined
      
      ;; extend the master_filter structure with new entry for the
      ;; truncated filter
      master_filter=[master_filter,master_filter[integfilternumber]]
      
      ;;Pad the edges to avoid bad interpolations
      thisfilter_response=[0.,thisfilter_response[lower_edge:upper_edge],0.]
      thisfilter_wave=[ thisfilter_wave[lower_edge] - 5.0, $
                        thisfilter_wave[lower_edge:upper_edge],$
                        thisfilter_wave[upper_edge]+5. ]

      npoints=N_ELEMENTS(thisfilter_wave)
      master_filter[newfilter_index].npoints=npoints
      master_filter[newfilter_index].wave[0:npoints-1]=thisfilter_wave
      master_filter[newfilter_index].response[0:npoints-1]=thisfilter_response
      master_filter[newfilter_index].wave[npoints:*]=0.d0
      master_filter[newfilter_index].response[npoints:*]=0.d0
      
      master_filter[newfilter_index].id = $
        master_filter[integfilternumber].id+'TRUNC'
      master_filter[newfilter_index].shortid = $
        master_filter[integfilternumber].shortid+'TRUNC'

      integfilternumber=newfilter_index
ENDIF

filter_integral=0.d0

nfilterpoints = master_filter[integfilternumber].npoints
filter_wave = master_filter[integfilternumber].wave[0:nfilterpoints-1]
filter_response = master_filter[integfilternumber].response[0:nfilterpoints-1]

opz = 1.0d0 + redshift

;;Now we have to figure out which parts of the redshifted SED fall
;; in the range of the filter.  We have to be a bit careful to
;; avoid extrapolating.  Previously, we would (almost) always
;; interpolate the filter to the SED points, so that interpolation
;; was done on the filter rather than the SED.  However, this can
;; backfire in some circumstances because the SDSS type filters have
;; very sharp edges.  We still prefer to interpolate on the filter
;; function, which will generally be smoother, but if it is
;; sufficiently more finely sampled than the SED we go the other way.

; locate region where redshifted SED values into filter
ilower=VALUE_LOCATE(sed_wave,filter_wave[0]/opz) ;; this gives the array index where sed_wave <=filter_wave[0]/opz
iupper=VALUE_LOCATE(sed_wave,filter_wave[nfilterpoints-1]/opz) ;; this gives the array index where where sed_wave<=filter_wave[last]/opz

;; so filter will always be within sed at lower, and usually outside
;; sed at iupper

IF(ilower LT 0)THEN BEGIN
   IF(verbose) THEN PRINT,'FILTER_INTEG WARNING: Filter '+$
     master_filter[integfilternumber].shortid+' is bluer ('+$
     STRN(filter_wave[0]/opz)+') than SED ('+STRN(sed_wave[0])+')'
   flag=1
ENDIF
ilower>=0

IF(iupper GE (N_ELEMENTS(sed_wave)-1))THEN BEGIN
   IF(verbose) THEN PRINT,'FILTER_INTEG WARNING: Filter '+$
     master_filter[integfilternumber].shortid+' is redder than SED'
   flag=1
ENDIF

IF iupper LT ilower THEN BEGIN
    PRINT,'FILTER_INTEG ERROR: SED fails to overlap with filter '+$
          master_filter[integfilternumber].shortid 
    flag=2
    RETURN,!VALUES.D_NAN
ENDIF

IF ~ KEYWORD_SET( nochecknan ) THEN BEGIN
    w=WHERE(~ FINITE(sed_flux),count,COMPLEMENT=w1,NCOMPLEMENT=count1)
    IF(count GT 0)THEN BEGIN
        IF(verbose) THEN PRINT,'WARNING in FILTER_INTEG: input spectrum ',$
          'contains non-finite numbers, interpolating..'
        LINTERP,sed_wave[w1],sed_flux[w1],sed_wave[w],newflux
        mysed_flux=sed_flux
        mysed_flux[w]=newflux
        flag=4
    ENDIF ELSE mysed_flux=sed_flux
ENDIF ELSE mysed_flux = sed_flux

IF KEYWORD_SET( nodoublesample ) THEN BEGIN ;; Don't change to N_ELEMENTS !
    filter_integral=filter_integ_quick(filter_wave,filter_response,$
                                       sed_wave,mysed_flux,opz,nfilterpoints,$
                                       iupper,ilower, FNU=fnu, $
                                       FLAMBDA=flambda,INTEGWAVE=integwave,$
                                       INTEGFLUX=integflux)
ENDIF ELSE BEGIN
    filter_integral=filter_integ_dblsamp(filter_wave,filter_response,$
                                         sed_wave,mysed_flux,opz,$
                                         nfilterpoints,iupper,ilower, FNU=fnu,$
                                         FLAMBDA=flambda,INTEGWAVE=integwave,$
                                         INTEGFLUX=integflux)
ENDELSE

IF (filter_integral LT 0.) THEN BEGIN
    IF(verbose) THEN BEGIN
       PRINT,'FILTER_INTEG WARNING: Negative flux ('+STRN(filter_integral)+$
             ') through filter '+master_filter[integfilternumber].shortid
    ENDIF
    filter_integral=0.d0
    flag=3
ENDIF
; remove the truncated filter from the master_filter structure
IF(truncate_filters) THEN BEGIN 
  master_filter=master_filter[0:N_ELEMENTS(master_filter)-1-1]
ENDIF
RETURN,filter_integral

END
