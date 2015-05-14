;+
;NAME
; calc_vega_zp
;PURPOSE
; Cacluate vega zeropoint for filters
;USAGE
; vegazp = calc_vega_zp(filternum,vega_struct)
;INPUTS
; filternum          Number of filter in usual system (i.e., index
;                      of filter in filter_info:master_filter).  This
;                      can be an array
; vega_struct        structure containing vega info -- see read_vega
;OPTIONAL INPUT/OUTPUT
; vegacolsys         The colours of Vega to use. Choices are 'astier',
;                     'calspec' or 'johnson' Default is 'johnson'
; landoltfilttype    Which landolt filters to use.  Options are:
;                     'bessell' (default), 'cohen', 'stritzinger',
;                      'shift' and 'astier'.  This matters for the
;                      CALSPEC colour set
;OPTIONAL OUTPUT
; vega_colour        The filter-V colour of Vega
;KEYWORD PARAMETERS:
;COMMON BLOCKS
; filter_info        Not modified
;NOTE
; vegazp is NOT set by this function, simply returned
; For CFHT filters we use the transformational equations to set
;  the zeropoints
; For non-cfht filters, we use linear interpolation on effective
;  wavelength to set the zeropoints.  For infrared filters we
;  set the mags of Vega to zero, as is conventional.
;AUTHOR
; Mark Sullivan
;-

FUNCTION calc_vega_zp,filternum,vega_struct,VEGACOLSYS=vegacolsys,$
  LANDOLTFILTTYPE=landoltfilttype,VEGA_COLOUR=vega_colour,$
  VEGA_MAG=vega_mag, SUCCESS=success

COMMON filter_info, master_filter

success = 0b

;; vegacolsys is the colour system used for VEGA. The default is
;; Johnson-Morgan-Taylor colours in the Landolt system.
;;Colours are always filter - V; be careful of the signs (i.e., R-V,
;;                            not V-R)
IF(N_ELEMENTS(vegacolsys) EQ 0)THEN vegacolsys='johnson'

IF N_ELEMENTS( landoltfilttype ) EQ 0 THEN landoltfilttype = 'bessell'

;;Choose the filter functions
baselandoltfilters = ['U','B','V','R','I']
landoltfilters = adjust_filtershorts( baselandoltfilters,$
                                      LANDOLTFILTTYPE=landoltfilttype )

CASE vegacolsys OF
    'astier' : BEGIN
        ;;These are the Johnson-Morgan-Taylor colours but not in the
        ;; Bessell system
        colours = [-0.01, 0.0, 0.0, 0.0, -0.006 ]
    END
    'calspec' : BEGIN
        ;;Using the colours based on the CALSPEC SEDs of Landolt standards
        ;;This depends on which SED of Vega we are using
        ;; and on which filter set
        ;; bessell are the Bessell 1990 filters
        ;; cohen are the Cohen 2003 filters
        ;; shift are the shifted Bessell filters using the S05 SED
        ;;   library; see
        ;;   http://qold.astro.utoronto.ca/conley/snls_research/calibration/landolt_filters.pdf
        ;;   The shifts are: +9.4, +3.8, +23.9, +17.6 for BVRI (Angstroms)
        ;; astier are the shifted Bessell filters as used in Astier '06
        ;;   The shifts are -41, -27, -21, -25 for BVRI (Angstroms)
        ;;More info about this approach can be found at
        ;;http://qold.astro.utoronto.ca/conley/snls_research/calibration/vegamags.html
        CASE vega_struct.sedtype OF
            'stis' : BEGIN
                ;;The Sep 2006 updated STIS SED of Vega
                CASE landoltfilttype OF
                    'bessell' : colours = [ 0.036, -0.018, 0.0, 0.006, -0.007 ]
                    'cohen'   : colours = [ 0.097, -0.005, 0.0, 0.003, -0.018 ]
                    'shift'   : colours = [ 0.037, -0.025, 0.0, 0.004, -0.007 ]
                    'astier'  : colours = [ 0.040, -0.037, 0.0, 0.007, -0.006 ]
                ENDCASE
            END
            'stisold' : BEGIN
                ;;The original Bohlin STIS SED of Vega (2004)
                CASE landoltfilttype OF
                    'bessell' : colours = [ 0.036, -0.018, 0.0, 0.006, -0.007 ]
                    'cohen'   : colours = [ 0.097, -0.005, 0.0, 0.003, -0.018 ]
                    'shift'   : colours = [ 0.037, -0.025, 0.0, 0.004, -0.007 ]
                    'astier'  : colours = [ 0.040, -0.037, 0.0, 0.007, -0.006 ]
                ENDCASE
            END
            'hayes85' : BEGIN
                ;;The Hayes (1985) SED -- uses a different normalization
                CASE landoltfilttype OF
                    'bessell' : colours = [ 0.032, -0.038, 0.0, 0.008, -0.008 ]
                    'cohen'   : colours = [ 0.093, -0.022, 0.0, 0.004, -0.021 ]
                    'shift'   : colours = [ 0.033, -0.044, 0.0, 0.006, -0.008 ]
                    'astier'  : colours = [ 0.033, -0.015, 0.0, 0.006, -0.009 ]
                ENDCASE
            END
            ELSE : BEGIN
                PRINT,"ERROR in calc_vega_zp: Vega SED type: ",$
                      vega_struct.sedtype," not supported for"
                PRINT," CALSPEC vega magnitudes"
                RETALL
            END
        ENDCASE
    END
    'johnson' : BEGIN
        ;;This is the DEFAULT
        ;;These are the Johnson-Morgan-Taylor colours in the Landolt
        ;; system.  These are almost identical to useastier above
        colours = [-0.004, -0.002, 0.0, +0.006, 0.0 ]
    END
    ELSE : BEGIN
        PRINT,'ERROR in calc_vega_zp: vegacolsys '+vegacolsys+' not recognised'
        RETALL
    END
ENDCASE

nfilts = N_ELEMENTS( filternum )
vega_colour = DBLARR( nfilts )

;;Figure which ones are MegaCam filters, since these have to be
;; handled differently
wcfht = WHERE( STREGEX(master_filter[filternum].id,'megacam',/FOLD_CASE,$
                       /BOOLEAN), ncfht, COMPLEMENT=wnotcfht, $
               NCOMPLEMENT=nnotcfht )

;;Handle non-CFHT filters by interpolation
IF nnotcfht GT 0 THEN $
  vega_colour[wnotcfht] = $
  estimate_vega_colour(filternum[wnotcfht],vega_struct,colours=colours,$
                       LANDOLTFILTERS=landoltfilters)

;;The CFHT filters are a bit complicated.  In order to ensure
;; consistency, we have to make these agree with the colour
;; transform equations used to establish the Vega zeropoint
;; So, for example, if g = V + 0.5 (B-V)
;; then we must have g_vega - V_vega = 0.5 ( B_vega - V_vega )
;; Ultimately, these coefficients may change, but since the colour
;; terms are farly small, right now I am using the Sullivan colour
;; terms as of July 24, 2006, broken linear versions
;;We have:
;;  u_vega - U_vega = -0.2868 * (U_vega - B_vega)
;;  u_vega - V_vega = -0.2868 * (U_vega - B_vega) + (U_vega - V_vega)
;;  u_vega - V_vega = -0.2868 * (U_vega-V_vega) + 0.2868*(B_vega-V_vega) + (U_vega - V_vega)
;;  u_vega - V_vega = (1.-0.2868)*(U_vega-V_vega) + 0.2868*(B_vega-V_vega)

;;  g_vega - V_vega = 0.5018 * (B_vega - V_vega)

;;  r_vega - R_vega = 0.1540 * (V_vega - R_vega) =>
;;   r_vega - V_vega = 0.1540 * (V_vega - R_vega) + (R_vega - V_vega) =>
;;   r_vega - V_vega = (1.0 - 0.1540)*(R_vega-V_vega)

;;  i_vega - I_vega = 0.1824 ( R_vega - I_vega ) =>
;;   i_vega - V_vega = (1.0 - 0.1824)*(I_vega-V_vega) + 0.1824*(R_vega-V_vega)
;;  z_vega - I_vega = -0.2678*(R_vega-I_vega) =>
;;   z_vega - V_vega = (1.0 + 0.2678)*(I_vega-V_vega) - 0.2678*(R_vega-V_vega)
IF ncfht NE 0 THEN BEGIN
    FOR i = 0, ncfht-1 DO BEGIN
        cfiltstring = STRMID(master_filter[filternum[wcfht[i]]].shortid,0,2)
        CASE cfiltstring OF
            'us' : vega_colour[wcfht[i]] = (1.-0.2868)*colours[0]+0.2868*colours[1]
            'gs' : vega_colour[wcfht[i]] = 0.5018 * colours[1]
            'rs' : vega_colour[wcfht[i]] = (1.0 - 0.1540)*colours[3]
            'is' : vega_colour[wcfht[i]] = (1.0 - 0.1824)*colours[4] + 0.1824 * colours[3]
            'zs' : vega_colour[wcfht[i]] = (1.0 + 0.2678)*colours[4] - 0.2678 * colours[3]
            ELSE : MESSAGE,"Unknown CFHT filter type: "+$
                           master_filter[filternum[wcfht[i]]].shortid
        ENDCASE
    ENDFOR
ENDIF

vega_mag = vega_colour + vega_struct.Vmag

retarr = DBLARR( nfilts )
FOR i=0,nfilts-1 DO $
  retarr[i] = filter_integ(filternum[i],vega_struct.wave,$
                           vega_struct.flux,0.d0) * $
  10.0d0^(0.4d0 * vega_mag[i] )

success = 1b

IF SIZE( filternum, /DIMENSION ) EQ 0 THEN RETURN,retarr[0] ELSE $
  RETURN,retarr

END
