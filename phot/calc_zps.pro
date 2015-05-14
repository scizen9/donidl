;+
;NAME
; calc_zps
;PURPOSE
; Cacluate vega and AB zeropoints for all filters in master_filter
;  as well as the filter areas
;USAGE
; calc_zps, vega_struct
;INPUTS
; vega_struct        structure containing vega info -- see read_vega
;OPTIONAL INPUT
; vegacolsys         The colours of Vega to use. Choices are 'astier',
;                     'calspec' or 'johnson' Default is 'johnson'
; landoltfilttype    Which landolt filters to use.  Options are:
;                     'bessell' (default), 'cohen', 'stritzinger',
;                      'shift' and 'astier'.  This matters for the
;                      CALSPEC colour set
;COMMON BLOCKS
; filter_info        Modified.  vegazp, abzp, ab_offset, area, 
;                     vegacolsys, vega_colour, mean_wavelength are
;                     set.  Does not update temporary filters!
;PROCEDURES CALLED
; calc_vega_zp
; calc_ab_zp
; mean_wavelength
;NOTES
; After this is called, the difference between the Vega and AB mags
; through a given filter are -2.5*alog10( vegazp/abzp ) - 48.6
;AUTHOR
; Alex Conley
;-

PRO calc_zps, vega_struct, VEGACOLSYS=vegacolsys,$
              LANDOLTFILTTYPE=landoltfilttype

COMPILE_OPT IDL2, STRICTARRSUBS

COMMON filter_info,master_filter

IF(N_ELEMENTS(vegacolsys) EQ 0)THEN vegacolsys='johnson'

hastmp = TAG_EXIST(master_filter,'temporary',/TOP_LEVEL)
FOR i=0,N_ELEMENTS(master_filter)-1 DO BEGIN

    ;;Don't do temporary filters
    IF hastmp && master_filter[i].temporary THEN CONTINUE

    ;;Check to see if there is enough coverage in the vega
    ;; spectrum to get the vega zeropoint
    fnpoints = master_filter[i].npoints
    min_fwav = MIN( master_filter[i].wave[0:fnpoints-1] )
    max_fwav = MAX( master_filter[i].wave[0:fnpoints-1] )
    IF ( min_fwav LT vega_struct.minwave OR $
         max_fwav GT vega_struct.maxwave ) THEN BEGIN
        PRINT,"Can't calculate Vega zeropoint for: ",$
              master_filter[i].shortid
        PRINT," Insufficient wavelength coverage for Vega"
        vegazp = !VALUES.F_NAN
        vega_colour = !VALUES.F_NAN
    ENDIF ELSE $
       vegazp = calc_vega_zp( i, vega_struct,VEGACOLSYS=vegacolsys,$
                              LANDOLTFILTTYPE=landoltfilttype,$
                              VEGA_COLOUR=vega_colour, VEGA_MAG=vega_mag)
    master_filter[i].vegacolsys = vegacolsys
    master_filter[i].vega_colour = vega_colour[0]
    master_filter[i].vega_mag = vega_mag[0]
    abzp = calc_ab_zp( i )
    master_filter[i].vegazp = vegazp
    master_filter[i].abzp = abzp
    IF abzp NE 0 AND vegazp NE 0 THEN $
      master_filter[i].ab_offset = -2.5*alog10(vegazp/abzp) - 48.6
    master_filter[i].area_lambda = filter_area( i )
    master_filter[i].area_nu = filter_area_nu( i )
    master_filter[i].mean_wave = mean_wavelength( i )
ENDFOR

RETURN
END
