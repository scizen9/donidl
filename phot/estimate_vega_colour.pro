;+
;NAME
; estimate_vega_colour
;PURPOSE
; Guess magnitude of Vega in specified colour on the
; Landolt system using linear interpolation.
;USAGE
; vega_colour = estimate_vega_colour( filter, vega_struct )
;INPUTS
; filter         The filter for which to return the filter - V mag in
; vega_struct    Structure holding information about Vega -- see
;                 read_vega
;OPTIONAL INPUTS
; landoltfilters Which filters you want to represent the Landolt ones.
;                 This has to be the same size as colours.
;                 The default is ['U','B','V','R','I']
; colours        The relative colours to interpolate on.  These are
;                 the colours relative to whatever you want, although
;                 the expected usage is relative to V_vega, which is
;                 how the rest of the documentation announces it.
;                 The default is [ -0.01, 0.0, 0.0, 0.0, -0.06 ] 
;                 corresponding to the values in Fukugita et al. 1996.
;                 Note that the last two are R-V and I-V, not V-R/V-I.
;RETURNS
; The filter - V_vega colour
;NOTES
; The way this is done is very arbitrary, as it must be.  Linear
; interpolation on the effective wavelength of the filter with respect
; to vega is used, with some arbitrary boundary conditions that ensure
; extreme filters go to zero colour.
; In the infrared we set the magnitude of Vega to zero, which is
; convention -- a filter is taken to be infrared if it's effective
; wavelength is greater than 1 micron
;MODIFICATION HISTORY
; Author: Alex Conley, June 2006
;-

FUNCTION estimate_vega_colour, filter, vega_struct,$
  COLOURS=colours, LANDOLTFILTERS=landoltfilters

IF N_ELEMENTS( landoltfilters ) EQ 0 THEN $
  landoltfilters = ['U','B','V','R','I']
landoltnumbers = get_filternumber( landoltfilters )
nland = N_ELEMENTS( landoltnumbers )
IF N_ELEMENTS( colours ) EQ 0 THEN colours = [ -0.01, 0.0, 0.0, 0.0, -0.006 ]

IF N_ELEMENTS( colours ) NE nland THEN $
  MESSAGE,"Number of relative colours and number of filters must match"

IF SIZE( filter, /TYPE ) EQ 7 THEN BEGIN
    filternumber = get_filternumber( filter )
ENDIF ELSE filternumber = filter

;;Get the relevant effective wavelengths
landolteffwave = FLTARR( N_ELEMENTS( landoltnumbers ) )
FOR i=0, nland - 1 DO $
  landolteffwave[i] = effective_wavelength( landoltnumbers[i],$
                                            vega_struct.wave,$
                                            vega_struct.flux )
efwav = effective_wavelength( filternumber, vega_struct.wave, $
                              vega_struct.flux )

;;Set up interpolation
mineffwave = MIN( landolteffwave )
maxeffwave = MAX( landolteffwave )
deltawave = maxeffwave - mineffwave
xvals = [ mineffwave - deltawave, mineffwave - deltawave/2, $
          mineffwave - deltawave/4, landolteffwave, maxeffwave+deltawave/4, $
          maxeffwave+deltawave/2, maxeffwave + deltawave ]
yvals = [ 0.0, 0.0, colours[0]/2.0, colours, colours[nland-1]/2.0, 0.0, 0.0 ]
interpvals = INTERPOL( yvals, xvals, efwav )

;;Deal with infrared, where the mag of vega is set to zero
winfra = WHERE( efwav GT 10000.0, count )
IF count NE 0 THEN interpvals[winfra] = - vega_struct.vmag

RETURN,interpvals

END
