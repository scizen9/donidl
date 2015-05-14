;+
;NAME
; theil
;PURPOSE
; Returns the Theil estimator of the slope and intercept of a set
; of data points
;USAGE
; result = theil( xdata, ydata )
;INPUTS
; xdata              X values
; ydata              Y values
;KEYWORDS
; complete           By default the incomplete Theil estimator is
;                     returned if the number of points is greater than
;                     75; setting this switch forces the complete
;                     estimator, which is more expensive to calcluate
; incomplete         Similarly, this forces the use of the incomplete
;                     estimator even for fewer than 75 points
;RETURNS
; A two element vector of [ slope, intercept ]
;OPTIONAL OUTPUTS
; sigma              A two element vector giving the error estimates
;                     for slope,intercept. This is not at all meant to
;                     be rigorous, and is simply the median absolute
;                     deviation of the slope values
;SEE ALSO
; theil_iterate      Similar, but with rejection of outlier points
;NOTES
; The incomplete Theil estimator requires the computation of
;  ndata/2 slopes, the complete one ndata * ( ndata - 1 ) / 2 slopes.
;  The branch at 1000 data points was arbitrarily chosen, but is
;  set to avoid using too much memory.
;MODIFICATION HISTORY
; Author: Alex Conley, May 2006
;-

FUNCTION theil, xdata, ydata, COMPLETE=complete, INCOMPLETE=incomplete,$
                SIGMA=sigma

branch_complete = 1000

ndat = N_ELEMENTS( xdata )
IF ndat LT 2 THEN RETURN,REPLICATE(!VALUES.F_NAN,2)
IF ndat NE N_ELEMENTS( ydata ) THEN $
  MESSAGE,"Number of x and y points not the same"

IF KEYWORD_SET( complete ) THEN slopes = theil_complete( xdata, ydata ) $
  ELSE IF KEYWORD_SET( incomplete ) THEN $
    slopes = theil_incomplete( xdata, ydata ) ELSE $
      IF ndat LE branch_complete THEN slopes = theil_complete( xdata, ydata ) $
  ELSE slopes = theil_incomplete( xdata, ydata )

;;Remove non-finite slopes
wgood = where( FINITE( slopes ), ngood )
IF ngood EQ 0 THEN MESSAGE,"No finite slopes"
slopes = slopes[wgood]


medslope = MEDIAN( slopes )
IF ARG_PRESENT( sigma ) THEN slopedisp = MEDIAN( ABS( slopes - medslope ) )
intercepts = ydata - medslope * xdata
DELVARX, slopes

medintercept = MEDIAN( intercepts )
IF ARG_PRESENT( sigma ) THEN $
  sigma = [ slopedisp, MEDIAN( ABS( intercepts - medintercept ) ) ]
RETURN,[medslope,medintercept]

END
