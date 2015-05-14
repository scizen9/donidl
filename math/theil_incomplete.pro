;+
;NAME
; theil_incomplete
;MODIFICATION HISTORY
; Author: Alex Conley, May 2006
;-

;;calculate slopes for incomplete method
FUNCTION theil_incomplete, xdata, ydata

ndat = N_ELEMENTS( xdata )
nslopes = ndat / 2
slopes = FLTARR( nslopes )

iseven = ndat mod 2 EQ 0
low_low_index = 0
low_high_index = ndat/2 - 1
high_low_index = ndat/2 + iseven
high_high_index = ndat-1


dy = ydata[high_low_index:high_high_index] - $
     ydata[low_low_index:low_high_index]
dx =  xdata[high_low_index:high_high_index] - $
      xdata[low_low_index:low_high_index]

;;Don't use points with no delta_x.  For this version, just don't
;; return them
wgood = WHERE( dx NE 0, count )
IF count EQ 0 THEN MESSAGE,"All points have same x value in test"
slopes = dy[wgood]/dx[wgood]

RETURN,slopes

END
