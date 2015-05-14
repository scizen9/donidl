;+
;NAME
; theil_complete
;MODIFICATION HISTORY
; Author: Alex Conley, May 2006
;-

;;calculate slopes for complete method
FUNCTION theil_complete, xdata, ydata

ndat = N_ELEMENTS( xdata )
nslopes = ndat * (ndat - 1)/2
slopes = FLTARR( nslopes )

lowindx = 0  ;;current position counter
FOR i = 0, ndat - 2 DO BEGIN   ;;only to ndat-2 since last entry already done
    ;;We have to put some effort into avoiding points where delta_x is
    ;; zero.  This makes the code a bit ungainly.  We set these points
    ;; to have NaN slopes, which are removed later
    dy = ydata[i+1:ndat-1] - ydata[i] 
    dx = xdata[i+1:ndat-1] - xdata[i]
    wgood = WHERE( dx NE 0.0, ngood, COMPLEMENT=wbad, NCOMPLEMENT=nbad )
    slopeindicies = indgen( ndat - i - 1 ) + lowindx ;;Indicies in slope
    IF nbad EQ 0 THEN slopes[slopeindicies] = dy / dx ELSE BEGIN
        IF ngood NE 0 THEN slopes[ slopeindicies[wgood] ] = dy[wgood]/dx[wgood]
        slopes[ slopeindicies[wbad] ] = !VALUES.F_NAN
    ENDELSE
    lowindx += ndat - i - 1
ENDFOR

RETURN,slopes

END

