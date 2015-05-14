function fwhm1d,x,y,xl,xu
;
peak = max(y)
;
ipk = where(y eq peak, n)
if n gt 1 then return,-1	; must have a single peak
;
ipk = ipk[0]
;
if ipk le 0 or ipk ge n_elements(x)-1 then return,-1	; can't be at edge
;
; upper half
xl = interpol(x[0:ipk],y[0:ipk],peak/2.)
;
; upper half
xu = interpol(x[ipk:*],y[ipk:*],peak/2.)
;
return,xu-xl
end
