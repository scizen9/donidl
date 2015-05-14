pro splinf,x,y,n,yp1,ypn,y2
;+
;			splinf
;
; Given arrays X and Y of length N containing a tabulated function, i.e.
; Yi = f(Xi), with X1 < X2 < ... < Xn, and given values YP1 and YPN for the
; first derivative of the interpolating function at points 1 and N,
; respectively, this routine returns and array Y2 of length N which contains
; the second derivatives of the interpolating function at the tabulated points
; Xi.  If YP1 and/or YPN are equal to 1.E30 or larger, the routine is signalled
; to set the corresponding boundary condition for a natural spline, with zero
; second derivative on that boundary.
;
; SOURCE:
;	Numerical Recipes, 1986. (page 88)
; 
; CALLING SEQUENCE:
;	splinf,x,y,n,yp1,ypn,y2
;
; INPUTS:
;	x - independent variable vector
;	y - dependent variable vector
;	n - dimension of x and y vectors
;	yp1 - first derivative at x(0)
;	ypn - first derivative at x(n-1)
;
; OUTPUTS:
;	y2 - second derivatives at all x, of length n
;
; HISTORY:
;	converted to IDL, D. Neill, October, 1991
;-
y2 = fltarr(n)
u = fltarr(n)
;
; The lower boundary condition is set either to be "natural"
;
if yp1 gt .99e30 then begin
	y2(0) = 0.
	u(0) = 0.
;
; or else to have a specified first derivative
;
endif else begin
	y2(0) = -0.5
	u(0) = ( 3. / ( x(1)-x(0) ) ) * ( ( y(1)-y(0) ) / ( x(1)-x(0) ) - yp1 )
endelse
;
; This is the decomposition loop of the tridiagonal algorithm.  Y2 and
; U are used for temporary storage of the decomposed factors.
;
for i=1,n-2 do begin
	sig = ( x(i)-x(i-1) ) / ( x(i+1)-x(i-1) )
	p = sig * y2(i-1) + 2.
	y2(i) = ( sig-1. ) / p
	u(i)=( 6. * ( ( y(i+1)-y(i) ) / ( x(i+1)-x(i) ) - ( y(i)-y(i-1) ) $
		/ ( x(i)-x(i-1) ) ) / ( x(i+1)-x(i-1) ) - sig*u(i-1) ) / p
endfor
;
; The upper boundary condition is set either to be "natural"
;
if ypn gt .99e30 then begin
	qn=0.
	un=0.
;
; or else to have a specified first deriviative
;
endif else begin
	qn=0.5
	un=( 3. / ( x(n-1)-x(n-2) ) ) * ( ypn - ( y(n-1)-y(n-2) ) $
		/ ( x(n-1)-x(n-2) ) )
endelse
;
y2(n-1) = ( un - qn * u(n-2) ) / ( qn * y2(n-2) + 1. )
;
; This is the backsubstitution loop of the tridiagonal algorithm
;
for k=n-2,0,-1 do begin
	y2(k) = y2(k) * y2(k+1) + u(k)
endfor
;
return
end	; splinf.pro
