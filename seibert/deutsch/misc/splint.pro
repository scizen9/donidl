pro splint,xa,ya,y2a,n,x,y
;+
;			splint
;
; Given arrays XA and YA of length N, which tabulate a function (with the
; XA's in order), and given the array Y2A, which is the output from SPLINE.PRO,
; and given a value of X this routine returns a cubic-spline interpolated
; value Y.
;
; SOURCE:
;	Numerical Recipes, 1986. (page 89)
; 
; CALLING SEQUENCE:
;	splint,xa,ya,y2a,n,x,y
;
; INPUTS:
;	xa - independent variable vector
;	ya - dependent variable vector
;	y2a- second derivative vector from SPLINF.PRO
;	n  - length of above vectors
;	x  - x value of interest
;
; OUTPUTS:
;	y  - cubic-spline interpolated value at x
;
; HISTORY:
;	converted to IDL, D. Neill, October, 1991
;-
; We will find the right place in the table by means of bisection.  This is
; optimal if sequential calls to this routine are at random values of x.  If
; sequential calls are in order, and closely spaced, one would do better to
; store previous values of KLO and KHI and test if they remain appropriate on
; the next call.
;
klo = 0
khi = n-1
;
one:
	if khi-klo gt 1 then begin
		k = ( khi+klo ) / 2
		if xa(k) gt x then khi = k $
			      else klo = k
		goto, one
	endif
;
; KLO and KHI now bracket the input value of X
;
h = xa(khi) - xa(klo)
if h eq 0. then begin
	print,'SPLINT - XA inputs must be distinct'
	retall
endif
;
; Cubic spline polynomial is now evaluated
;
a = ( xa(khi) - x ) / h
b = ( x - xa(klo) ) / h
y = a * ya(klo) + b * ya(khi) + $
    ( ( a^3 - a ) * y2a(klo) + ( b^3 - b ) * y2a(khi) ) * ( h^2 ) / 6.
;
return
end	; splint.pro
