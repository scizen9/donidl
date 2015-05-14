; $Id: linfunc.pro,v 1.1 2013/12/17 18:15:31 neill Exp $
;+
; curfunc - linear function for use with MPFITFUN
;-
function curfunc, x, p
	return,p[0] + x * p[1] + x^2 * p[2]
end
