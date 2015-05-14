; $Id: biasfunc.pro,v 1.1 2014/05/22 23:19:01 neill Exp $
;+
; biasfunc - TFR bias function for use with MPFITFUN
;-
function biasfunc, x, p
	return,p[0] * x ^ p[1]
end
