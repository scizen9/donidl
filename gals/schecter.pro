function schecter, alpha, mstar, phistar, mag
;+
;
;-
mex = -0.4 * (mag - mstar)
n = 0.4 * alog(10.) * phistar * (10.^mex)^(alpha+1.) * exp(-10.^mex)
return, n
end

