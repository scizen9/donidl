FUNCTION poisson, x, p

; p=[mean]

 lambda = p(0)
 ;k=lindgen(ceil(lambda)*3 > 5)
 k = x 
 f = ((1d*lambda)^(k) * exp(-lambda))/factorial(k)
 ymod = 1.0*f/max(f)

 return, YMOD

END
