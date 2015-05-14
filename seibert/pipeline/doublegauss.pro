
;*******************************************************************************
function f2gauss, x, coef  
;*******************************************************************************
;
; Function which returns the sum of two gaussians and their partial derivatives
;
; coef[0] = ampl1, coef[1] = center1, coef[2] = fwhm1
; coef[3] = ampl2, coef[4] = center2, coef[5] = fwhm2
;
;*******************************************************************************

n = n_elements(coef)
pder = fltarr(n_elements(x), n)

if n ne 6 then BEGIN
  print, 'ERROR!! Wrong number of parameters.'
  f=x*0.
  pder[*,*]=0.
  GOTO, skip 
ENDIF

; lets run through the first gaussian

IF (coef[2] LE 0.0 AND coef[0] ne 0.0) THEN BEGIN
  print, 'ERROR!! Sigma1 = 0.0'
  f1=x*0.
  pder[*,0:2]=0.  
endif

IF coef[0] EQ 0.0 THEN BEGIN
  f1=x*0.
  pder[*,0:2] = 0.
ENDIF ELSE begin  
  z1 = (x - coef[1]) / coef[2]
  f1 = coef[0] * exp(-(z1^2) / 2)
  pder = fltarr(n_elements(x), n)  
  pder[*, 0] =  f1 / coef[0]
  pder[*, 1] =  f1 * z1 / coef[2]
  pder[*, 2] =  f1 * z1^2 / coef[2] 
endelse

; now lets do the same for the second

IF (coef[5] LE 0.0 AND coef[3] ne 0.0) THEN BEGIN
  print, 'ERROR!! Sigma2 = 0.0'
  f2=x*0.
  pder[*,3:5]=0.  
endif

IF coef[3] EQ 0.0 THEN BEGIN
  f2=x*0.
  pder[*,3:5] = 0.
ENDIF ELSE begin  
  z2 = (x - coef[4]) / coef[5]
  f2 = coef[3] * exp(-(z2^2) / 2)
  pder[*, 3] =  f2 / coef[3]
  pder[*, 4] =  f2 * z2 / coef[5]
  pder[*, 5] =  f2 * z2^2 / coef[5] 
ENDELSE

f = f1 + f2

skip:

return, [[f],[pder]] 

end

;*******************************************************************************
pro doublegauss, x, coef, f, pder 
;*******************************************************************************

gfitcoef = f2gauss(x, coef)
f = gfitcoef[*, 0]
if n_params() ge 4 then pder = gfitcoef[*,1:*]

end
