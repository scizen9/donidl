pro radprof2_fn,x,a,f,pder

  COMMON FITPHOT,SKYV1,SIGMAV1,BETA4,BETA6

  nel=n_elements(a)
  b=a
  if (nel eq 2) then begin
    if ((SKYV1 ne -999) and (SIGMAV1 ne -999)) then b=[a(0),SKYV1,SIGMAV1]
    if ((SKYV1 eq -999) and (SIGMAV1 ne -999)) then b=[a,SIGMAV1]
    if ((SIGMAV1 eq -999) and (SKYV1 ne -999)) then b=[a(0),SKYV1,a(1)]
    endif

  f = b(0) * exp(-x^2/(2*b(2)^2)) + b(1)

  if (n_params(0) lt 4) then return
  pder = fltarr(n_elements(x),n_elements(a))
  pder(*,0) = exp(-x^2/(2*b(2)^2))

  pder(*,1) = 1.0
  if (nel eq 2) then begin
    if ((SKYV1 ne -999) and (SIGMAV1 ne -999)) then pder(*,1) = 2.222
    if ((SKYV1 eq -999) and (SIGMAV1 ne -999)) then pder(*,1) = 1.0
    if ((SIGMAV1 eq -999) and (SKYV1 ne -999)) then $
      pder(*,1) = b(0) * exp(-x^2/(2*b(2)^2)) * x^2/b(2)^3
    endif

  if (n_elements(a) eq 3) then pder(*,2) = a(0) * exp(-x^2/(2*b(2)^2)) * x^2/b(2)^3

  return
end
