pro radprof2_fn2,x,a,f,pder

  COMMON FITPHOT,SKYV1,SIGMAV1,BETA4,BETA6

  nel=n_elements(a)
  b=a
  fudgeflag=1.0

  if (nel eq 2) then begin
    if ((SKYV1 ne -999) and (SIGMAV1 ne -999)) then begin
      b=[a(0),SKYV1,SIGMAV1,BETA4,BETA6]
      fudgeflag=a(1)	; cuz curvefit is a bitch, add a second amplitude
      endif
    if ((SKYV1 eq -999) and (SIGMAV1 ne -999)) then b=[a,SIGMAV1,BETA4,BETA6]
    if ((SIGMAV1 eq -999) and (SKYV1 ne -999)) then b=[a(0),SKYV1,a(1),BETA4,BETA6]
    endif
  if (nel eq 3) then b=[a,BETA4,BETA6]
  if (nel eq 4) then b=[a,BETA6]

  z = x/(sqrt(2d)*b(2))
  tmp1=where(z eq 0.0)
  if (tmp1(0) ne -1) then z(tmp1)=0.001

  f = b(0)/(1. + z^2 + 0.5*b(3)*z^4 + (1/6.)*b(4)*z^6 )*fudgeflag + b(1)

  if (n_params(0) lt 4) then return
  pder = fltarr(n_elements(x),n_elements(a))
  pder(*,0)=1./(1. + z^2 + 0.5*b(3)*z^4 + (1/6.)*b(4)*z^6)*fudgeflag
  dfdt = b(0)*(-1.)*pder(*,0)*pder(*,0)

  pder(*,1) = 1.0
  if (nel eq 2) then begin
    if ((SKYV1 ne -999) and (SIGMAV1 ne -999)) then $
      pder(*,1) = b(0)/(1. + z^2 + 0.5*b(3)*z^4 + (1/6.)*b(4)*z^6 )
    if ((SKYV1 eq -999) and (SIGMAV1 ne -999)) then pder(*,1) = 1.0
    if ((SIGMAV1 eq -999) and (SKYV1 ne -999)) then $
      pder(*,1) = dfdt*(2*z + 2.*b(3)*z^3 + b(4)*z^5)*(-1.)*z/b(2)
    endif
  if (nel ge 3) then pder(*,2) = dfdt*(2*z + 2.*b(3)*z^3 + b(4)*z^5)*(-1.)*z/b(2)
  if (nel ge 4) then pder(*,3) = dfdt*0.5*z^4
  if (nel eq 5) then pder(*,4) = dfdt*(1/6.)*z^6

  return
end
