pro rh3,wave


  rho=1.735

  ;; encoder fit:
  ;;      216969.
  ;;     0.222463
  ;;  -0.000852767


  ;; angle fit:  
  ;; -0.0784353
  ;;  0.000118029
  ;; -1.96050e-09


  
 encfit=[      216969.0d,     0.222463, -0.000852767]
angfit=[   -0.0784353d,  0.000118029, -1.96050e-09]


  wave=wave*1.0d

  sinangle=angfit[0]+angfit[1]*wave+angfit[2]*wave*wave
  angle=!radeg*asin(sinangle)
  encoder=encfit[0]+encfit[1]*wave+encfit[2]*wave*wave

  sinbeta=wave/10000.0*rho-sinangle

  beta=!radeg*asin(sinbeta)
  

  
  print,"alpha(AOI) = "+string(angle,"(f6.2)")+" deg"
  print,"encoder = "+string(encoder)
  print,"beta(AOD) ="+string(beta,"(f6.2)")+" deg"
  print,"camangle = "+string(angle+beta,"(f6.2)")+" deg"
  

  end
