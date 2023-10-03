pro rh2,wave


  rho=2.02

  ;; encoder fit:
  ;;      212573.
  ;;   1.65586
  ;;    -0.00126732

  ;; angle fit:
  ;;  -0.0886470
  ;; 0.000139173
  ;; -2.68953e-09



  
  encfit=[212573.0d,1.65586, -0.00126732]
  angfit=[ -0.0886470,0.000139173, -2.68953e-09]
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
