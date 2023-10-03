pro rh1,wave


  rho=2.360

  ;; encoder fit:
 ;;       247497.
 ;;    -9.55989
;; -0.000796375

 ;; -0.293876
 ;; 0.000230592
 ;;-9.22712e-09


  
  encfit=[      247497.0d,    -9.55989, -0.000796375 ]
  
  angfit=[  -0.293876d,   0.000230592, -9.22712e-09 ]


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
