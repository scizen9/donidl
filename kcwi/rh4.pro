pro rh4,wave


  rho=1.490

  ;; encoder fit:
 ;;     253323.
 ;;    -7.40071
 ;;    -0.000237583
     
 ;;   -0.282844
 ;; 0.000144208
 ;; -3.66047e-09


  
encfit=[       253323.0d,     -7.40071,     -0.000237583]
angfit=[    -0.282844d,  0.000144208, -3.66047e-09]


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
