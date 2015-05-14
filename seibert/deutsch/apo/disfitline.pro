pro disfitline,wl,spec,halfwidth=halfwidth,coeff=coeff

  if (n_elements(halfwidth) eq 0) then halfwidth=10

  plot,wl,spec,xsty=1

  print,'Click on line to fit...'
  crd,/data,x,y
  tmp1=abs(wl-x)
  xpos=where(tmp1 eq min(tmp1)) & xpos=xpos(0)

  w1=wl(xpos-halfwidth:xpos+halfwidth)
  s1=spec(xpos-halfwidth:xpos+halfwidth)
;  plot,w1,s1,xsty=1,psym=-4
  plot,w1,s1,xsty=1,psym=10,ysty=16

  fit=gaussfit(w1,s1,coeff,nterms=4)
  oplot,w1,fit,psym=-4,color=!d.n_colors-2

  print,'Amplitude = ',strn(coeff(0))
  print,'Center    = ',strn(coeff(1))
  print,'FWHM      = ',strn(coeff(2)*2.35),'      sigma=',strn(coeff(2))
  print,'continuum = ',strn(coeff(3))
  print,'area      = ',strn(abs(coeff(0)*sqrt(!pi*(2*coeff(2)^2))))
  print,'EW        = ',strn(abs(coeff(0)*sqrt(!pi*(2*coeff(2)^2)))/coeff(3))

  return
end
