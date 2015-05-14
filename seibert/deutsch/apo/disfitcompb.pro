pro disfitcompb,wl,spec,wl2

  lines=[4358.33,4387.929,4471.477,4713.143,4921.929,5015.675,5047.740]
  fitwl=lines*0 &  fitfx=fitwl
  halfwidth=10

  xax=findgen(n_elements(wl))
  for i=0,n_elements(lines)-1 do begin
    wlt=lines(i)
    tmp1=abs(wlt-wl)
    xpos=where(tmp1 eq min(tmp1)) & xpos=xpos(0)
    w1=xax(xpos-halfwidth:xpos+halfwidth)
    s1=spec(xpos-halfwidth:xpos+halfwidth)
    plot,w1,s1,xsty=1,psym=10,ysty=16
    fit=gaussfit(w1,s1,coeff)
    oplot,w1,fit,psym=-4,color=!d.n_colors-2
    fitwl(i)=coeff(1) & fitfx(i)=coeff(0)
    print,'Press any key...'
    key1=get_kbrd(1)
    endfor

  coeff=polyfitw(fitwl,lines,lines*0+1,2,yfit)
  plot,fitwl,yfit-lines,psym=-4
  print,'stdev=',stdev(yfit-lines)

  wl2=coeff(0) + coeff(1)*xax + coeff(2)*xax^2

  return

  if (0 eq 1) then begin
    disspec,img,h,315,/skyspec,rtnspec=comp,yr=[0,200],/wcal
    wl=comp(*,0) & spec=comp(*,1)
    disfitline,wl,spec
    disfitcompb,wl,spec,wlft
    disfitline,wlft,spec
    endif

end



