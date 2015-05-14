pro disfitcompr,wl,spec,wl2

  lines=[5944.8342,6029.9971,6074.3377,6096.163,6143.0623,6163.5939, $
    6217.2913,6266.495,6304.7892,6334.4279,6382.99,6402.246,6506.5279, $
    6532.8824,6598.9529]
;    6532.8824,6598.9529,6717.0428,6929.467]
  fitwl=lines*0 &  fitfx=fitwl
  halfwidth=10

  nel=n_elements(wl)
  xax=findgen(nel)
  for i=0,n_elements(lines)-1 do begin
    wlt=lines(i)
    tmp1=abs(wlt-wl)
    xpos=where(tmp1 eq min(tmp1)) & xpos=xpos(0)
    w1=xax(xpos-halfwidth:(xpos+halfwidth)<(nel-1))
    s1=spec(xpos-halfwidth:(xpos+halfwidth)<(nel-1))
    plot,w1,s1,xsty=1,psym=10,ysty=16
    fit=gaussfit(w1,s1,coeff)
    oplot,w1,fit,psym=-4,color=!d.n_colors-2
    fitwl(i)=coeff(1) & fitfx(i)=coeff(0)
    print,lines(i),fitwl(i)
    print,'Press any key...'
    key1=get_kbrd(1)
    endfor

  coeff=polyfitw(fitwl,lines,lines*0+1,2,yfit)
  plot,fitwl,yfit-lines,psym=-4
  print,'stdev=',stdev(yfit-lines)

  wl2=coeff(0) + coeff(1)*xax + coeff(2)*xax^2

  return

  if (0 eq 1) then begin
    disspec,img,h,409,/skyspec,rtnspec=comp,yr=[0,5000],/wcal
    wl=comp(*,0) & spec=comp(*,1)
    disfitline,wl,spec
    disfitcompr,wl,spec,wlft
    disfitline,wlft,spec
    endif

end



