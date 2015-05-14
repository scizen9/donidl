pro qphot,image,x,y,qflag,correc=correc,aperval=aperval,test=test

  COMMON QphotComm,qdist,prev,ptr,flatsky

  if (n_elements(test) eq 0) then test=0

  if (n_elements(qdist) lt 50) or (x eq -999) then begin
    print,n_elements(qdist),'     Creating qdist....'
    xdist=fltarr(41,41)
    for i=0,40 do xdist(*,i)=findgen(41)
    ydist=rotate(xdist,1)
    qdist=sqrt(abs(xdist-20)^2+abs(ydist-20)^2)
    prev=fltarr(2,3000) & ptr=0
    sky_value,image,32,20,flatsky
    if (x eq -999) then return
    endif

  xx=fix(x+.5) & yy=fix(y+.5)
  BScentrd,image,fix(x+.5),fix(y+.5),xc,yc

  if (test eq 0) then begin
    tmp6=sqrt((xc-prev(0,*))^2.+(yc-prev(1,*))^2.)
    if (min(tmp6) le 3) then return
    endif

  if (qflag eq 2) then begin
    printf,8,format='(i5,2f8.2)',ptr,xc,yc
    prev(*,ptr)=[xc,yc] & ptr=ptr+1
    return
    endif


  xci=fix(xc+.5) & yci=fix(yc+.5)
  img=extrac(image,xx-20,yy-20,41,41)

  PCPsf=[0.151,0.189,0.227,0.253]
;  PCPsf=[0.151,0.189,0.227,0.253,0.276,0.302,0.324,0.344,0.357,0.370]
		; derived from Tiny Tim B PC Psf

  meas=fltarr(6-3+1)
  for i=3,6 do begin
    mask=where(qdist le i)
    meas(i-3)=total(img(mask))-flatsky*n_elements(mask)
    endfor

;  aper,image,xc,yc,flx,eflx,sky,esky,8,findgen(9)+3,[30,40],[-32767,+32768],/silent
;  aperval=mag2flux(flx,-25)/PCPsf

  correc=meas/PCPsf

  printf,8,format='(i5,2f8.2,f11.1,f9.1,a,a)',ptr,xc,yc,avg(correc), $
    stdev(correc),' ',vect(correc)

  prev(*,ptr)=[xc,yc] & ptr=ptr+1

  return

end







