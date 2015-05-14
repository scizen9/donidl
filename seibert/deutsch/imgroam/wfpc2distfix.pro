pro wfpc2distfix,xin,yin,xout,yout,chip

  if (n_params(0) ne 5) then begin
    print,'Call> wfpc2distfix,xin,yin,xout,yout,chip'
    return
    endif

  if (chip ne 0) then begin
    print,'Can only handle the PC (chip=0) so far'
    return
    endif


  C=[0,  3.54356d2,   1.00021d0,   9.79758d-4,   9.84222d-8, -6.31327d-9, $
        -7.19983d-7, -3.73922d-8,  6.65101d-10, -3.51470d-8, -2.55003d-9]
  D=[0,  3.43646d2,   1.00544d-3,  9.99786d-1,  -5.80870d-7, -5.07221d-7, $
         3.41574d-7, -1.08091d-9, -3.42070d-8,   1.17819d-9, -4.48966d-8]

  xx1=xin-400d & yy1=yin-400d
  xout=C(2)*xx1+C(3)*yy1+C(4)*xx1^2+C(5)*xx1*yy1+C(6)*yy1^2+ $
     C(7)*xx1^3+C(8)*xx1^2*yy1+C(9)*xx1*yy1^2+C(10)*yy1^3
  yout=D(2)*xx1+D(3)*yy1+D(4)*xx1^2+D(5)*xx1*yy1+D(6)*yy1^2+ $
     D(7)*xx1^3+D(8)*xx1^2*yy1+D(9)*xx1*yy1^2+D(10)*yy1^3

  yout=yout+400
  xout=xout+400

  return


  plot,indgen(10),/nodata,xr=[0,800],yr=[0,800]
  fac=10.0
  for y=25,775,25 do begin
    for x=25,775,25 do begin
      xx1=x-400d & yy1=y-400d
      x2=C(2)*xx1+C(3)*yy1+C(4)*xx1^2+C(5)*xx1*yy1+C(6)*yy1^2+ $
         C(7)*xx1^3+C(8)*xx1^2*yy1+C(9)*xx1*yy1^2+C(10)*yy1^3
      y2=D(2)*xx1+D(3)*yy1+D(4)*xx1^2+D(5)*xx1*yy1+D(6)*yy1^2+ $
        D(7)*xx1^3+D(8)*xx1^2*yy1+D(9)*xx1*yy1^2+D(10)*yy1^3
      arrow,x,y,(x2-xx1)*fac+x,(y2-yy1)*fac+y,/data
      endfor
    endfor


end




