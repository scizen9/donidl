pro disrcaladj,factor1

  if not exist('rcal.dat:preadj') then begin
    print,'Cannot find file "rcal.dat:preadj".  Maybe you need to do:'
    print,'%  cp -p rcal.dat rcal.dat:preadj'
    print,'Exiting...'
    return
    endif

  print,'Reading rcal.dat:preadj and multiplying by ',strn(factor1)
  openr,1,'rcal.dat:preadj'
  rcal=fltarr(2,1000) & i=0 & wl=0.0 & fl=0.0
  while not EOF(1) do begin
    readf,1,wl,fl
    rcal(*,i)=[wl,fl*factor1] & i=i+1
    endwhile
  close,1 & rcal=rcal(*,0:i-1)


  print,'Writing rcal.dat'
  openw,1,'rcal.dat'
  for i=0,(size(rcal))(2)-1 do printf,1,rcal(*,i)
  close,1

  return

end

