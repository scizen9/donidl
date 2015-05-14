

pro skycalc,image,skyv,rms,plotf

  if (n_params(0) lt 4) then plotf=0
  s=size(image)
  if (s(0) ne 2) then begin
    print,'Must be 2D array'
    return
    endif

  AX1=s(1) & AX2=s(2)
  svals=fltarr(100) & srms=svals

  for i=0,99 do begin
    line=image(AX1*.1:AX1-AX1*.1,i*.01*(AX2*.9)+AX2*.05)
;    print,vect([AX1*.1,AX1-AX1*.1,i*.01*(AX2*.9)+AX2*.05])
    ltmp=where(line ne 0)
    chk=size(ltmp)
    if (chk(0) ne 0) then begin
      line=line(ltmp)
      skyline,line,val,rm
      svals(i)=val & srms(i)=rm
    endif else begin
      svals(i)=-1 & srms(i)=-1
      endelse
    endfor

  svals=svals(where(svals ne -1))
  srms=srms(where(srms ne -1))
  if (plotf eq 1) then plot,indgen(100),svals
    
  skyline,svals,skyv,rm
  skyline,srms,rms,rm

  return
end
