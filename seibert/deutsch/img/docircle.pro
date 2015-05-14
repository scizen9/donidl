pro docircle,objfile,objtype,xrange=xr,yrange=yr,zoom=zoom

  if (n_params(0) lt 1) then begin
    print,'Call> docircle,objout_file,object_type,[xrange=,yrange=,zoom=]'
    print,"e.g.> docircle,'example.objout',1,zoom=2"
    return
    endif

  if (n_elements(objtype) eq 0) then objtype=1
  if (n_elements(zoom) eq 0) then zoom=1

; Read file

  MAXSTARS=5000
  data=fltarr(6,MAXSTARS)		; [Index, X, Y, Mag, Err, Type]

  openr,1,objfile
  print,'Reading: ',objfile
  datin=fltarr(15) & i=0
  while not EOF(1) do begin
    readf,1,datin
    if (datin(1) eq objtype) then begin
      data(*,i)=datin([0,2,3,4,5,1]) & i=i+1 & endif
    endwhile
  close,1
  data=data(*,0:i-1)

  print,strn(i),' objects read in'

  if (n_elements(xr) ne 2) then $
    for j=0,i-1 do tvcircle,5,data(1,j)*zoom,data(2,j)*zoom,/device $
  else begin
    for j=0,i-1 do begin
      x=data(1,j) & y=data(2,j)
      if (x ge xr(0))*(x le xr(1))*(y ge yr(0))*(y le yr(1)) $
        then tvcircle,5,(x-xr(0))*zoom,(y-yr(0))*zoom,/device
      endfor
    endelse

  return

end
