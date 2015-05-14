pro wfpcallchips,wfpc2file,ps=ps,stretch=stretch,trim=trim,dstarr=dstarr, $
  dontclose=dontclose

  if (n_params(0) ne 1) then begin
    print,"Call> wfpcallchips,wfpc2file,[/ps,/trim,stretch=[nn,nn]]"
    print,"e.g.> wfpcallchips,wfpc2file,stretch=[0,10]"
    return
    endif

  if (n_elements(ps) eq 0) then ps=0
  if (n_elements(trim) eq 0) then trim=0
  if (n_elements(dontclose) eq 0) then dontclose=0
  if (ps eq 0) then thk=1 else thk=4

  imgread,img,h,wfpc2file,0
  xyad,h,0,0,a1,d1

  getrot,h,rot1,cdelt1
  print,'Rotation of PC image after GSSS_STDAST: ',rot1
  print,'Plate scale is: ',abs(cdelt1(1))*3600

  if (n_elements(stretch) ne 2) then begin
    skyv=9999 & rmsv=9999
    for i=100,700,50 do begin
      skyline,img(100:700,i),s1,r1
      if (s1 lt skyv) then skyv=s1
      if (r1 lt rmsv) then rmsv=r1
      endfor
    topv=skyv+(rmsv*10)>20
  endif else begin
    skyv=stretch(0) & topv=stretch(1)
    endelse
  print,'stretch=[',skyv,topv,']'

  if (ps eq 1) then dstarr=bytarr(1600,1600) $
  else erase

  xll=[35,43,23,39]+5.0
  yll=[50,16,41,40]+5.0

  rots=[2,3,0,1] & xs=[.5,1,1,0] & ys=[.5,0,1,1]
  szs=[.5,1,1,1]
  for i=0,3 do begin
    imgread,img,h,wfpc2file,i
    if (trim eq 1) then begin
      img2=img*0
      img2(0:799-xll(i),0:799-yll(i))= $
        extrac(img,xll(i),yll(i),800-xll(i),800-yll(i))
      img=img2
      endif
    if (i eq 0) then img=img*4
    img=rotate(img,rots(i))
    if (ps eq 1) then begin
      dstarr(xs(i)*800:xs(i)*800+szs(i)*800-1, $
        ys(i)*800:ys(i)*800+szs(i)*800-1)= $
        bytscl(congrid(img,szs(i)*800,szs(i)*800),skyv,topv)
      endif $
    else begin
      tv,bytscl(congrid(img,szs(i)*256,szs(i)*256),skyv,topv, $
        top=!d.n_colors-2<254),xs(i)*256,ys(i)*256
      endelse
    if (i eq 2) then hsav=h
    endfor


  if (ps ne 0) then begin
    psout,dstarr,/dontclose,/inv,file='wfpcallchips.ps'
    !p.font=0
    arrows,hsav,.1,.1,thick=thk,/norm
    plots,[0,1,1,0,0],[0,0,1,1,0],/norm,thick=thk
    if (dontclose eq 0) then psclose
  endif else begin
    !p.font=-1
    arrows,hsav,70,70,/notvert,thick=thk
    endelse

  return

end
