
  CLIB='/host/bluemoon/usr2/idllib/deutsch/clib/imcombine.so'

  imgcube=randomu(seed,10,3,5)*10
  for z=0,4 do begin & print,imgcube(*,*,z),format='(10f7.2)' & print,'' & end

  sz=size(imgcube)
  fimg=fltarr(sz(1),sz(2)) & timg=fimg
;  result=call_external(CLIB,'imcombine',imgcube,fimg,sz,1L)
  result=call_external(CLIB,'imcombine',imgcube,fimg,sz,[2L,1,1])

  for i=0,sz(3)-1 do timg=timg+imgcube(*,*,i)
  timg=timg/sz(3)

  print,'-----'
  print,fimg,format='(10f7.2)'
  print,''
  print,timg,format='(10f7.2)'

  tmp1=imgcube(0,0,*)
  tmp2=tmp1(sort(tmp1))
  print,avg(tmp2(1:3))

end
