pro slitread,img,h,filename,disp=disp,fitslit=fitslit,calcoffset=calcoffset, $
  rotate=rot1,instang=instang,faint=faint

  if (n_elements(disp) eq 0) then disp=0
  if (n_elements(rot1) eq 0) then rot1=1
  if (n_elements(fitslit) eq 0) then fitslit=1
  if (n_elements(calcoffset) eq 0) then calcoffset=0
  if (n_elements(instang) eq 0) then instang=0
  if (n_elements(faint) eq 0) then faint=0

  if (calcoffset eq 1) then disp=1


  if (n_params(0) lt 3) then begin
    print,'Call> slitread,image,header,filename,[/disp,fitslit=0,/calcoffset,rotate=0,'
    print,'        instang=]'
    print,"e.g.> slitread,img,h,'slit.001',/disp"
    return
    endif
 
  if not exist(filename) then begin
    print,'File "',filename,'" not found.'
    return
    endif
 
  img=0 & ffmt='FITS'
  if (ffmt eq 'GEIS') then imgread,img,h,filename  
  if (ffmt eq 'FITS') then begin
    print,'Reading file "',filename,'"'
    img=readfits(filename,h,/silent)
    endif

  sign1=1
  if (rot1 eq 1) then begin
    img=rotate(img,2) & sign1=-1
    endif


  sz=size(img)
  if (disp eq 1) then begin
    skyline,img(*,0:60),skyv1,rmsv1
    print,'Sky,rms=',skyv1,rmsv1
    mn1=skyv1-rmsv1*9 & mx1=rmsv1*80
    if (faint ne 0) then begin
      mn1=skyv1 & mx1=skyv1+rmsv1*3
      endif
    tp1=(!d.n_colors<256)-1
    tv,congrid(bytscl(img,mn1,mx1,top=tp1-1),sz(1)*2,sz(2)*2)
    dispsz=[sz(1)*2.0,sz(2)*2.0]
    if (instang ne 0) then begin
      img3=fltarr(300,300) & imgput,img3,img,(300-sz(1))/2,(300-sz(2))/2
      img2=rot(img3,-instang,/interp,missing=0)
      tv,congrid(bytscl(img2,mn1,mx1,top=tp1-1),512,512)
      dispsz=[512.0,512.0]
      endif
    endif


  if (fitslit gt 0) then begin
    med=fltarr(sz(2)) & x=indgen(sz(2))
    for i=0,sz(2)-1 do med(i)=median(img(*,i))
    med2=median(med,5)
    pos1=(where(med2 eq min(med2)))(0)
    strip=med(pos1-15:pos1+14)
    x=x(pos1-15:pos1+14)
    fit=gaussfit(x,strip,coeff)
    slitY=coeff(1)
    print,'slit Y position (whole image)=',slitY
    print,'slit Y FWHM (arcsec)=',coeff(2)*2.35*0.34
    if (fitslit gt 1) then begin
      plot,x,strip,xsty=1
      oplot,x,fit,color=tp1/2
      endif
    endif


  if (calcoffset eq 1) then begin
    if (instang ne 0) then begin
      print,'Cannot calculate an offset unless instang keyword is 0'
      return
      endif
    print,'Click on star to be put in the slit'
    cursor,x,y,/device
    x=x/dispsz(0)*sz(1) & y=y/dispsz(1)*sz(2)
    bscentrd,img,x,y,xc,yc
    print,'star X,Y=',xc,yc
    if (fitslit eq 0) then begin
;      print,'WARNING: slit was not fit for.  Assuming slit position of 103.8'
      print,'WARNING: slit was not fit for.  Assuming slit position of 164.8'
      slitY=164.8
      endif
    print,''
    targX=140.0
    print,'Need Remark X,Y inst offset:',(targX-xc)*sign1*0.29*(-1), $
      (slitY-yc)*sign1*0.34,' arcsec', $
      format='(a,2f8.2,a)'
    endif



  return

end
