pro disread,img,h,filenamein,image=image,spectrum=noproc,spflat=spflat, $
  newflats=newflats,Bflatfname=Bflatfname,Rflatfname=Rflatfname

;+
; No formal header yet.  See the documentation in
;   http//www.astro.washington.edu/deutsch/apoinfo.html
;-

  if (n_params(0) lt 3) then begin
    print,"Call> disread,image,header,filename,[/image,/spec]"
    print,"e.g.> disread,img,h,'a0001.fit'"
    print,"e.g.> disread,img,h,'im.0001b'"
    print,"  image     returned image array"
    print,"  hdr       returned header array"
    print,"  filename  string of filename (either .fit or GEIS file)"
    print,"  /image    if set, the file will be processed as an image"
    print,"              (i.e. trimmed, biased, flattened, and oriented)"
    print,"  /spec     if set, the file will be processed as a spectrum"
    print,"              (i.e. biased, and oriented)"
    return
    endif

  if (n_elements(image) eq 0) then image=0
  if (n_elements(noproc) eq 0) then noproc=0
  if (n_elements(spflat) eq 0) then spflat=0
  if (n_elements(newflats) eq 0) then newflats=0
  !quiet=1

;-----------------------------------------------------------------------------
  COMMON DIS_COMM,prevfile,Rflat,Bflat,Rspflat,Bspflat

  if (n_elements(Bflatfname) eq 0) then $
    Bflatfname='/host/dione/d3/deutsch/apo/jan07/Flat_final_S'
  if (n_elements(Rflatfname) eq 0) then $
    Rflatfname='/host/dione/d3/deutsch/apo/jan07/Flat_final_L'
  Bspflatfname='/host/dione/d3/deutsch/apo/dec28/flats/Flat_Bsp2'
  Rspflatfname='/host/dione/d3/deutsch/apo/dec28/flats/Flat_Rsp2'
  if (newflats eq 1) then begin
    Rflat=0 & Bflat=0 & Rspflat=0 & Bspflat=0
    endif

;-----------------------------------------------------------------------------

  ffmt='GEIS' & filename=filenamein
  ext=strmid(filename,strlen(filename)-3>0,99)
  if (ext eq 'fit') or (ext eq 'its') then ffmt='FITS'
  if (ffmt eq 'GEIS') and (ext ne 'hhh') then begin
    if (exist(filename+'.fit')) then begin
      ffmt='FITS'
      filename=filename+'.fit'
      endif
    if (exist(filename+'.fits')) then begin
      ffmt='FITS'
      filename=filename+'.fits'
      endif
    endif
  if (ffmt eq 'GEIS') and (ext ne 'hhh') then filename=filename+'.hhh'

  if not exist(filename) then begin
    print,'File "',filename,'" not found.'
    return
    endif

  if (ffmt eq 'GEIS') then imgread,img1,h,filename  
  if (ffmt eq 'FITS') then begin
    print,'Reading file "',filename,'"'
    img1=readfits(filename,h)
    endif

  camera=(['BLUE','RED'])(sxpar(H,'CCDNUM')-1)
  if (camera eq 'BLUE') then begin
    xmin=121 & xmax=494 & ymin=116 & ymax=386
    xsize=xmax-xmin+1 & ysize=ymax-ymin+1
    s=size(img1) & if (s(1)+s(2) ne 1108) then begin
      print,'Unexpected image size!!  Exiting...' & img=img1 & return
      endif
    if (n_elements(Bflat) lt 1000) then begin
      imgread,Bflat,Bfh,Bflatfname
      img2=Bflat(xmin:xmax,ymin:ymax)
      Bflat=img2/median(img2)
      if exist('Flat_Bsp.hhh') then imgread,Bspflat,Bspfh,'Flat_Bsp.hhh' $
      else imgread,Bspflat,Bspfh,Bspflatfname
      endif
    endif
  if (camera eq 'RED') then begin
    xmin=106 & xmax=786 & ymin=195 & ymax=677
    xsize=xmax-xmin+1 & ysize=ymax-ymin+1
    s=size(img1) & if (s(1)+s(2) ne 1690) then begin
      print,'Unexpected image size!!  Exiting...' & img=img1 & return
      endif
    if (n_elements(Rflat) lt 1000) then begin
      imgread,Rflat,Rfh,Rflatfname
      img2=Rflat(xmin:xmax,ymin:ymax)
      Rflat=img2/median(img2)
      if exist('Flat_Rsp.hhh') then imgread,Rspflat,Rspfh,'Flat_Rsp.hhh' $
      else imgread,Rspflat,Rspfh,Rspflatfname
      endif
    endif


  if ((strn(sxpar(h,'TURRET')) eq 'MIRRORS') or (image eq 1)) and $
    (noproc eq 0) then begin

    biaslevel=avg(avg(img1(28:45,ymin:ymax),0))
    img2=img1(xmin:xmax,ymin:ymax)*1.0
    tmp1=where(img2 lt 0)
    if (tmp1(0) ne -1 ) then img2(tmp1)=65536.0+img2(tmp1)
    if (camera eq 'RED') then img=((img2-biaslevel)/Rflat)
    if (camera eq 'BLUE') then img=((img2-biaslevel)/Bflat)
    tmp1=where((img2 eq 65535.0))
    if (tmp1(0) ne -1 ) then img(tmp1)=65535.0
    img=img(xsize-1-indgen(xsize),*)
    img=rotate(img,2)

    medn=median(img)+200 & scmin=(medn-(sqrt(medn)*2+100))>0 - 200
    scmax=medn+(medn/7.0+200) - 200
    print,'Bias level subtracted: ',strn(biaslevel)
    print,'Sky level: ',strn(medn-200),'    Stretch: ',vect([scmin,scmax])
    check_FITS,img,h,/UPDATE
    sxaddpar,h,'IR_SCMIN',scmin,' IMGRoam Frame Scaling Minimum'
    sxaddpar,h,'IR_SCMAX',scmax,' IMGRoam Frame Scaling Maximum'
    sxaddpar,h,'IR_RDTYP','NONE',' IMGRoam Frame Reduction Type'
    sxaddpar,h,'IR_SATLM',65535.0,' IMGRoam Saturation Limit Value'
    sxaddpar,h,'BITPIX',32,' IEEE single precision floating point'

    print,'Image read, trimmed, bias-subtracted, flat-fielded, and reoriented.'

    if (camera eq 'RED') then begin
      pixsize=0.6098
      CRPIX=[328,244]+1
    endif else begin
      pixsize=1.088
      CRPIX=[173,136]+1
      endelse

    ra=0.0 & dec=0.0
    if (strn(sxpar(h,'RA')) ne '0') then begin
      coords=strn(sxpar(h,'RA'))+' '+strn(sxpar(h,'DEC'))
      if (strpos(coords,'0.0000') ne -1) then stringad,coords,ra,dec $
        else begin
        ra=0d & dec=0d
        endelse
      endif
    sxaddpar,h,'CRVAL1',ra,' RA from RA keyword'
    sxaddpar,h,'CRVAL2',dec,' Dec from DEC keyword'
    sxaddpar,h,'CRPIX1',CRPIX(0),' X target pixel'
    sxaddpar,h,'CRPIX2',CRPIX(1),' Y target pixel'
    sxaddpar,h,'CTYPE1','RA---TAN',' Tangent Astrometry'
    sxaddpar,h,'CTYPE2','DEC--TAN',' Tangent Astrometry'
    sxdelpar,h,'CDELT1'
    sxdelpar,h,'CDELT2'
    ang=sxpar(h,'INSTANG')/!radeg
    CD=[-cos(ang),sin(ang),sin(ang),cos(ang)]*pixsize/3600
    sxaddpar,h,'CD1_1',CD(0),' dRA/dX'
    sxaddpar,h,'CD1_2',CD(1),' dRA/dY'
    sxaddpar,h,'CD2_1',CD(2),' dDEC/dX'
    sxaddpar,h,'CD2_2',CD(3),' dDEC/dY'
    endif

  if (n_elements(biaslevel) eq 0) then begin
    biaslevel=avg(avg(img1(28:45,ymin:ymax),0))
    img2=img1*1.0
    tmp1=where(img2 lt 0)
    if (tmp1(0) ne -1 ) then img2(tmp1)=65536.0+img2(tmp1)
    s=size(img1)
    img=img2(s(1)-1-indgen(s(1)),*)-biaslevel

    if (spflat eq 1) then begin
      if (camera eq 'RED') then img=img/Rspflat
      if (camera eq 'BLUE') then img=img/Bspflat
      print,'Applying Spectrum flat...'
      endif

    img=rotate(img,2)
    medn=median(img(xmin:xmax,ymin:ymax))+200
    scmin=(medn-(sqrt(medn)*2+100))>0 - 200
    scmax=medn+(medn/7.0+200) - 200
    print,'Bias level subtracted: ',strn(biaslevel)
    print,'Sky level: ',strn(medn-200),'    Stretch: ',vect([scmin,scmax])
    check_FITS,img,h,/UPDATE
    sxaddpar,h,'IR_SCMIN',scmin,' IMGRoam Frame Scaling Minimum'
    sxaddpar,h,'IR_SCMAX',scmax,' IMGRoam Frame Scaling Maximum'
    sxaddpar,h,'IR_RDTYP','NONE',' IMGRoam Frame Reduction Type'
    sxaddpar,h,'IR_SATLM',65535.0,' IMGRoam Saturation Limit Value'

    disfindspec,img,h

    endif

  return

end




