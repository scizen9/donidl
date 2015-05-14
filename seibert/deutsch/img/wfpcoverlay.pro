pro wfpcoverlay,dssfile,wfpc2file,ps=ps,dsclabel=dsclabel,chiplabel=chiplabel,$
  fieldsize=fieldsize,stretch=stretch

  if (n_params(0) ne 2) then begin
    print,"Call> wfpcoverlay,DSSfilename,WFPC2filename,[ fieldsize=,/ps,/dsclabel,"
    print,"        /chiplabel,stretch=[nn,nn] ]"
    print,"e.g.> wfpcoverlay,'ngc6441.fits','u2hj0101t.c0h',fiel=5.0,/chip"
    return
    endif

  if (n_elements(ps) eq 0) then ps=0
  if (n_elements(dsclabel) eq 0) then dsclabel=0
  if (n_elements(chiplabel) eq 0) then chiplabel=0
  if (n_elements(fieldsize) eq 0) then fieldsize=3.0

  if (ps eq 0) then thk=1 else thk=4

  if (strmid(dssfile,strlen(dssfile)-5,5) eq '.fits') then $
    imgdss=readfits(dssfile,hdss) $
  else imgread,imgdss,hdss,dssfile

  imgread,img,h,wfpc2file,0
  xyad,h,0,0,a1,d1

  if (strn(sxpar(hdss,'PLATEID')) ne '0') then begin
    gsssextast,hdss,astr
    gsssadxy,astr,a1,d1,x,y
    gsss_stdast,hdss,x+[-200,0,200],y+[200,-200,50]
  endif else begin
    adxy,hdss,a1,d1,x,y
    endelse


  getrot,hdss,rot1,cdelt1
  print,'Rotation of DSS image after GSSS_STDAST: ',rot1
  print,'Plate scale is: ',abs(cdelt1(1))*3600


  if (n_elements(stretch) ne 2) then sky_value,imgdss,16,10,skyv,topv $
  else begin
    skyv=stretch(0) & topv=stretch(1)
    endelse


  if (n_elements(fieldsize) ne 0) then begin
    sz=size(imgdss)
    s1=fieldsize*60/(abs(cdelt1(1))*3600)
    hextract,imgdss,hdss,imgdss2,hdss2,x-s1/2,x+s1/2-1,y-s1/2,y+s1/2-1
    imgdss=imgdss2 & hdss=hdss2
    endif
  sz=size(imgdss)
  tp=!d.n_colors-1
  if (ps ne 0) then psout,bytscl(imgdss,skyv,topv),/inv,/dontclose, $
    filename='wfpcoverlay.ps' $
  else tv,congrid(bytscl(imgdss,skyv,topv,top=tp-1),512,512)
  plots,[0,1,1,0,0],[0,0,1,1,0],/norm,thick=thk

  xll=[35,43,23,39]+5.0
  yll=[50,16,41,40]+5.0

  for i=0,3 do begin

    imgread,img,h,wfpc2file,i

    xyad,h,xll(i),yll(i),a1,d1
    if (i eq 0) and (dsclabel ne 0) then xyouts,0.1,1.1, $
      'DSC Center of Field = '+adstring(a1,d1,3),/norm,charsize=1.5
    xyad,h,xll(i),799,a2,d2
    xyad,h,799,yll(i),a3,d3
    xyad,h,799,799,a4,d4

    adxy,hdss,a1,d1,x1,y1
    if (i eq 0) then  center=[x1,y1]
    adxy,hdss,a2,d2,x2,y2
    adxy,hdss,a3,d3,x3,y3
    adxy,hdss,a4,d4,x4,y4

    plots,[x1,x2]/sz(1),[y1,y2]/sz(2),/norm,thick=thk
    plots,[x2,x4]/sz(1),[y2,y4]/sz(2),/norm,thick=thk
    plots,[x3,x4]/sz(1),[y3,y4]/sz(2),/norm,thick=thk
    plots,[x3,x1]/sz(1),[y3,y1]/sz(2),/norm,thick=thk
    if (chiplabel ne 0) then xyouts,x4/sz(1),y4/sz(2),strn(i),/norm

    endfor

  if (dsclabel ne 0) then begin
    dsc_sz=288.768
    dss_sz=360.0
    dss_pix=360.0/212.0
    dsc_proj=dsc_sz/dss_pix
    
    noe=[center(0)-0.5*dsc_proj,center(1)+0.5*dsc_proj]
    now=[center(0)+0.5*dsc_proj,center(1)+0.5*dsc_proj]
    sow=[center(0)-0.5*dsc_proj,center(1)-0.5*dsc_proj]
    soe=[center(0)+0.5*dsc_proj,center(1)-0.5*dsc_proj]
    
    plots,[noe(0),now(0)]/sz(1),[noe(1),now(1)]/sz(2),/norm,thick=thk
    plots,[now(0),soe(0)]/sz(1),[now(1),soe(1)]/sz(2),/norm,thick=thk
    plots,[soe(0),sow(0)]/sz(1),[soe(1),sow(1)]/sz(2),/norm,thick=thk
    plots,[sow(0),noe(0)]/sz(1),[sow(1),noe(1)]/sz(2),/norm,thick=thk
    endif


  if (ps ne 0) then psclose

  return

end
