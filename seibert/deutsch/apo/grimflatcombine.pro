pro grimflatcombine,searchspec,outputfile,dark=dark,roughflat=roughflat, $
  starmaskfile=starmaskfile,shiftfile=shiftfile
;+
; NAME:
;	GRIMFLATCOMBINE
;
; PURPOSE:
;	Combine a set of image frames to make a flat.
;
; CATEGORY:
;	APO software
;
; CALLING SEQUENCE:
;	grimflatcombine,searchspec
;	grimlist,searchspec [,outputfilename]
;
; INPUTS:
;	searchspec: A string containing the search specification for the
;		images to be combined.  This is typically '*.hhh'
;
; OPTIONAL INPUTS:
;	outputfilename: A string containing the name of an output file
;		which will contain the final flat.  If this
;		paramenter is not supplied, the name will be 'Flat'
;
; OPTIONAL INPUT KEYWORDS:
;	None.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	A list of images is assembled, and then the images are median combined.
;
; EXAMPLE:
;	grimflatcombine,'flat10/*.hhh'
;
; MODIFICATION HISTORY:
;	1998 Written by E. Deutsch
;
;-

  if (n_params(0) lt 1) then begin
    print,"Call> grimflatcombine,searchspec"
    print,"Call> grimlist,searchspec [,outputfilename]"
    print,"e.g.> grimflatcombine,'flats/*.hhh'"
    return
    endif


  files=findfile(searchspec)
  nfiles=n_elements(files)
  if (nfiles eq 1) and (files(0) eq '') then begin
    print,'Unable to find any files with search spec: ',searchspec
    return
    endif


  if (n_elements(outputfile) eq 0) then outputfile='Flat'
  if (n_elements(roughflat) gt 100) then roughflat=roughflat/median(roughflat)


  window,xs=512
  tp=(!d.n_colors<256)-1
  cube=fltarr(256,256,nfiles)


  if (n_elements(starmaskfile) eq 1) then begin
    print,'Creating star mask....'
    openr,1,starmaskfile
    tmp1=fltarr(3) & starmaskdata=fltarr(3,500) & i=0
    while (not EOF(1)) do begin
      readf,1,tmp1
      starmaskdata(*,i)=tmp1
      i=i+1
      endwhile
    close,1
    nstars=i
    starmaskdata=starmaskdata(*,0:nstars-1)
    starmask=intarr(512,512)+1
    for i=0,nstars-1 do begin
      dist_circle,mask,512,starmaskdata(0,i)+256,starmaskdata(1,i)+256
      good=where(mask le starmaskdata(2,i))
      starmask(good)=0
      endfor
    tv,starmask*tp
    endif


  if (n_elements(shiftfile) eq 1) then begin
    openr,1,shiftfile
    tmp1=fltarr(5) & shiftdata=fltarr(5,500) & i=0
    while (not EOF(1)) do begin
      readf,1,tmp1
      tmp1(1)=255-tmp1(1)			; fudge
      shiftdata(*,i)=tmp1
      i=i+1
      endwhile
    close,1
    nframes=i
    shiftdata=shiftdata(*,0:nframes-1)
    endif


  xpos=0 & ypos=0
  for i=0,nfiles-1 do begin
    grimread,img,h,files(i),/noproc,/norot
    img=img-dark

    if (n_elements(roughflat) gt 100) then begin
      img=img/roughflat
      skyv=median(img)
      corr=fltarr(2,256)
      for y=0,1 do begin
        for x=0,1 do begin
          tmp1=fltarr(128)
          for j=0,127 do tmp1(j)=median(img(128*x:128*x+127,j+128*y))
          tmp2=splfit(findgen(128),tmp1,5)
          corr(x,y*128:y*128+127)=tmp2
          endfor
        endfor
      corr=corr-skyv
      for j=0,255 do img(j,*)=img(j,*)-corr(j/128,*)
      endif

    skyline,img(140:200,50:150),s1,r1
    m1=median(img) & print,s1,r1,m1
    if (i eq 0) then refsky=m1 else img=img/(m1/refsky)
    skyline,img(140:200,50:150),s1,r1
    m1=median(img) & print,s1,r1,m1,refsky

    if (n_elements(starmaskfile) eq 1) then begin
      print,files(i),shiftdata(*,i)
      xsh=shiftdata(1,i)-255.5 & ysh=shiftdata(2,i)-255.5
      msk=interpolate(starmask,findgen(512)-xsh,findgen(512)-ysh,/grid,miss=0)
      msk(where(msk ne 0))=1
      msk=msk(0:255,0:255)
;      tv,msk*255
      img=img*msk
      endif

    cube(*,*,i)=img
    tv,bytscl(img,s1-r1*5,s1+r1*15,top=tp-1),xpos*256,ypos*256

    xpos=xpos+1
    if (xpos eq 2) then begin
      ypos=ypos+1 & xpos=0
      if (ypos eq 2) then ypos=0
      endif
    endfor


  if (1 eq 1) then begin
    flat=img*0
    flatrms=img*0
    for y=0,255 do begin
      for x=0,255 do begin
        pix=cube(x,y,*)
        good=where(pix ne 0)
        if (good(0) ne -1) then pix=pix(good)
        flat(x,y)=median(pix)
        flatrms(x,y)=stdev(pix)
        endfor
      endfor
    endif

  skyline,flat(157:181,51:74),s1,r1 & print,s1,r1
  tv,congrid(bytscl(flat,s1-r1*5,s1+r1*15,top=tp-1),512,512)

  stwrt,flat,h,outputfile,/sdas
  stwrt,flatrms,h,outputfile+'_stdev',/sdas

  wait,4

  useflat=flat
  if (n_elements(roughflat) gt 100) then useflat=useflat*roughflat
  useflat=useflat/median(useflat)

  xpos=0 & ypos=0
  for i=0,nfiles-1 do begin
    grimread,img,h,files(i),/norot,/noproc
    img=(img-dark)/useflat

    skyv=median(img)
    corr=fltarr(2,256)
    for y=0,1 do begin
      for x=0,1 do begin
        tmp1=fltarr(128)
        for j=0,127 do tmp1(j)=median(img(128*x:128*x+127,j+128*y))
        tmp2=splfit(findgen(128),tmp1,5)
        corr(x,y*128:y*128+127)=tmp2
        endfor
      endfor
    corr=corr-skyv
    for j=0,255 do img(j,*)=img(j,*)-corr(j/128,*)

    skyline,img(157:181,51:74),s1,r1 & print,s1,r1
    tv,bytscl(img,s1-r1*4,s1+r1*15,top=tp-1),xpos*256,ypos*256
    xyouts,xpos*256+10,ypos*256+10,strn(files(i)),/device
    wait,0.5
    xpos=xpos+1
    if (xpos eq 2) then begin
      ypos=ypos+1 & xpos=0
      if (ypos eq 2) then ypos=0
      endif
    endfor

  return

end



