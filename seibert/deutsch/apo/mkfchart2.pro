pro mkfchart2,coords,dssfile=dssfile,gscfile=gscfile,ps=ps, $
  objname=objname,fieldsize=fieldsize,a10file=a10file, $
  polygon=polygon1,interactive=inter1,spifields=spifields, $
  stretch=stretch,inst=inst,usnolabel=usnolabel,offset=offset,orient=orient
;+
; NAME:
;	MKFCHART
;
; PURPOSE:
;	Generate a simple finding chart, given an input of the Digitized
;	Sky Survey (DSS) or the Guide Star Catalog (GSC).
;
; CATEGORY:
;	APO software
;
; CALLING SEQUENCE:
;	mkfchart,coords [,dssfile=,gscfile=,/ps,objname=,fieldsize=]
;
; INPUTS:
;	coords: A string containing the J2000 coordinates of the target
;		object in sexigesimal format.
;
; OPTIONAL INPUT KEYWORDS:
;	dssfile: The filename of a Digitized Sky Survey (DSS) FITS image which
;		contains the field and the surrounding region.
;
;	gscfile: The filename of an HST Guide Star Catalog (GSC) extraction
;		which contains the target field and the surrounding region.
;		Note that since the limiting magnitude of the GSC is variable
;		around 14th magnitude, the GSC is almost necessary.
;
;	a10file: The filename of an USNO-A1.0 extraction
;		which contains the target field and the surrounding region.
;
;	ps:	If this keyword is set, the the output is directed to the
;		PostScript file 'gdracq.ps' instead of the screen.  Alternately
;		this keyword can be set to a string which is the filename.
;
;	objname: A string which will be printed out as an object name at the
;		top of a PostScript map.
;
;	fieldsize: This keyword can specify a fieldsize other than some
;		default value (DSS image size or 10 arcmin).
;
;	polygon: Overlay a polygon with verticies [ra0,dec0,ra1,dec1,ra2,dec2,...]
;		The polygon is not close automatically.
;
;	interactive: Interactively draw SPIcam fields on the image.
;
;	spifields: Overplot the SPIcam fields.
;
; OUTPUTS:
;	Screen image or postscript file.
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
;	Load DSS image if provided, rotate and display an appropriate region.
;	Load GSC list if provided, display star symbols.  Finally, overlay
;	a cross at the coordinates and labels.
;
; EXAMPLE:
;	window,colors=50,xs=512
;	whitesky,res='RED'
;	coords='19 15 11.52  +10 56 45.1'
;	apoguide,coords,dssfile='test08zr.fits',gscfile='test.gsclst'
;	 (see separate instruction sheet for complete details)
;
; MODIFICATION HISTORY:
;	11/16/96 Written by E. Deutsch based on APOGUIDE
;
;-


; -- This is a site-specific location of a necessary dummy header file ---
  dsshdrfile='$IDL_MAIN/deutsch/apo/apoguide.hhh'


; -- Not enough parameters?  Show the call sequence -------------------
  if (n_params(0) lt 1) then begin
    print,'Call> mkfchart,coords, [dssfile=],[gscfile=],[a10file=],[/ps],[objname=],[fieldsize=]'
    print,"e.g.> mkfchart,'20 33 26.064  +60 39 55.26',dssfile='n69306z0.fits',object='NGC 6930',/ps"
    return
    endif


; -- Do we want PostScript output?  To what filename? ------------
  psfile='fchart.ps'
  if (n_elements(ps) eq 0) then ps=0 $
  else begin
    sz=size(ps)
    if (sz(1) eq 7) then begin
      psfile=ps(0) & ps=1
      endif
    endelse


; -- Set default values to unspecified keywords -----------------
  if (n_elements(dssfile) eq 0) then dssfile=''
  if (n_elements(gscfile) eq 0) then gscfile=''
  if (n_elements(objname) eq 0) then objname=''
  if (n_elements(a10file) eq 0) then a10file=''
  if (n_elements(polygon1) eq 0) then polygon1=-1
  if (n_elements(inter1) eq 0) then inter1=-1
  if (n_elements(spifields) eq 0) then spifields=''
  if (n_elements(inst) eq 0) then inst='SPICAM'
  if (n_elements(usnolabel) eq 0) then usnolabel=0
  if (n_elements(offset) eq 0) then offset=[0,0]


; -- Convert and check the specified coordinates ---------------
  if (n_elements(coords) eq 0) then begin
    print,'ERROR: Coordinate variable undefined!'
    return
    endif
  stringad,coords,ra,dec
  if (ra eq 0.0) and (dec eq 0.0) then begin
    print,'ERROR: RA and DEC are equal to 0.0.  Probably an error occurred"
    return
    endif
  raorig=ra & decorig=dec


; -- Set up fixed variables ------------------------------------
  pltscl=1.70


; -- Read DSS image if desired -----------------------
  if (dssfile ne '') then begin
    if (not exist(dssfile)) then begin
      print,'ERROR: '+dssfile+' not found.'
      dssfile=''
      endif
    endif

  if (dssfile ne '') then begin
    print,'Loading image '+dssfile
    img=readfits(dssfile,h)
    if (n_elements(img) lt 100) then begin
      print,'DSS image read failed.  No image will be used.'
      dssfile=''
      goto,DSSBAIL
      endif


    dsssize=size(img) & sz=min(dsssize(1:2))
    gsssextast,h,gsa
    gsssadxy,gsa,ra,dec,x,y
    gsss_stdast,h,[(x-sz/3)>0,x,(x-sz/3)<(sz-1)], $
      [(y-sz/3)>0,(y+sz/2)<(sz-1),y]
    getrot,h,rot1,cdelt1
    pltscl=avg(abs(cdelt1))*3600
    print,'Plate Scale=',strn(pltscl)

    if (n_elements(fieldsize) eq 0) then fieldsize=(sz-10)*pltscl/60
    range=round(fieldsize/2.0*60/pltscl)/2*2+1
    print,'DSS image size: ',vect(dsssize(1:2)),' pixels;  ',$
      vect(dsssize(1:2)*1.7/60,format='(f20.2)'),' arcmin'


    print,'Rotating image by ',strn(rot1),' degrees....'
    hrot,img,h,img3,h3,rot1,x,y,0,missing=0
    adxy,h3,ra,dec,x,y

    xc=round(x) & yc=round(y)
    img2=extrac(img3,xc-range,yc-range,range*2+1,range*2+1)
    imsize=size(img2) & print,imsize
    fac=pltscl*imsize(1)
    print,'fac=',fac
    skyv=30000.0 & rmsv=skyv
    for ij=1,10 do begin
      skyline,img2(imsize(4)/13*ij:imsize(4)/13*ij+imsize(1)*3),v1,v2
      skyv=skyv<v1 & rmsv=rmsv<v2
      endfor
    print,'SkyV,rmsV=',skyv,rmsv
    stretchtype=0
    if (n_elements(stretch) ge 2) then begin
      skyv=stretch(0) & rmsv=stretch(1)
      print,'SkyV,rmsV=',skyv,rmsv
      if (n_elements(stretch) ge 3) then stretchtype=stretch(2)
      endif
    endif

DSSBAIL:
  if (dssfile eq '') then begin
    if (n_elements(fieldsize) eq 0) then fieldsize=10.0
    range=round(fieldsize/2.0*60/pltscl)/2*2+1
    fac=pltscl*(range*2+1)
    img2=fltarr(16,16)
    skyv=100.0 & rmsv=10.0
    sxhread,dsshdrfile,h & h3=h
    endif


; -- Display the image ----------------------------------------
  if (ps eq 1) then begin
    if (stretchtype eq 0) then $
      psout,/inv,imscl(img2-skyv-rmsv*2,0,15000,1500),7.5,7.5,0.5,1.5,/dontclose,filename=psfile
    if (stretchtype eq 1) then $
      psout,/inv,bytscl(img2,skyv,rmsv),7.5,7.5,0.5,1.5,/dontclose,filename=psfile
    thk=2
  endif else begin
    if (stretchtype eq 0) then $
      tv,imscl(congrid(img2,!d.x_size,!d.y_size)-skyv-rmsv*2,0,15000,1500,top=(!d.n_colors<256)-2)
    if (stretchtype eq 1) then $
      tv,bytscl(congrid(img2,!d.x_size,!d.y_size),skyv,rmsv,top=(!d.n_colors<256)-2)
    thk=1
    endelse


; -- Draw central cross ---------------------------------------------
  plots,/norm,[0,0,0,1,-1]*.03+0.5,[1,-1,0,0,0]*.03+0.5


; -- Draw NE Arrows -------------------------------------------------
  if (!d.name eq 'PS') then begin
    !p.font=0
    arrows,h3,/norm,.97,1.07,arrowlen=2,charsize=1,color=1
  endif else begin
    !p.font=-1
    arrows,h3,/norm,.97,.03,arrowlen=2,charsize=1
    endelse


; -- Write Text Labels ---------------------------------------
  xyouts,.05,1.20,/norm,'Object: '+objname
  xyouts,.06,1.17,/norm,'Coordinates: '+coords+' (J2000)'
  xyouts,.06,1.14,/norm,'Field Size: '+strn(fac/60,format='(f10.2)')+ $
    '!9'+string(162B)+'!X x '+strn(fac/60,format='(f10.2)')+'!9'+string(162B)+'!X'
  if (dssfile ne '') then begin
    xyouts,.06,1.11,/norm,'DSS Plate '+strn(sxpar(h,'PLTLABEL'))+ $
      '  ('+strn(sxpar(h,'DATE-OBS'))+')  ('+strn(fix(sxpar(h,'EXPOSURE')))+' min)'
    endif
  xyouts,0.97,1.17,align=1,/norm,getenv('USER')+':  '+!stime
  xyouts,0.57,1.07,/norm,'mkfchart ver. 16nov96',charsize=.9

  plots,/norm,[0,0,0,1,-1]*.01+0.03,[1,-1,0,0,0]*.01+1.08
  xyouts,.05,1.07,/norm,'Position of Coordinates'

  plots,[0,1,1,0,0],[0,0,1,1,0],/norm,thick=thk


;  print,'Coordinate system labels debugging information:'

; -- DEC minute labels ---------------------------------------
  stepsize=fix(fac/60.0/16)>1
  centralmin=(dec-fix(dec))*60 & sign=1
  if (centralmin lt 0.0) then begin
    centralmin=abs(centralmin)
    sign=-1
    endif
  botlim=centralmin-fac/60/2
  toplim=centralmin+fac/60/2
  i=fix(botlim*1.0/stepsize-1)*stepsize
  while (i lt botlim) do i=i+stepsize
  while (i lt toplim) do begin
    if (sign eq 1) then lev=(i-botlim)*60/fac $
    else lev=(toplim-i)*60/fac
    lab=fix(i) & if (lab gt 59) then lab=lab-60 & if (lab lt 0) then lab=lab+60
    labl=strn(lab)+'!9'+string(162B)+'!X'
    plots,[.98,1.01],[lev,lev],/norm
    xyouts,1.015,lev-.007,labl,/norm,charsi=.8
    plots,[-.01,.02],[lev,lev],/norm
    xyouts,-.015,lev-.007,labl,/norm,ali=1,chars=.8
    print,lev,lab,'  ',labl
    i=i+stepsize
    endwhile


; -- RA minute labels ---------------------------------------
  botlim=(ra/15-fix(ra/15))*60 - fac/2/15/60/cos(dec/!radeg)
  toplim=(ra/15-fix(ra/15))*60 + fac/2/15/60/cos(dec/!radeg)
  incr=[1,2,5,10,20,30,60,90,120]
  diffs=abs((toplim-botlim)/10*60-incr)
  best=where(diffs eq min(diffs))

  i=fix(botlim-1)*1.0
  while (i lt botlim) do i=i+(incr(best))(0)/60.0
  while (i lt toplim) do begin
    lev=(i-botlim)*60*15/fac*cos(dec/!radeg)
    lab=i & if (lab gt 59) then lab=lab-60 & if (lab lt 0) then lab=lab+60
    labl=strn(fix(lab+.02))+'!Um!N'+strn(fix(((lab-fix(lab+.01))*60)+.5))+'!Us!N'
    plots,1-[lev,lev],[.98,1.01],/norm
    xyouts,1-lev,1.015,labl,/norm,charsize=.8,align=0.5
    plots,1-[lev,lev],[-.01,.02],/norm
    xyouts,1-lev,-.035,labl,/norm,charsize=.8,align=0.5
;    print,lev,lab,'  ',labl
    i=i+(incr(best))(0)/60.0
    endwhile


; -- Read and Draw GSC stars if desired -----------------------
  if (gscfile ne '') then begin
    if (not exist(gscfile)) then begin
      print,'ERROR: '+gscfile+' not found.'
      gscfile=''
      endif
    endif
  if (gscfile ne '') then begin
    gsc_read,gscfile,ss,/verbose
    for i=0,n_elements(ss)-1 do begin
      psym=4
      x=(ss(i).ra-ra)*cos(ss(i).dec/!radeg)*(-3600.0)/fac + 0.5
      y=(ss(i).dec-dec)*(3600.0)/fac + 0.5
      if (max(abs([x,y]-[0.5,0.5])) lt 0.5) then $
        plots,[x],[y],/norm,symsize=(16-ss(i).mag)/2.0,psym=psym
      endfor
    for i=15,5,-1 do begin
      plots,[16-i]/13.0,[-.09],/norm,psym=4,symsize=(16-i)/2.0
      xyouts,[16-i]/13.0,[-.16]+i/400.0,/norm,align=.5,strn(i)
      endfor
    xyouts,0.90,-0.09,'GSC',/norm
    xyouts,0.90,-0.11,'magnitude',/norm
    endif


; -- Read and Draw USNO-A1.0 stars if desired -----------------------
  if (a10file ne '') then begin
    if (not exist(a10file)) then begin
      print,'ERROR: '+a10file+' not found.'
      a10file=''
      endif
    endif
  if (a10file ne '') then begin
    psym=4
    a10read,a10file,data
    szdata=size(data)
    for i=0,szdata(2)-1 do begin
      x=(data(0,i)-ra)*cos(dec/!radeg)*(-3600.0)/fac + 0.5 +offset(0)
      y=(data(1,i)-dec)*(3600.0)/fac + 0.5 +offset(1)
      if (max(abs([x,y]-[0.5,0.5])) lt 0.5) then begin
        plots,[x],[y],/norm,symsize=((22-data(3,i))/1.5)>0.4,psym=psym
        if (usnolabel gt 0) then $
          xyouts,x+0.02,y,/norm,'V='+strn(data(3,i),format='(f10.1)')+ $
            ', B-V='+strn(data(2,i)-data(3,i),format='(f10.1)'),charsize=1.3,$
            orient=orient
        endif
      endfor
    close,1
    for i=21.0,14.5,-0.5 do begin
      plots,[21-i]/7.5,[-.09],/norm,psym=4,symsize=((22.0-i)/1.5)>.4
      xyouts,[21-i]/7.5,[-.17]+i/500.0,/norm,align=.5,strn(i,format='(f6.1)')
      endfor
    xyouts,0.92,-0.09,'Magnier',/norm
    xyouts,0.91,-0.12,'V magnitude',/norm
    endif


; -- Draw a polygon if desired -----------------------
  if (polygon1(0) ne -1) then begin
    nel=n_elements(polygon1)
    if (nel/2 ne nel/2.0) then begin
      print,'polygon must have even number of elements'
      goto,POLYBAIL
      endif
    ras=polygon1(indgen(nel/2)*2)
    decs=polygon1(indgen(nel/2)*2+1)
    xs=(ras-ra)*cos(dec/!radeg)*(-3600.0)/fac + 0.5
    ys=(decs-dec)*(3600.0)/fac + 0.5
    plots,xs,ys,/norm
    endif
POLYBAIL:


; -- Determine field size -----------------------

  fldsz=0.281585*1023
  if (inst eq 'SPICAM') then fldsz=0.281585*1023
    						; GRIM: 0.473, 0.236, 0.113
  if (inst eq 'GRIM') then fldsz=0.236*256
  if (inst eq 'STIS') then fldsz=25


; -- Draw SPIcam field interactively if desired -----------------------
  if (strn(inter1(0)) ne "-1") and (!d.name eq 'X') then begin
    intersz=size(inter1)
    if (intersz(1) eq 7) then openw,1,inter1
    flag=0 & rotang=0 & fctr=0
    device,set_graphics=6
    camxv=([0,1,1,0,0]-0.5)*fldsz/fac
    camyv=([0,0,1,1,0]-0.5)*fldsz/fac
    curx=0.5 & cury=0.5
    ang=RotAng/!radeg
    camx=camxv*cos(ang)-camyv*sin(ang)+curx
    camy=camxv*sin(ang)+camyv*cos(ang)+cury
    plots,camx,camy,/norm
    while (flag eq 0) do begin
      print,''
      print,'1 - Set Rotator Angle'
      print,'2 - Place SPIcam field'
      print,'q - Quit'
      print,'Enter option [1,2,q]: '
      key1=get_kbrd(1)
      if (key1 eq '1') then begin
        print,'Current Rotator Angle is ',strn(rotang)
        read,'New Angle: ',rotang
        plots,camx,camy,/norm
        ang=RotAng/!radeg
        camx=camxv*cos(ang)-camyv*sin(ang)+curx
        camy=camxv*sin(ang)+camyv*cos(ang)+cury
        plots,camx,camy,/norm
        endif
      if (key1 eq '2') then begin
        print,'Left Mouse Button - place field'
        print,'Right Mouse Button - Done'
AGAIN1:
        !ERR=0
        while (!ERR eq 0) do begin
          cursor,curx,cury,/norm,/change
          plots,camx,camy,/norm
          camx=camxv*cos(ang)-camyv*sin(ang)+curx
          camy=camxv*sin(ang)+camyv*cos(ang)+cury
          plots,camx,camy,/norm
          endwhile
        if (!ERR eq 1) then begin
          cursor,d1,d2,/norm,/up
          device,set_graphics=3
          plots,camx,camy,/norm,color=(!d.n_colors<256)-2
          device,set_graphics=6
          decpos=(cury-0.5)*fac/3600+dec
          rapos=(curx-0.5)*fac/(-3600)/cos(decpos/!radeg)+ra
          print,fctr,RotAng,rapos,decpos,'  ',adstring(rapos,decpos,1),$
            format='(i3,f7.3,2f12.6,a,a)'
          if (intersz(1) eq 7) then $
            printf,1,fctr,RotAng,rapos,decpos,'  ',adstring(rapos,decpos,1),$
              format='(i3,f7.3,2f12.6,a,a)'
          curx=0.0 & cury=0.0 & fctr=fctr+1
          camx=camxv*cos(ang)-camyv*sin(ang)+curx
          camy=camxv*sin(ang)+camyv*cos(ang)+cury
          plots,camx,camy,/norm
          goto,AGAIN1
          endif
        endif
      if (key1 eq 'q') then flag=99
      endwhile
    device,set_graphics=3
    if (intersz(1) eq 7) then close,1
    endif
INTERBAIL:


; -- Draw SPIcam field interactively if desired -----------------------
  if (spifields ne '') then begin
    if (not exist(spifields)) then begin
      print,'ERROR: '+spifields+' not found.'
      spifields=''
      endif
    endif
  if (spifields ne '') then begin
    openr,1,spifields
    tmp1=dblarr(10)
    camxv=([0,1,1,0,0]-0.5)*fldsz/fac
    camyv=([0,0,1,1,0]-0.5)*fldsz/fac

    while not EOF(1) do begin
      readf,1,tmp1
      RotAng=tmp1(1)
      ang=RotAng/!radeg
      rapos=tmp1(2)
      decpos=tmp1(3)
      if (tmp1(0) ge 0) then begin
        curx=(rapos-ra)*cos(dec/!radeg)*(-3600.0)/fac + 0.5
        cury=(decpos-dec)*(3600.0)/fac + 0.5
        camx=camxv*cos(ang)-camyv*sin(ang)+curx
        camy=camxv*sin(ang)+camyv*cos(ang)+cury
        plots,camx,camy,/norm
        endif
      if (tmp1(0) eq -1) then begin
        curx=(rapos-ra)*cos(dec/!radeg)*(-3600.0)/fac + 0.5
        cury=(decpos-dec)*(3600.0)/fac + 0.5
        ciridx=findgen(500)/499*2*!pi
        camx=RotAng*60/fac*sin(ciridx)+curx
        camy=RotAng*60/fac*cos(ciridx)+cury
        plots,camx,camy,/norm
        endif
      endwhile
    close,1
    endif


  if (!d.name eq 'PS') then psclose
  close,1

end
