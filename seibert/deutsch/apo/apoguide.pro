pro apoguide,instrument,coords,dssfile=dssfile,gscfile=gscfile,ps=ps, $
  objname=objname,suggest=suggest,InstAng=InstAng,a10file=a10file
;+
; NAME:
;	APOGUIDE
;
; PURPOSE:
;	Generate a guide star acquisition map for the APO guider camera.
;	See the separate instruction sheet for complete instructions:
;	(http://www.astro.washington.edu/deutsch/apoinfo/guider/guideacq.txt)
;
; CATEGORY:
;	APO software
;
; CALLING SEQUENCE:
;	apoguide,instrument,coords [,dssfile=,gscfile=,/ps,objname=]
;
; INPUTS:
;	instrument: A string specifying the desired primary instrument.
;		Current choices are: dis, grim, dsc
;
;	coords: A string containing the J2000 coordinates of the target
;		onject in sexigesimal format.
;
; OPTIONAL INPUT KEYWORDS:
;	dssfile: The filename of a Digital Sky Survey (DSS) FITS image which
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
;	suggest: If this keyword is set (default), a list of GSC stars in
;		the annulus is listed in brightness order.  If ps is
;		set, then the output is written to a file called suggest.dat
;
;	InstAng: Specify the desired Instrument Angle; the default is 0.
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
;	Load DSS image if provided, rotate and display an appropriate region.
;	Load GSC list if provided, display star symbols.  Finally, overlay
;	the acquisition field map and labels.
;
; EXAMPLE:
;	window,colors=50,xs=512
;	whitesky,res='RED'
;	coords='19 15 11.52  +10 56 45.1'
;	apoguide,'DIS',coords,dssfile='test08zr.fits',gscfile='test.gsclst'
;	 (see separate instruction sheet for complete details)
;
; MODIFICATION HISTORY:
;	5/05/95 Written by E. Deutsch
;	5/20/95 Changed Guider Instrument offsets to 5/15/95 instrument
;	        block values.
;	7/11/95 Added default dsshdrfile variable.  E. Deutsch
;	9/13/95 Improved background level determination.  E. Deutsch
;	10/11/95 Made major changes to the logic.  Now uses actual
;		Instrument Blocks as input.  New verified with
;		actual observations.  E. Deutsch
;	10/20/95 Added coordinate grid labels.  E. Deutsch
;	10/25/95 Fixed some rouning errors in thr coordinate grid labels,
;		and added rotator angle numbers for GRIM.  Also modified the
;		image stretch algoritm back to imscl().  E. Deutsch
;	11/20/95 Removed bad column graphic for SS512.  E. Deutsch
;	04/17/96 Updated labels or other various things to conform to the
;		new convention of rotator angle.  E. Deutsch
;	05/05/96 Tidied up some of the angles stuff after updating the
;		inst blocks.  Also added the /----X graphics, although
;		I kind of doubt that they're right.  E. Deutsch
;	09/24/96 Added code to support SPICAM.  E. Deutsch
;	09/24/96 Added code for the X,Y orientation of the cameras.  E. Deutsch
;	10/25/96 Changed the behavior of GC_GIm_ang to something a little
;		more correct, although it is far from understood.  E. Deutsch
;	10/25/96 Made lots of other transformation changes.  I think it
;		is more correct than it was, but there are still problems.  E. Deutsch
;	12/31/96 Added support of the USNO-A1.0 catalog.  E. Deutsch
;
;-


; -- This is a site-specific location of a necessary dummy header file ---
  dsshdrfile='$IDL_MAIN/deutsch/apo/apoguide.hhh'
  verstr='UW apoguide ver. 30jul97'


; -- Not enough parameters?  Show the call sequence -------------------
  if (n_params(0) lt 2) then begin
    print,'Call> apoguide,instrument,coords, [dssfile=],[gscfile=],[/ps],[objname=]'
    print,"e.g.> apoguide,'DIS','20 33 26.064  +60 39 55.26',dssfile='n69306z0.fits',object='NGC 6930',/ps"
    return
    endif

; -- Do we want PostScript output?  To what filename? ------------
  psfile='gdracq.ps'
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
  if (n_elements(a10file) eq 0) then a10file=''
  if (n_elements(objname) eq 0) then objname=''
  if (n_elements(suggest) eq 0) then suggest=1
  if (n_elements(InstAng) eq 0) then InstAng=0.0


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


; -- Set up fixed variables ------------------------------------
  pltscl=1.70
  range=1050.0/pltscl
  range=round(1048.0/pltscl)/2*2+1


; -- Set up instrument-specific variables from inst block --------
  apogetinst,instrument,success
  if (success lt 0) then return
  COMMON INSTBLOCK,IIm_Offset,IIm_Scale,IIm_MinXY,IIm_MaxXY,Rot_Inst_xy, $
    Rot_Inst_ang,GIm_Offset,GIm_Scale,GIm_MinXY,GIm_MaxXY, $
    GC_GIm_xy,GC_GIm_ang


; -- Here's a fudge in the coordinates if necessary
  raorig=ra & decorig=dec
  ; here we add a fudge for the DIS because the instrument block is wrong
  ;if (strupcase(instrument) eq 'DIS') and $
  ;  (n_elements(disfudge) ne 0) then dec=dec+80.0/3600.0


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
    if ((size(img))(1) lt 1400) then begin
      print,'Size of this DSS image is too small.  You must have at least'
      print,'a 40x40 arcminute field for this to work properly.  I will'
      print,"try this, but you probably won't be happy with the results.."
      endif
    gsssextast,h,gsa
    gsssadxy,gsa,ra,dec,x,y
    gsss_stdast,h,x+[-200,0,200],y+[200,-200,50]
    getrot,h,rot1,cdelt1
    print,'Rotating image by ',strn(rot1),' degrees....'
    hrot,img,h,img3,h3,rot1,x,y,0,missing=0
    adxy,h3,ra,dec,x,y

    xc=round(x) & yc=round(y)
    img2=extrac(img3,xc-range,yc-range,range*2+1,range*2+1)
    imsize=size(img2) & print,imsize
    fac=pltscl*imsize(1)
    skyv=30000.0 & rmsv=skyv
    for ij=0,10 do begin
      skyline,img2(imsize(4)/13*ij+1000:imsize(4)/13*ij+6000),v1,v2
      skyv=skyv<v1 & rmsv=rmsv<v2
      endfor
    print,'SkyV,rmsV=',skyv,rmsv
    endif

DSSBAIL:
  if (dssfile eq '') then begin
    img2=fltarr(16,16)
    skyv=100.0 & rmsv=10.0
    sxhread,dsshdrfile,h & h3=h
    fac=pltscl*1235
    endif


; -- Display the image ----------------------------------------
  if (ps eq 1) then begin
    psout,/inv,imscl(img2-skyv-rmsv*3,0,15000,1500),7.5,7.5,0.5,1.5, $
      /dontclose,filename=psfile
    thk=2
  endif else begin
    tv,imscl(congrid(img2,!d.x_size,!d.y_size)-skyv-rmsv*3,0,15000,1500,top=!d.n_colors-2)
    thk=1
    endelse


; -- Draw instrument ----------------------------------------
  signs=IIm_Scale/abs(IIm_Scale)
  print,'signs=',signs
  scl=1.0/(IIm_Scale/3600)		; pixels per arcsecond
  sz=(IIm_MaxXY-IIm_MinXY)*scl		; chip size ('')
  coff=((IIm_MinXY+IIm_MaxXY)/2-IIm_Offset)*scl
					; offset ('') from chip physical cent

  outerxx=[-2.5,-2.3,-2.7,-2.5,-2.7,-2.3,-2.5,  -1]*(-1)/signs(0)
  outerxy=[  -1, -.8,-1.2,  -1, -.8,-1.2,  -1,  -1]/signs(1)
  outeryx=[1,1,1.2,1,.8,1,           1,-1]*(-1)/signs(0)
  outeryy=[-1,2.5,2.7,2.5,2.7,2.5,  -1,-1]/signs(1)

  x1=[-1,1,1,-1,-1,outerxx,outeryx, $
    -1,-.5,.5,.3,.7,.5,.7,.3,       .5,-.5,-.5,-.3,-.5,-.7]*sz(0)/2+coff(0)
  y1=[-1,-1,1,1,-1,outerxy,outeryy, $
    -1,-.5,-.5,-.3,-.7,-.5,-.3,-.7, -.5,-.5,.5,.7,.5,.7]*sz(1)/2+coff(1)
  y1=-y1				; conversion from inst to normal coordinates

  ang=-round(Rot_Inst_ang/90)*90/!radeg
  x2=x1*cos(ang)-y1*sin(ang)
  y2=x1*sin(ang)+y1*cos(ang)

  ang=InstAng/!radeg
  x=x2*cos(ang)-y2*sin(ang)
  y=x2*sin(ang)+y2*cos(ang)

  plots,/norm,0.5+x/fac,0.5+y/fac,thick=thk
  plots,/norm,[0,0,0,1,-1]*.01+0.5,[1,-1,0,0,0]*.01+0.5
  plots,/norm,[0,1]*Rot_Inst_xy(0)*3600/fac+0.5,[0,-1]*Rot_Inst_xy(1)*3600/fac+0.5


; -- Draw guide camera field ----------------------------------------
; Note: Russ says that GC_GIm_xy means: (not what inst block comment says)
;    ! pos. of the rotator axis in the guide image frame (deg on sky)
; Rotate the guider offset into instrument coordinate system

  GC_GIm_xy_orig=GC_GIm_xy
  ang=GC_GIm_ang/!radeg
  GC_GIm_xy(0)=GC_GIm_xy_orig(0)*cos(ang)-GC_GIm_xy_orig(1)*sin(ang)
  GC_GIm_xy(1)=GC_GIm_xy_orig(0)*sin(ang)+GC_GIm_xy_orig(1)*cos(ang)
  GC_GIm_xy=-GC_GIm_xy			; switch origins from guider to rotator??
  Rot_Inst_xy=Rot_Inst_xy*signs		; switch from instrument to normal

  scl=1.0/(GIm_Scale/3600)		; pixels per arcsecond
  sz=(GIm_MaxXY-GIm_MinXY+1)*scl	; chip size ('')
  coff=sz/2.0-GIm_Offset*scl		; offset ('') from chip physical cent
  roff=(GC_GIm_xy-Rot_Inst_xy)*3600	; offset ('') from instrument axis  

; Try to calculate the exact instrument offset to the guider
  troff=roff
  ang=-Rot_Inst_ang/!radeg
  troff(0)=roff(0)*cos(ang)-roff(1)*sin(ang)
  troff(1)=roff(0)*sin(ang)+roff(1)*cos(ang)
  print,'Instrument offset from Rotator Axis: ',Rot_Inst_xy*3600*[1,-1]
  print,'Guider offset from Rotator Axis:     ',GC_GIm_xy*3600*[1,-1]
  print,'Guider offset from Instrument Axis:  ',troff*[1,-1]


  x1=[-1,1,1,-1,-1, -.5,.5,.3,.7,.5,.7,.3,       .5,-.5,-.5,-.3,-.5,-.7]*sz(0)/2+coff(0)
  y1=[-1,-1,1,1,-1, -.5,-.5,-.3,-.7,-.5,-.3,-.7, -.5,-.5,.5,.7,.5,.7]*sz(1)/2+coff(1)

;  ang=-Rot_Inst_ang/!radeg
;  x3=x1*cos(ang)-y1*sin(ang)
;  y3=x1*sin(ang)+y1*cos(ang)
  x3=x1 & y3=y1

  ang=GC_GIm_ang/!radeg
  x2=x3*cos(ang)-y3*sin(ang)
  y2=x3*sin(ang)+y3*cos(ang)

  x2=x2+roff(0)
  y2=y2+roff(1)
  y2=-y2				; conversion from inst to normal coordinates

  for i=0,270,90 do begin
    ang=(Rot_Inst_ang+InstAng+i)/!radeg
    x=x2*cos(ang)-y2*sin(ang)
    y=x2*sin(ang)+y2*cos(ang)
    plots,/norm,0.5+x/fac,0.5+y/fac,thick=thk
    endfor


; -- Draw annulus -------
  i=findgen(1000)/999*2*!dpi
  inrad=min(sqrt(x^2+y^2))
  outrad=max(sqrt(x^2+y^2))
  plots,/norm,cos(i)*inrad/fac+.50, $
    sin(i)*inrad/fac+.50,linesty=2,thick=thk
  plots,/norm,cos(i)*outrad/fac+.50, $
    sin(i)*outrad/fac+.50,linesty=2,thick=thk

  line=[1,1.1]
  for i=0,350,10 do $
    plots,/norm,cos(i/!radeg)*outrad/fac*line+.50, $
      sin(i/!radeg)*outrad/fac*line+.50,thick=thk


; -- Draw Labels ----------------------------------
  if (!d.name eq 'PS') then begin
    !p.font=0
    arrows,h3,/norm,.97,1.07,arrowlen=2,charsize=1,color=1
  endif else begin
    !p.font=-1
    arrows,h3,/norm,.97,.03,arrowlen=2,charsize=1
    endelse

; -- Write Instrument Angle Labels ---------------------------

;  if (strupcase(instrument) eq 'DIS') then Iang=[0,90,180,-90]
;  if (strupcase(instrument) eq 'SPI') then Iang=[0,90,180,-90]
;  if (strupcase(instrument) eq 'DSC') then Iang=[-90,0,90,180]
;  if (strupcase(instrument) eq 'GRIM') then Iang=[-90,0,90,180]
  rotcor=0
;  if (signs(0) lt 0) then rotcor=180
  Iang=[0,90,180,270]+rotcor-round(Rot_Inst_ang/90)*90
  ttmp1=where(Iang gt 180)
  if (ttmp1(0) ne -1) then Iang(ttmp1)=Iang(ttmp1)-360

  for i=0,3 do $
    xyouts,/norm,cos(i*90/!radeg)*outrad/fac*1.17+.50,orien=i*90, $
      sin(i*90/!radeg)*outrad/fac*1.17+.50,strn(Iang(i)),align=.5,charsize=1.5


; -- Write Text Labels ---------------------------------------
  if (strn(objname) ne '') then objname=' for '+objname
  xyouts,.05,1.20,/norm,'APO Guider Acquisition Chart'+objname
  xyouts,.06,1.17,/norm,'at coordinates: '+coords+' (J2000)'
  xyouts,.06,1.14,/norm,'Instrument: '+strupcase(instrument)+ $
    '     Guider: Photometrics '+strn(fix(GIm_MaxXY(0)+1))+'!U2!N'
  xyouts,.06,1.11,/norm,'InstAng: '+strn(fix(InstAng))
  stime=!stime
  xyouts,0.97,1.17,align=1,/norm,getenv('USER')+':  '+stime
  xyouts,0.57,1.07,/norm,verstr,charsize=.9

  plots,/norm,[0,0,0,1,-1]*.01+0.03,[1,-1,0,0,0]*.01+1.08
  xyouts,.05,1.07,/norm,'Instrument Axis (Boresight)'

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
;    print,lev,lab,'  ',labl
    i=i+stepsize
    endwhile


; -- RA minute labels ---------------------------------------
  botlim=(ra/15-fix(ra/15))*60 - fac/2/15/60/cos(dec/!radeg)
  toplim=(ra/15-fix(ra/15))*60 + fac/2/15/60/cos(dec/!radeg)
  incr=[10,20,30,60,90,120,240,480]
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
    print,(incr(best))(0),lev,lab,'  ',labl
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
    dist=sqrt((decorig-ss.dec)^2+((raorig-ss.ra)*cos(decorig/!radeg))^2)*3600
    good=where((dist gt inrad+5) and (dist lt outrad-5))
    for i=0,n_elements(ss)-1 do begin
      psym=4
      if (min(abs(good-i)) eq 0) then psym=4
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


; -- Suggest Possible Guide Stars ---------------------------
    if (suggest eq 1) then begin
      angl=atan((decorig-ss.dec)/ $
        ((raorig-ss.ra)*cos(decorig/!radeg)))*!radeg
      tmp1=where(raorig-ss.ra lt 0)
      if (tmp1(0) ne -1) then angl(tmp1)=angl(tmp1)+180
      angl=angl+Rot_Inst_ang & angl=-angl
      tmp1=where(angl lt -180)
      if (tmp1(0) ne -1) then angl(tmp1)=angl(tmp1)+360
      tmp1=where(angl gt 180)
      if (tmp1(0) ne -1) then angl(tmp1)=angl(tmp1)-360

      good=where((dist gt inrad+5) and (dist lt outrad-5))
      if (good(0) eq -1) then goto,NOSTARS
      cenrad=avg([inrad,outrad]) & halfwid=outrad-cenrad
      data=fltarr(4,n_elements(good))
      for i=0,n_elements(good)-1 do begin
        data(*,i)=[ss(good(i)).mag,angl(good(i)),dist(good(i)),(dist(good(i))-cenrad)/halfwid]
        endfor
      brt=sort(data(0,*))
      if (ps eq 1) then begin
        if (not exist('suggest.dat')) then begin
          openw,6,'suggest.dat' & lin=''
          openr,7,'/host/bluemoon/usr2/idllib/deutsch/apo/apoguide.hdr1'
          while not EOF(7) do begin
            readf,7,lin & printf,6,lin
            endwhile
          close,7
          endif $
        else openu,6,'suggest.dat',/append
        endif
      print,''
      print,'Here is the list of guide stars which fall in the annulus:'
      print,'Index   Mag    Angle   Radius  Off Center'
      print,'-----  -----  -------  ------  -------'
      if (ps eq 1) then begin
        for jj=0,3 do printf,6,''
        printf,6,'Suggested GSC guide stars'+objname
        printf,6,' at coordinates: '+coords+' (J2000)'
        printf,6,'     '+getenv('USER')+':  '+stime
        printf,6,'     '+verstr
        printf,6,''
        printf,6,'Index   Mag    Angle   Radius  Off Center'
        printf,6,'-----  -----  -------  ------  -------'
        endif
      for i=0,n_elements(good)-1 do begin
        print,i,data(*,brt(i)),format='(i5,f7.1,f9.1,f8.1,f9.2)'
        if (ps eq 1) then printf,6,i,data(*,brt(i)),format='(i5,f7.1,f9.1,f8.1,f9.2)'
        endfor
      endif
    if (ps eq 1) then close,6
    endif
NOSTARS:


; -- Read and Draw USNO-A1.0 stars if desired -----------------------
  if (a10file ne '') then begin
    if (not exist(a10file)) then begin
      print,'ERROR: '+a10file+' not found.'
      gscfile=''
      endif
    endif
  if (a10file ne '') then begin
    psym=4
    openr,1,a10file
    lin='' & readf,1,lin & readf,1,lin
    d1=dblarr(8)
    while not EOF(1) do begin
      readf,1,d1
      x=(d1(0)-ra)*cos(dec/!radeg)*(-3600.0)/fac + 0.5
      y=(d1(1)-dec)*(3600.0)/fac + 0.5
      if (max(abs([x,y]-[0.5,0.5])) lt 0.5) then $
        plots,[x],[y],/norm,symsize=((21-d1(3))/3.0)>0.4,psym=psym
      endwhile
    close,1
    for i=20,5,-1 do begin
      plots,[20-i]/17.0,[-.09],/norm,psym=4,symsize=((21-i)/3.0)>.4
      xyouts,[20-i]/17.0,[-.17]+i/500.0,/norm,align=.5,strn(i)
      endfor
    xyouts,0.92,-0.09,'USNO-A1.0',/norm
    xyouts,0.91,-0.12,'R magnitude',/norm
    endif


  if (!d.name eq 'PS') then psclose
  close,/all

end
