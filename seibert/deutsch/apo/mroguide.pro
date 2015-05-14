pro mroguide,instrument,coords,dssfile=dssfile,gscfile=gscfile,ps=ps, $
  objname=objname,suggest=suggest,InstAng=InstAng,a10file=a10file
;+
; NAME:
;	MROGUIDE
;
; PURPOSE:
;	Generate a guide star acquisition map for the MRO guider camera.
;	See the separate instruction sheet for complete instructions:
;	(http://www.astro.washington.edu/deutsch/apoinfo/guider/guideacq.txt)
;
; CATEGORY:
;	MRO software
;
; CALLING SEQUENCE:
;	mroguide,instrument,coords [,dssfile=,gscfile=,/ps,objname=]
;
; INPUTS:
;	instrument: A string specifying the desired primary instrument.
;		Current choices are: MRO
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
;		around 14th magnitude, the GSC is not really necessary.
;
;	ps:	If this keyword is set, the the output is directed to the
;		PostScript file 'gdracq.ps' instead of the screen.  Alternately
;		this keyword can be set to a string which is the filename.
;
;	objname: A string which will be printed out as an object name at the
;		top of a PostScript map.
;
;	suggest: If this keyword is set (default), a list of GSC stars in
;		the annulus is listed in brightness order.
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
;	mroguide,'tbird',coords,dssfile='test08zr.fits',gscfile='test.gsclst'
;	 (see separate instruction sheet for complete details)
;
; MODIFICATION HISTORY:
;	5/05/95 Written by E. Deutsch
;	9/13/95 Improved background level determination.  E. Deutsch
;	9/20/96 Updated to be similar to apoguide.pro.  E. Deutsch
;-


; -- This is a site-specific location of a necessary dummy header file ---
  dsshdrfile='$IDL_MAIN/deutsch/apo/apoguide.hhh'
  verstr='UW mroguide ver. 07jun99'


; -- Not enough parameters?  Show the call sequence -------------------
  if (n_params(0) lt 2) then begin
    print,'Call> mroguide,instrument,coords,[dssfile=],[gscfile=],[/ps],[objname=]'
    print,"e.g.> mroguide,'tbird','8 42 4.9  +33 42 56',dssfile='kuv.fits',/ps"
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
  if (n_elements(objname) eq 0) then objname=''
  if (n_elements(suggest) eq 0) then suggest=1
  if (n_elements(InstAng) eq 0) then InstAng=0.0
  usnolabel=0
  usnolimit=12

; -- Convert and check the specified coordinates ---------------
  if (n_elements(coords) eq 0) then begin
    print,'ERROR: Coordinate variable undefined!'
    return
    endif
  stringad,coords,ra,dec
  if (ra eq 0.0) and (dec eq 0.0) then begin
    print,'ERROR: RA and DEC is equal to 0.0.  Probably an error occurred.  exit."
    return
    endif


; -- Set up fixed variables -------
  stringad,coords,ra,dec
  pltscl=1.70


; -- Set up instrument-specific variables -------
  gxcen=52.3
  gycen=50.0
  gdist=sqrt(gxcen^2+gycen^2)
  gxsz=4.0*60
  gysz=4.0*60
  gscl=20.0
  range=round(25*60.0/pltscl)/2*2+1

  ixcen=0.0
  iycen=0.0
  ixsz=12.0*60
  iysz=12.0*60


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
    if ((size(img))(1) lt 2100) then begin
      print,'Size of this DSS image is too small.  You must have at least'
      print,'a 60x60 arcminute field for this to work properly.  I will'
      print,"try this, but you probably won't be happy with the results.."
      endif
    gsssextast,h,gsa
    gsssadxy,gsa,ra,dec,x,y
    gsss_stdast,h,x+[-400,0,400],y+[400,-400,100]
    getrot,h,rot1,cdelt1
    print,'Rotating image by ',strn(rot1),' degrees....'
    hrot,img,h,img3,h3,rot1,x,y,0,missing=0
    adxy,h3,ra,dec,x,y

    xc=round(x) & yc=round(y)
    img2=extrac(img3,xc-range,yc-range,range*2+1,range*2+1)
    imsize=size(img2)
    fac=pltscl*imsize(1)
    skyv=30000.0 & rmsv=skyv
    for ij=0,10 do begin
      skyline,img2(imsize(4)/13*ij+1000:imsize(4)/13*ij+6000),v1,v2
      skyv=skyv<v1 & rmsv=rmsv<v2
      endfor
    endif

DSSBAIL:
  if (dssfile eq '') then begin
    img2=fltarr(16,16)
    skyv=100.0 & rmsv=10.0
    sxhread,dsshdrfile,h & h3=h
    fac=pltscl*883*2
    endif


; -- Display the image ----------------------------------------
  if (ps eq 1) then begin
    !p.font=0
    if (n_elements(img2) ne 256) then img2=congrid(img2,1059,1059)
    psout,/inv,imscl(img2-skyv-rmsv*3,0,15000,1500),7.5,7.5,0.5,1.5, $
      /dontclose,filename=psfile
    thk=2
  endif else begin
    !p.font=-1
    tv,imscl(congrid(img2,!d.x_size,!d.y_size)-skyv-rmsv*3,0,15000,1500,top=!d.n_colors-2)
    thk=1
    endelse


; -- Draw instrument ----------------------------------------
  plots,/norm,[-1,1,1,-1,-1]*ixsz/2/fac-ixcen/fac+.50, $
    [-1,-1,1,1,-1]*iysz/2/fac+iycen/fac+.50,thick=thk
  plots,/norm,[0,0,0,1,-1]*.01+0.5,[1,-1,0,0,0]*.01+0.5


; -- Plot Movable Stage Box -----------------------
  plot,findgen(12)-0.5,(findgen(12)-0.5)*10,xsty=1,ysty=1, $
    /noerase,/nodata,xrange=[10.5,-0.5], $
    pos=[(0-5-(100-gxcen))*gscl/fac,(0-5-gycen)*gscl/fac, $
         (100+5-(100-gxcen))*gscl/fac,(100+5-gycen)*gscl/fac]+0.5,$
    xtitle='centimeters E/W',ytitle='millimeters N/S'

  for i=0,10,2 do begin
    xyouts,-0.58,i*10-1.2,strn(i*10),/data
    xyouts,i,107,strn(i),align=0.5
    endfor


; -- Draw Zone of Avoidance Circle -----------------------
  i=findgen(10000)/9999*2*!dpi
  plots,/data,cos(i)*3.5+5.0, $
    sin(i)*35.0+50.0,linesty=2,thick=thk


; -- Draw Cardinal Arrows------------------------
  if (!d.name eq 'PS') then begin
    !p.font=0
    arrows,h3,/norm,.97,1.07,arrowlen=2,charsize=1,color=1
  endif else begin
    !p.font=-1
    arrows,h3,/norm,.97,.03,arrowlen=2,charsize=1
    endelse


; -- Write Text Labels ---------------------------------------
  objname1=objname
  if (strn(objname) ne '') then objname1=' for '+objname
  xyouts,.05,1.20,/norm,'MRO Guider Acquisition Chart'+objname1
  xyouts,.06,1.17,/norm,'at coordinates: '+coords+' (J2000)'
  xyouts,.06,1.14,/norm,'Instrument: '+strupcase(instrument)+ $
    '     Guider: Lynxx'
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
  incr=[10,20,30,60,90,120]
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
    ngscstars=n_elements(ss)
    x=fltarr(ngscstars) & y=x
    psym=4
    for i=0,ngscstars-1 do begin
      x(i)=(ss(i).ra-ra)*cos(ss(i).dec/!radeg)*(-3600.0)/fac + 0.5
      y(i)=(ss(i).dec-dec)*(3600.0)/fac + 0.5
      if (max(abs([x(i),y(i)]-[0.5,0.5])) lt 0.5) then $
        plots,[x(i)],[y(i)],/norm,psym=psym,symsize=(16-ss(i).mag)/2.0
      endfor
    for i=15,5,-1 do begin
      plots,[16-i]/13.0,[-.09],/norm,psym=psym,symsize=(16-i)/2.0
      xyouts,[16-i]/13.0,[-.16]+i/400.0,/norm,align=.5,strn(i)
      endfor
    xyouts,0.90,-0.09,'GSC',/norm
    xyouts,0.90,-0.11,'magnitude',/norm
    endif


; -- Read and Draw USNO-A2.0 stars if desired -----------------------
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
    for i=0L,szdata(2)-1 do begin
      x=(data(0,i)-ra)*cos(dec/!radeg)*(-3600.0)/fac + 0.5
      y=(data(1,i)-dec)*(3600.0)/fac + 0.5
      if (max(abs([x,y]-[0.5,0.5])) lt 0.5) and (data(3,i) le usnolimit) then begin
        plots,[x],[y],/norm,symsize=((21-data(3,i))/3.0)>0.4,psym=psym
        if (usnolabel gt 0) then $
          xyouts,x+0.02,y,/norm,'R='+strn(data(3,i),format='(f10.1)')+ $
            ', B-R='+strn(data(2,i)-data(3,i),format='(f10.1)'),charsize=0.7
        endif
      endfor
    close,1
    for i=20,5,-1 do begin
      if (i le usnolimit) then begin
        plots,[20-i]/17.0,[-.09],/norm,psym=4,symsize=((21-i)/3.0)>.4
        xyouts,[20-i]/17.0,[-.17]+i/500.0,/norm,align=.5,strn(i)
        endif
      endfor
    xyouts,0.92,-0.09,'USNO-A2.0',/norm
    xyouts,0.91,-0.12,'R magnitude',/norm
    endif


; -- Suggest Possible Guide Stars ---------------------------
  if (suggest eq 1) and (gscfile ne '') then begin
    dist=sqrt((x-0.5)^2 + (y-0.5)^2)
    EWloc=(x/(-gscl)*fac+gxcen+75)/10
    NSloc=y/gscl*fac-gycen/2
    good=where((dist gt 0.22) and (EWloc gt -1.5) and (EWloc lt 11.5) and $
      (NSloc gt -15) and (NSloc lt 115) and (ss.mag lt 12))
    if (good(0) eq -1) then goto,NOSTARS

    data=fltarr(3,n_elements(good))
    for i=0,n_elements(good)-1 do begin
      data(*,i)=[ss(good(i)).mag,EWloc(good(i)),NSloc(good(i))]
      endfor

    brt=sort(data(0,*))
    if (ps eq 1) then begin
      if (not exist('suggest.dat')) then begin
        openw,6,'suggest.dat' & lin=''
        openr,7,'/host/bluemoon/usr2/idllib/deutsch/apo/mroguide.hdr1'
        while not EOF(7) do begin
          readf,7,lin & printf,6,lin
          endwhile
        close,7
        endif $
      else openu,6,'suggest.dat',/append
      endif

    print,''
    print,'Here is the list of guide stars which may be suitable:'
    print,'Index   Mag   E/W Pos  N/S Pos'
    print,'-----  -----  -------  -------'
    if (ps eq 1) then begin
      for jj=0,3 do printf,6,''
      printf,6,'Suggested GSC guide stars'+objname1
      printf,6,' at coordinates: '+coords+' (J2000)'
      printf,6,'     '+getenv('USER')+':  '+stime
      printf,6,'     '+verstr
      printf,6,''
      printf,6,'Index   Mag   E/W Pos  N/S Pos'
      printf,6,'-----  -----  -------  -------'
      endif
    for i=0,n_elements(good)-1 do begin
      print,i,data(*,brt(i)),format='(i5,f7.1,f9.1,i9)'
      if (ps eq 1) then printf,6,i,data(*,brt(i)),format='(i5,f7.1,f9.1,i9)'
      endfor
    if (ps eq 1) then close,6
    endif

NOSTARS:


  if (!d.name eq 'PS') then psclose
  close,/all


end
