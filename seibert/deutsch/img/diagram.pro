pro diagram,paramfile,PS=PS,SkipRead=SkipRead,DontWrite=DontWrite, $
  NoMatchDump=NoMatchDump
;+
; NAME:
;   DIAGRAM
; PURPOSE:
;   Generate a snazzy color-magnitude diagram with a variety of options
;   and input formats.  Everything is controlled via a self-documenting
;   parameter file instead of keywords or parameters
; CALLING SEQEUNCE:
;   diagram,paramfile,[/ps,/SkipRead,/DontWrite]
; INPUT:
;   PARAMFILE This is the name of a diagram.pro parameter file.
; OPTIONAL KEYWORDS:
;   PS        Write to Postscript instead of the screen.
;   SkipRead  Remember and use the most recently-read dataset.
;   DontWrite Don;t bother to write diagram.out and diagram.dmp (much faster).
; HISTORY:
;   01-AUG-96 Added Header to program which has been evolving for years
;     (E. Deutsch)
;-

; ########## Calling Sequence ###############################################
  if (n_params(0) ne 1) then begin
    print,'Call: Diagram,paramfile,[/PS,/JustPlot,/SkipRead]
    print,"e.g.> Diagram,'chip0.params',/PS"
    return
    endif


; ########## Common Block Stuff to Recycle Data #############################
  COMMON DIAGDATA,LWdata,SWdata,LWi,SWi


; ########## Keyword Checking ###############################################
  if (n_elements(PS) eq 0) then PS=0
  if (PS eq 1) then begin
    setps,7,6.613+(1/16.) & !p.font=0 & device,/helv
  endif else !p.font=-1

  if (n_elements(DontWrite) eq 0) then DontWrite=0
  if (n_elements(NoMatchDump) eq 0) then NoMatchDump=0
  if (DontWrite eq 1) then NoMatchDump=1


; ########## Read Parameter File ##########################################
  INPUTMODE='' & SWINFILE='' & LWINFILE='' & DUMPFILE=''
  OUTFILE='' & SWLETTER='' & LWLETTER='' & LWXADJ=0. & LWYADJ=0.
  MATCHRAD=0. & CLIPEDGE=0. & CLIPMIN=0. & CLIPMAX=0. & XMIN=0.
  XMAX=0. & YMIN=0. & YMAX=0. & MTITLE='' & XTITLE='' & YTITLE=''
  PLOTSYM=3. & CONSERR=0. & MAXERROR=0.
  SWALLTYPES=0 & LWALLTYPES=0 & ONETYPEONE=0 & FORCETHREETOONE=0
  SHOWLEGEND=0 & SWEXPTIME=100.0 & LWEXPTIME=100.0
  SWZEROPT=25.0 & LWZEROPT=25.0 & CALIBFILE="" & PLOTCAL=1 & FORCETYPE=0
  SWAPERCORR=1.0 & LWAPERCORR=1.0 & SWUPPERLIM=1.0 & LWUPPERLIM=1.0
  MAXSTARS=5000 & CTECORR=-1 & SYMSIZE=0.4 & SUBTRACTFILE='' & SUBTRACTTIMES=1
  GAINCORR=1.0
  SELECTCIRCLE=0 & CIRCLECENX=0 & CIRCLECENY=0 & CIRCLERAD=0
  ERRVSMAGPLOT=0


  lineno=-1
  if (not exist(paramfile)) then begin
    print,'Parameter file '+paramfile+' not found.'
    return
    endif
  on_ioerror,PARFILERR
  openr,1,paramfile

  lin='' & lineno=0
  while not EOF(1) do begin
    readf,1,lin & lineno=lineno+1
    if (strmid(lin,0,1) ge "A") and (strmid(lin,0,1) le "Z") then begin
      tmp1=strpos(lin,'=') & if (tmp1 eq -1) then goto,PARFILERR
      keyword=strupcase(strmid(lin,0,tmp1))
      value=strtrim(strmid(lin,tmp1+1,100),2)
      if (strnumber(value) eq 1) then value=float(value) $
      else begin
        tmp1=strpos(value,'"') & if (tmp1 eq -1) then goto,PARFILERR
        endelse
      result=execute(keyword+'='+strn(value))
      endif
    endwhile

  close,1
  goto,START

PARFILERR:
  if (lineno eq -1) then print,'Unable to open parameter file "'+paramfile+'"' $
  else begin
    print,'Error reading parameter file "'+paramfile+'" at line ',strn(lineno)
    close,1
    endelse
  print,!ERR_STRING
  return


; ########## Let's start things rolling ####################################
START:
  if (n_elements(skipread) eq 0) then skipread=0
  if (skipread eq 1) then goto,SKIPREAD1

  LWdata=fltarr(6,MAXSTARS)	; [Index, X, Y, Flux, Err, Type]
  SWdata=fltarr(6,MAXSTARS)	; [Index, X, Y, Flux, Err, Type]
  print,'Reading data in ',LWINFILE,' and ',SWINFILE,' (Type ',INPUTMODE,')...'

  on_ioerror,GACK
  goto,NEXT1

GACK:
  print,'Error reading: ',LWINFILE,' or ',SWINFILE
  return

NEXT1:
; ########## READ SEQUENCE FOR DOPHOT DATA #################################
  if (INPUTMODE eq 'DOPHOT') then begin
    openr,1,LWINFILE
    datin=fltarr(15) & LWi=0
    while not EOF(1) do begin
      readf,1,datin
      if (FORCETHREETOONE eq 1) and (datin(1) eq 3) then datin(1)=1
      dt=datin([0,2,3,4,5,1])
      if (FORCETYPE eq 1) then dt(5)=1
      good=0
      if (LWALLTYPES eq 0) and (datin(1) eq 1) then good=1
      if (LWALLTYPES eq 1) and (datin(1) ne 8) then good=1
      if (LWALLTYPES eq 2) and ((datin(1) eq 1) or (datin(1) eq 3)) then good=1
      if good then begin
        LWdata(*,LWi)=dt
        LWi=LWi+1 & endif
      endwhile
    LWdata(3,*)=mag2flux(LWdata(3,*),0)
    LWdata(4,*)=mag2flux(LWdata(4,*),0)
    close,1

    openr,1,SWINFILE
    datin=fltarr(15) & SWi=0
    while not EOF(1) do begin
      readf,1,datin
      if (FORCETHREETOONE eq 1) and (datin(1) eq 3) then datin(1)=1
      dt=datin([0,2,3,4,5,1])
      if (FORCETYPE eq 1) then dt(5)=1
      good=0
      if (SWALLTYPES eq 0) and (datin(1) eq 1) then good=1
      if (SWALLTYPES eq 1) and (datin(1) ne 8) then good=1
      if (SWALLTYPES eq 2) and ((datin(1) eq 1) or (datin(1) eq 3)) then good=1
      if good then begin
        SWdata(*,SWi)=dt
        SWi=SWi+1 & endif
      endwhile
    SWdata(3,*)=mag2flux(SWdata(3,*),0)
    SWdata(4,*)=mag2flux(SWdata(4,*),0)
    close,1
    endif

; ##########################################################################
  if (inputmode eq 'DAOFIND') then begin
    openr,1,LWINFILE
    datin=fltarr(6) & LWi=0 & lin=''
    while (strmid(lin,0,2) ne '#F') do readf,1,lin
    readf,1,lin
    while not EOF(1) do begin
      readf,1,datin
      LWdata(*,LWi)=[datin([5,0,1,2,4]),1] & LWi=LWi+1
      endwhile
    LWdata(3,*)=mag2flux(LWdata(3,*),0)/.15*100
    close,1

    openr,1,SWINFILE
    datin=fltarr(6) & SWi=0 & lin=''
    while (strmid(lin,0,2) ne '#F') do readf,1,lin
    readf,1,lin
    while not EOF(1) do begin
      readf,1,datin
      SWdata(*,SWi)=[datin([5,0,1,2,4]),1] & SWi=SWi+1
      endwhile
    SWdata(3,*)=mag2flux(SWdata(3,*),0)/.15*100
    close,1
    endif


; ##########################################################################
  if (inputmode eq 'DAOPHOT') then begin
    openr,1,LWINFILE
    datin=fltarr(6) & LWi=0 & lin=''
    while (strmid(lin,0,8) ne '#U scale') do readf,1,lin
    readf,1,lin & readf,1,lin
    while not EOF(1) do begin
      readf,1,lin & datin(0)=fix(strmid(lin,43,5))
      readf,1,lin & datin(1)=float(strmid(lin,3,8))
                    datin(2)=float(strmid(lin,12,8))
      readf,1,lin & readf,1,lin & readf,1,lin
      isokay=strnumber(strmid(lin,42,6))
      if (isokay eq 1) then begin
        datin(3)=float(strmid(lin,42,6)) & datin(5)=1
        datin(4)=float(strmid(lin,49,5))
        if (datin(4) lt .5) then begin
          LWdata(*,LWi)=datin & LWi=LWi+1
          endif
        endif
      endwhile
    LWdata(3,*)=mag2flux(LWdata(3,*)-18,0)/.15*100
    close,1


    openr,1,SWINFILE
    datin=fltarr(6) & SWi=0 & lin=''
    while (strmid(lin,0,8) ne '#U scale') do readf,1,lin
    readf,1,lin & readf,1,lin
    while not EOF(1) do begin
      readf,1,lin & datin(0)=fix(strmid(lin,43,5))
      readf,1,lin & datin(1)=float(strmid(lin,3,8))
                    datin(2)=float(strmid(lin,12,8))
      readf,1,lin & readf,1,lin & readf,1,lin
      isokay=strnumber(strmid(lin,42,6))
      if (isokay eq 1) then begin
        datin(3)=float(strmid(lin,42,6)) & datin(5)=1
        datin(4)=float(strmid(lin,49,5))
        if (datin(4) lt .5) then begin
          SWdata(*,SWi)=datin & SWi=SWi+1
          endif
        endif
      endwhile
    SWdata(3,*)=mag2flux(SWdata(3,*)-18,0)/.15*100
    close,1
    endif

; ##########################################################################
  if (inputmode eq 'IMGCLEAN') then begin
    openr,1,LWINFILE
    datin=fltarr(5) & LWi=0
    while not EOF(1) do begin
      readf,1,format='(i5,2f8.2,f11.1,f9.1)',datin
      if (datin(3) gt 1) then begin
        LWdata(*,LWi)=[datin,1] & LWi=LWi+1
        endif
      endwhile
    close,1

    openr,1,SWINFILE
    datin=fltarr(5) & SWi=0
    while not EOF(1) do begin
      readf,1,format='(i5,2f8.2,f11.1,f9.1)',datin
      if (datin(3) gt 1) then begin
        SWdata(*,SWi)=[datin,1] & SWi=SWi+1
        endif
      endwhile
    close,1
    endif

; ##########################################################################
  if (inputmode eq 'FITPHOT') then begin
    openr,1,LWINFILE
    datin=fltarr(7) & LWi=0
    readf,1,lin & readf,1,lin
    while not EOF(1) do begin
      readf,1,datin
;      if (datin(6) gt 100) and (sqrt(total((datin(1:2)-[128,260])^2)) lt 71) then begin
      if (datin(6) gt 100) then begin
        LWdata(*,LWi)=[datin(0:2),datin(6),0.1,1] & LWi=LWi+1
        endif
      endwhile
    close,1

    openr,1,SWINFILE
    datin=fltarr(7) & SWi=0
    readf,1,lin & readf,1,lin
    while not EOF(1) do begin
      readf,1,datin
;      if (datin(6) gt 100) and (sqrt(total((datin(1:2)-[128,260])^2)) lt 71) then begin
      if (datin(6) gt 100) then begin
        SWdata(*,SWi)=[datin(0:2),datin(6),0.1,1] & SWi=SWi+1
        endif
      endwhile
    close,1
    endif


; ########### Snip data down to size #######################################
  LWdata=LWdata(*,0:LWi-1) & print,strn(LWi),' objects read from ',LWINFILE
  SWdata=SWdata(*,0:SWi-1) & print,strn(SWi),' objects read from ',SWINFILE


; ##########################################################################
; ##########################################################################


SKIPREAD1:

; ###### Create a lookup index so we don't have to compare so many stars ###
  LWdata=LWdata(*,sort(LWdata(2,*)))
  nindex=LWdata(2,LWi-1)
  index=indgen(nindex)
  LWlookup1=index(LWdata(2,*))
  LWlookup2=index
  for i=0,nindex-1 do LWlookup2(i)=(where(LWlookup1 eq i))(0)
  tmp1=where(LWlookup2 eq -1)
  while (tmp1(0) ne -1) do begin
    print,'Fixing '+strn(n_elements(tmp1))+' holes in the lookup index...'
    tmp2=tmp1+1<(nindex-1)
    LWlookup2(tmp1)=LWlookup2(tmp2)
    tmp1=where(LWlookup2 eq -1)
    endwhile


; ############ Create a combined data array ###############################
  data=fltarr(9,SWi+LWi)		; [B#,U#,Xu,Yu,B,U,type,Berr,Uerr]
  di=0L


; ############ Get Calibration Star Info ##################################
  if (CALIBFILE ne "") then begin
    openr,5,CALIBFILE
    cal=fltarr(6,100) & tmpc=fltarr(4) & i1=0
    ; cal = [x,y,true LW counts,true SW counts,LWcts,SWcts from fit]
    while not EOF(5) do begin
      readf,5,tmpc
      cal(0:3,i1)=tmpc & i1=i1+1
      endwhile
    cal=cal(*,0:i1-1)
    close,5
    endif


; #### If SELECTCIRCLE==1 prepare a distance array ==
  if (SELECTCIRCLE eq 1) then $
    rdist=sqrt((SWdata(1,*)-CIRCLECENX)^2 + (SWdata(2,*)-CIRCLECENY)^2)


; ############ Matching Loop ##############################################
  print,'Dumping match information to diagram.dmp...'
  if (DontWrite eq 0) then openw,1,DUMPFILE
  t=0 & nmat=0 & matches=intarr(2,MAXSTARS)

  for i=0,SWi-1 do begin
    SWx=SWdata(1,i) & SWy=SWdata(2,i)
    match=0


; #### If CLIPEDGE==1 and the star is outside allowed region then ignore ==
    if (CLIPEDGE eq 1) then begin
      if (SWx>SWy<CLIPMAX eq CLIPMAX) or (SWx<SWy>CLIPMIN eq CLIPMIN) then begin
        if (DontWrite eq 0) then printf,1,strn(i), $
          ': Outside clip edge.  Excluded.'
        match=-2
        endif
      endif


; #### If SELECTCIRCLE==1 and the star is outside allowed region then ignore ==
    if (SELECTCIRCLE eq 1) then begin
      if (rdist(i)*SELECTCIRCLE gt CIRCLERAD*SELECTCIRCLE) then begin
        if (DontWrite eq 0) then printf,1,strn(i), $
          ': Outside circular selection paramter.  Excluded.'
        match=-2
        endif
      endif


; #### If we haven't tossed out the object yet, then find the best matches ==
    if (match(0) ge 0) then begin
      minsrch=LWlookup2(SWy-3>0<(nindex-1))
      maxsrch=LWlookup2(SWy+3>0<(nindex-1))
      tmp=sqrt((LWdata(1,minsrch:maxsrch)+LWXadj-SWx)^2+ $
        (LWdata(2,minsrch:maxsrch)+LWYadj-SWy)^2)
      match=where(tmp lt MATCHRAD)
      endif


; #### If there are no matches, let's assign an upper limit if desired =====
    if (match(0) eq -1) and (LWUPPERLIM ne 0) and $
      ((SWdata(5,i) eq 1) or (ONETYPEONE ne 1)) then begin
      data(*,di)=[-1,SWdata(0,i),SWx,SWy,1,SWdata(3,i),6,.01,SWdata(4,i)]
      t=t+1 & di=di+1
      if (NoMatchDump eq 0) then $
        printf,1,strn(i),': No match found within ',strn(MATCHRAD),' pixels'
      endif


; #### If there is a match, process it =====
    if (match(0) ge 0) then begin
      match=match+minsrch
      if (n_elements(match) gt 1) then begin
        match=match(sort(LWdata(5,match)))
        endif
      matches(*,t)=[i,match(0)]
      data(*,di)=[LWdata(0,match(0)),SWdata(0,i),SWx,SWy,LWdata(3,match(0)), $
        SWdata(3,i),0,LWdata(4,match(0)),SWdata(4,i)]

      ty=1
      if (SWdata(5,i) eq 1) and (LWdata(5,match(0)) ne 1) then ty=2
      if (SWdata(5,i) ne 1) and (LWdata(5,match(0)) eq 1) then ty=3
      if (SWdata(5,i) ne 1) and (LWdata(5,match(0)) ne 1) then ty=4
      data(6,di)=ty
      di=di+1
      LWdata(5,match(0))=-LWdata(5,match(0))

      if (NoMatchDump eq 0) then $
        printf,1,strn(i),': ',SWLETTER,vect([fix(SWdata(0,i))]),' matches ', $
                              LWLETTER,vect([fix(LWdata(0,match(0)))]),' to ', $
        strn(tmp(match(0)-minsrch),format='(f10.3)'),$
        vect([SWdata(1,i)-LWdata(1,match(0)),SWdata(2,i)-LWdata(2,match(0))], $
        format='(f10.3)'),' pixels ', $
        '(best of ',strn(n_elements(match)),')'

      if (CALIBFILE ne "") then begin
        tmp=sqrt((SWx-cal(0,*)-0.5)^2+(SWy-cal(1,*)-0.5)^2)
;        tmp=sqrt((SWx-cal(0,*))^2+(SWy-cal(1,*))^2)
        mat=where(tmp lt MATCHRAD)
        if (mat(0) ge 0) then begin
          mat=mat(0)
          cal(4,mat)=LWdata(3,match(0))
          cal(5,mat)=SWdata(3,i)
          print,'Calib star ',strn(mat),' SW, LW: ',cal(3,mat)/cal(5,mat), $
            cal(2,mat)/cal(4,mat)
          if (data(6,di-1) ne 1) then $
            print,'  WARNING: previous calib star not both type 1 !!'
          data(6,di-1)=5
          endif
        endif

      t=t+1 & nmat=nmat+1
    endif

    if (i/300 eq i/300.) then print,strn(i+1),' stars compared... ',strn(nmat),' matches...'

    endfor
  close,1
  print,strn(i),' stars compared... ',strn(nmat),' matches...'


; ############# Add the points which are missing in SW if desired ##########
  tmp1=where(LWdata(5,*) gt 0)
  match=0
  if (tmp1(0) ne -1) and (SWUPPERLIM ne 0) then begin
    for i=0,n_elements(tmp1)-1 do begin
      if (CLIPEDGE eq 1) then begin
        match=0
        LWx=LWdata(1,tmp1(i)) & LWy=LWdata(2,tmp1(i))
        if (LWx>LWy<(CLIPMAX-1) eq CLIPMAX-1) or $
          (LWx<LWy>(CLIPMIN+1) eq CLIPMIN+1) then match=-2
        endif
      if (SELECTCIRCLE eq 1) then begin
        match=0
        rdist=sqrt((LWdata(1,tmp1(i))-CIRCLECENX)^2 + (LWdata(2,tmp1(i))-CIRCLECENY)^2)
        if (rdist*SELECTCIRCLE gt CIRCLERAD*SELECTCIRCLE) then match=-2
        endif
      if (match eq 0) and ((LWdata(5,i) eq 1) or (ONETYPEONE ne 1)) then begin
        data(*,di)=[LWdata(0,tmp1(i)),-1,LWdata(1:3,tmp1(i)),SWUPPERLIM,7,$
          LWdata(4,tmp1(i)),.01]
        di=di+1
        endif
      endfor
    endif


; ############# Trim the final data array and keep going ####################
  data=data(*,0:di-1)


; ############# Print out the average calibration star info #################
  if (CALIBFILE ne "") then begin
    good=where(cal(5,*) ne 0)
    print,'Mean calib SW, LW:          ',avg(cal(3,good)/cal(5,good)), $
      avg(cal(2,good)/cal(4,good))
    print,'Median calib SW, LW:        ',median(cal(3,good)/cal(5,good)), $
      median(cal(2,good)/cal(4,good))
    print,'stdev calib SW, LW:         ',stdev(cal(3,good)/cal(5,good)), $
      stdev(cal(2,good)/cal(4,good))
    endif


; ############# Apply the calibration parameters ############################
  LWmag=flux2mag(data(4,*)/GAINCORR/LWEXPTIME*LWAPERCORR,0)+LWZEROPT
  SWmag=flux2mag(data(5,*)/GAINCORR/SWEXPTIME*SWAPERCORR,0)+SWZEROPT

  if (CTECORR ne -1) then begin
    LWmag=LWmag-(data(3,*)+CTECORR)*0.04/800
    SWmag=SWmag-(data(3,*)+CTECORR)*0.04/800
    endif

  tmp1=where(data(0,*) lt 0)
  if (tmp1(0) ne -1) then begin
    LWmag(tmp1)=LWUPPERLIM
    print,strn(n_elements(tmp1)),' objects using an B upper limit of ', $
      strn(LWUPPERLIM)
    endif

  tmp1=where(data(1,*) lt 0)
  if (tmp1(0) ne -1) then begin
    SWmag(tmp1)=SWUPPERLIM
    print,strn(n_elements(tmp1)),' objects using an U upper limit of ', $
      strn(SWUPPERLIM)
    endif

  type=data(6,*)


; ############## Subtract out another (control) CM diagram ##############
  if (SUBTRACTFILE ne '') then begin
    sub=fltarr(3,MAXSTARS) & lin=''  & Si=0
    print,'Reading '+SUBTRACTFILE
    openr,1,SUBTRACTFILE & readf,1,lin & readf,1,lin
    while not EOF(1) do begin
      readf,1,lin
      reads,lin,n1,n2,n3,x1,y1,B1,U1,UmB
      sub(*,Si)=[B1,UmB,0]
      Si=Si+1
      endwhile
    close,1
    sub=sub(*,0:Si-1)
    SWmLW=(SWmag-LWmag)
    print,'Subtracting out stars from '+SUBTRACTFILE
    while (SUBTRACTTIMES gt 0) do begin
      if (SUBTRACTTIMES ge 1) then skipfac=1 $
        else skipfac=1.0/SUBTRACTTIMES
      for i=0.0,Si-1,skipfac do begin
	dist=sqrt( (sub(0,i)-LWmag)^2 + (sub(1,i)-SWmLW)^2 )
	srt=sort(dist) & j=0
	while (type(srt(j)) eq 0) do j=j+1
	type(srt(j))=0
	print,'(nth best, distance: ',j,dist(srt(j))
	endfor
      if (SUBTRACTTIMES ge 1) then SUBTRACTTIMES=SUBTRACTTIMES-1 $
        else SUBTRACTTIMES=0
      endwhile
    endif


; ############## Set up the plot ###################################

  !x.charsize=1.5 & !y.charsize=1.5
  !x.thick=PS*2+1 & !y.thick=PS*2+1
  plotsym,0,/fill			; symbol 8 is filled circles

  plot,SWmag-LWmag,SWmag,yrange=[YMIN,YMAX],ysty=1, $
    xrange=[XMIN,XMAX],xsty=1,xminor=4,yminor=4, $
    title=MTITLE,xtit=XTITLE,ytitle=YTITLE,/nodata
    
  if (DontWrite eq 0) then begin
    openw,1,OUTFILE
    printf,1,'Plot #   ',LWLETTER,' #    ',SWLETTER,' #      X   (',SWLETTER, $
      ')  Y       ',LWLETTER,'       ',SWLETTER,'      ',SWLETTER,'-',LWLETTER,'   Sym'
    printf,1,'------  -----  -----  -------  -------  ------  ------  ------  --'
    endif

; ############## Plotting Loop ###################################
  errarr=fltarr(5,n_elements(type)) & errctr=0 & pltcnt=0
  for i=0,n_elements(type)-1 do begin

    sym1=([9,PLOTSYM,4,6,7,2,1,1])(type(i))
    if (type(i) eq 5) and (PLOTCAL eq 0) then begin
      print,'Not plotting Calib star B '+strn(fix(data(0,i)))
    endif else begin
      if (sym1 ne 9) and ((sym1 ne 7) or (ONETYPEONE ne 1)) then begin
        plots,[SWmag(i)-LWmag(i)],[LWmag(i)],psym=sym1,symsize=SYMSIZE
        pltcnt=pltcnt+1
        if (DontWrite eq 0) then begin
          printf,1,format='(i6,1x,i6,1x,i6,1x,f8.2,1x,f8.2,3f8.3,i4)',$
            i,data(0:3,i),LWmag(i),SWmag(i),SWmag(i)-LWmag(i),sym1
          endif
        errarr(*,errctr)=[LWmag(i),SWmag(i),flux2mag(data(7,i),0),flux2mag(data(8,i),0),sym1]
        errctr=errctr+1
        endif
      endelse
    endfor
  print,strn(pltcnt),' stars plotted...'


  if (SHOWLEGEND eq 1) then begin
    for i=1,7 do begin
      symp=9-i & if (symp eq 3) then symp=5
      plots,[0.79],[0.305-.02*i],/norm,psym=symp
      xyouts,[0.8],[0.3-.02*i],/norm,'Btype '+strn(i)
      endfor
    endif

  if (PS eq 1) then print,'PostScript Channel still open!  Close with PSCLOSE'

  close,/all

  tmp1=where(LWdata(5,*) lt 0)
  if (tmp1(0) ne -1) then LWdata(5,tmp1)=-LWdata(5,tmp1)

  if (ERRVSMAGPLOT ne 0) then begin
    if (PS eq 0) then begin
      print,'Press any key to continue...'
      key1=get_kbrd(1)
      endif
    errarr=errarr(*,0:errctr-1)
    plot,errarr(0,*),errarr(2,*),psym=1,symsize=.5,max_val=4, $
      xtitle=LWLETTER,ytitle=INPUTMODE+' Error',/nodata
    for i=0,errctr-1 do plots,errarr(0,i),errarr(2,i),psym=errarr(4,i),symsize=SYMSIZE
    if (PS eq 0) then begin
      print,'Press any key to continue...'
      key1=get_kbrd(1)
      endif
    plot,errarr(1,*),errarr(3,*),psym=8,symsize=.5,max_val=4, $
      xtitle=SWLETTER,ytitle=INPUTMODE+' Error',/nodata
    for i=0,errctr-1 do plots,errarr(1,i),errarr(3,i),psym=errarr(4,i),symsize=SYMSIZE
    endif

  return

end

