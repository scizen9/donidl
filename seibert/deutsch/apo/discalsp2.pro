pro discalsp2,filename,xRcen,xBcen,xrange=xrange,rtnspec=rtnspec, $
  splicewl=splicewl,smooth=smooth1,outfile=outfile,ymax=finalymax
;+
; No formal header yet.  See the documentation in
;   http//www.astro.washington.edu/deutsch/apoinfo.html
;-

  if (n_params(0) ne 3) then begin
    print,"Call> discalsp,rfilename,xRcen,xBcen"
    print,"e.g.> discalsp,'im0008r',423,304"
    print," Filename must correspond to the RED image"
    return
    endif

  defcaldir='/host/dione/d3/deutsch/apo/dqjun95/idl/'

  tmp1=strpos(filename,'r',strlen(filename)-7)
  if (tmp1 eq -1) then begin
    print," Filename must correspond to the RED image"
    return
    endif

; ---------------------------------------------------------------------------

  disread,imgr,hr,filename,/spflat
  if (n_elements(imgr) lt 1000) then return

  bfile=strmid(filename,0,tmp1)+'b'+strmid(filename,tmp1+1,99)
  disread,imgb,hb,bfile,/spflat
  if (n_elements(imgb) lt 1000) then return

  exptime=sxpar(hr,'EXPOSURE')
  airmass=sxpar(hr,'AIRMASS')
  print,'EXPOSURE=',strn(exptime),'      AIRMASS=',strn(airmass)

; ---------------------------------------------------------------------------

  if (!d.name eq 'PS') then begin	; If Postscript output mode
    !p.font=0					; select hardware fonts
    device,/helv,/isolatin1			; Helvetica ISOLatin fontset
    ang=string(197B)				; Angstrom sym char string
  endif else begin			; If screen or other output mode
    !p.font=-1					; select Hershey fonts
    xyouts,0,0,/norm,'!17'			; Set to Triplex Roman font
    ang='!3'+string(197b)+'!X'			; only Simplex Angstrom
    endelse

;  disspec,imgr,hr,xRcen,/skyspec,/wcal,/fitskyline,rtn=specr
  disspec,imgr,hr,xRcen,/tot,/wcal,/fitskyline,rtn=specr
  print,'Press any key....' & key1=get_kbrd(1)

;  disspec,imgb,hb,xBcen,/skyspec,/wcal,/fitskyline,rtn=specb
  disspec,imgb,hb,xBcen,/tot,/wcal,/fitskyline,rtn=specb
  print,'Press any key....' & key1=get_kbrd(1)

; ---------------------------------------------------------------------------

  openr,1,'/astro/iraf/noao/lib/onedstds/kpnoextinct.dat'
  extinc=fltarr(2,81)
  readf,1,extinc & close,1

  if exist('bcal2.dat') then openr,1,'bcal2.dat' $
    else openr,1,defcaldir+'bcal2.dat'
  bcal=fltarr(2,1000) & i=0
  while not EOF(1) do begin
    readf,1,w2,f2
    bcal(0,i)=w2 & bcal(1,i)=f2 & i=i+1
    endwhile
  close,1 & bcal=bcal(*,0:i-1)

  if exist('rcal2.dat') then openr,1,'rcal2.dat' $
    else openr,1,defcaldir+'rcal2.dat'
  rcal=fltarr(2,1000) & i=0
  while not EOF(1) do begin
    readf,1,w2,f2
    rcal(0,i)=w2 & rcal(1,i)=f2 & i=i+1
    endwhile
  close,1 & rcal=rcal(*,0:i-1)

; ---------------------------------------------------------------------------

  waveb=specb(*,0)
  bflux1=specb(*,1)
  cal1=bcal(1,*)/max(bcal(1,*))
  cal1=cal1>0.01
  cal2=spline(bcal(0,*),cal1,waveb)
  extincfac=interpol(10^(0.4*airmass*extinc(1,*)),extinc(0,*),waveb)
  bflux2=bflux1/exptime/cal2*extincfac

;  plot,bcal(0,*),cal1,psym=-4
;  oplot,waveb,result,color=20
;  plot,waveb,bflux2,xr=[3800,6000]

;stop

  waver=reverse(specr(*,0))
  rflux1=reverse(specr(*,1))
  cal1=rcal(1,*)/max(rcal(1,*))
  cal1=cal1>0.01
  wcal=indgen((rcal(0,(size(rcal))(2)-1)-rcal(0,0))/50+1)*50+rcal(0,0)
  cal11=interpol(cal1,rcal(0,*),wcal)
  cal2=spline(wcal,cal11,waver)
  extincfac=interpol(10^(0.4*airmass*extinc(1,*)),extinc(0,*),waver)
  rflux2=rflux1/exptime/cal2*extincfac
;  plot,waver,rflux2,xr=[5300,10000],xsty=1

  print,avg(bflux1(300:320))/exptime

; ---------------------------------------------------------------------------

  bflux2=bflux2/max(bcal(1,*))
  rflux2=rflux2/max(rcal(1,*))

; ---------------------------------------------------------------------------

  if (n_elements(splicewl) eq 0) then begin
    splrng=(where(waveb gt 5000))(0)
    ymax=max(bflux2(splrng:splrng+80))*2
    plot,waveb,bflux2>0,xr=[5000,6000],xsty=1,yr=[0,ymax],ysty=1
    oplot,waver,rflux2,color=30
    print,'Click mouse cursor as desired splicing wavelength...'
    cursor,splicewl,y,/data & print,splicewl
    endif

  tmp1=where(waveb gt splicewl)
  tmp2=where(waver gt splicewl)

  fwave=[waveb(0:tmp1(0)-1),waver(tmp2(0):819)]
  fflux=[bflux2(0:tmp1(0)-1),rflux2(tmp2(0):819)]
  if (n_elements(smooth1) ne 0) then fflux=smooth(fflux,smooth1)

; ---------------------------------------------------------------------------

  if (n_elements(xrange) ne 2) then xrange=[3700,10000]

  fstart=-1
  for i=0,strlen(filename)-1 do begin
    tmp1=strpos(filename,'/',i)
    if (tmp1 ne -1) then fstart=tmp1
    endfor
  fname=strmid(filename,fstart+1,30) & len1=strlen(fname)
  if (strmid(fname,len1-4,4) eq '.hhh') then fname=strmid(fname,0,len1-4)
  object=strn(sxpar(hr,'OBJECT'))
  name=object+' ('+fname+')'

  tmp1=where((fwave gt xrange(0)>3800) and (fwave lt xrange(1)-300))
  if (n_elements(finalymax) eq 0) then ymax=max(fflux(tmp1))*1.05 $
    else ymax=finalymax
  subfac=0
TRYAGAIN2:
  subfac=subfac+1
  exp1=fix(alog10(ymax))-subfac & strexp1=strn(exp1)
  fac=10d^(exp1)
  if (ymax/fac lt 3 ) then goto,TRYAGAIN2

  ytitle='F!D!9l!X!N (10!U'+strexp1+'!N erg cm!U-2!N s!U-1!N '+ang+'!U-1!N)'
  print,'exp=',strexp1

  plot,fwave,fflux/fac>0,xr=xrange,yr=[0,ymax/fac],xsty=1,ysty=1, $
    title='Calibrated Spectrum - '+name, $
    xtitle='Wavelength ('+ang+'ngstroms)',ytitle=ytitle

  tmp1=where(fwave gt splicewl)
  oplot,[splicewl,splicewl],[0,fflux(tmp1(0))/fac*.7],linesty=1

  xyouts,.80,.92,/norm,'ExpTime='+strn(fix(exptime))+'s'
  xyouts,.80,.89,/norm,'Airmass='+strn(airmass,format='(f10.3)')
  xyouts,.80,.86,/norm,'DateObs='+strn(sxpar(hr,'DATE-OBS'))
  xyouts,.80,.83,/norm,'UT='+strn(sxpar(hr,'UT'))

  rtnspec=fltarr(2,n_elements(fflux))
  rtnspec(0,*)=fwave
  rtnspec(1,*)=fflux

  if (n_elements(outfile) ne 0) then begin
    wl2=indgen((10000-3700)/5+1)*5+3700.0
    sp2=interpol(fflux,fwave,wl2)
    h1=hr
    sxaddpar,h1,'CTYPE1','LINEAR'
    sxaddpar,h1,'CRVAL1',3700.0
    sxaddpar,h1,'CRPIX1',1.0
    sxaddpar,h1,'CDELT1',5.0
    sxaddpar,h1,'CD1_1',5.0
    sxdelpar,h1,'CTYPE2'
    sxdelpar,h1,'CRVAL2'
    sxdelpar,h1,'CRPIX2'
    sxdelpar,h1,'CDELT2'
    stwrt,sp2,h1,outfile,/sdas
    endif

  return

end
