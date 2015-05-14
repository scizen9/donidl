pro discalsphr,img,h,xcen,xrange=xrange,rtnspec=rtnspec, $
  smooth=smooth1,outfile=outfile,ymax=finalymax,skyspec=skyspec
;+
; No formal header yet.  See the documentation in
;   http//www.astro.washington.edu/deutsch/apoinfo.html
;-

  if (n_params(0) ne 3) then begin
    print,"Call> discalsphr,img,h,xcen"
    print,"e.g.> disread,img,h,'n1.0008b'"
    print,"e.g.> discalsphr,img,h,318"
    return
    endif

  defcaldir='/host/dione/u5/deutsch/work9712/'

; ---------------------------------------------------------------------------

  exptime=sxpar(h,'EXPOSURE')
  airmass=sxpar(h,'AIRMASS')
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

; ---------------------------------------------------------------------------

  if (n_elements(skyspec) ne 1) then skyspec=0

  disinfo,img,h,inf
  if (skyspec eq 1) then disspec,img,h,xcen,/skyspec,/wcal,rtn=spec $
    else disspec,img,h,xcen,/tot,/wcal,rtn=spec
  print,'Press any key....' & key1=get_kbrd(1)

  openr,1,'/astro/iraf/noao/lib/onedstds/kpnoextinct.dat'
  extinc=fltarr(2,81)
  readf,1,extinc & close,1

  calfile='bcal3.dat'
  if (inf.chip eq 'RED') then calfile='rcal3.dat'

  if exist(calfile) then openr,1,calfile $
    else openr,1,defcaldir+calfile
  cal=fltarr(2,1000) & i=0
  while not EOF(1) do begin
    readf,1,w2,f2
    cal(*,i)=[w2,f2] & i=i+1
    endwhile
  close,1 & cal=cal(*,0:i-1)


; ---------------------------------------------------------------------------

  wave=spec(*,0)
  flux1=spec(*,1)
  if (inf.chip eq 'RED') then begin
    wave=reverse(wave) & flux1=reverse(flux1)
    endif

  cal2=spline(cal(0,*),cal(1,*),wave)
  extincfac=interpol(10^(0.4*airmass*extinc(1,*)),extinc(0,*),wave)
  flux2=flux1/exptime/cal2*extincfac
  plot,wave,flux2,xsty=1

  fwave=wave
  fflux=flux2


; ---------------------------------------------------------------------------

  fname=strn(sxpar(h,'IMTITLE'))
  object=strn(sxpar(h,'OBJECT'))
  name=object+' ('+fname+')'

  if (n_elements(xrange) ne 2) then xrange=[fwave(0),fwave(n_elements(fwave)-1)]
  if (n_elements(finalymax) eq 0) then ymax=max(fflux)*1.05 $
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


  xyouts,.80,.92,/norm,'ExpTime='+strn(fix(exptime))+'s'
  xyouts,.80,.89,/norm,'Airmass='+strn(airmass,format='(f10.3)')
  xyouts,.80,.86,/norm,'DateObs='+strn(sxpar(h,'DATE-OBS'))
  xyouts,.80,.83,/norm,'UT='+strn(sxpar(h,'UT'))


  rtnspec=fltarr(2,n_elements(fflux))
  rtnspec(0,*)=fwave
  rtnspec(1,*)=fflux

  if (n_elements(outfile) ne 0) then begin
    wl2=indgen((10000-3700)/5+1)*5+3700.0
    sp2=interpol(fflux,fwave,wl2)
    h1=h
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
