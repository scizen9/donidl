pro dismkspcal,rtnspec,calfile,h
;+
; No formal header yet.  See the documentation in
;   http//www.astro.washington.edu/deutsch/apoinfo.html
;-

  if (n_params(0) ne 3) then begin
    print,"Call> dismkspcal,rtnspec,calfile"
    print,"e.g.> calfile='/astro/iraf/noao/lib/onedstds/spec50cal/hilt600.dat'"
    print,"e.g.> disread,img,h,'n1.0015r,/spflat"
    print,"e.g.> disspec,img,h,420,/tot,/wcal,/fitskyline,rtn=rtnspec"
    print,"e.g.> dismkspcal,rtnspec,calfile,h"
    return
    endif

  exptime=sxpar(h,'EXPOSURE')
  airmass=sxpar(h,'AIRMASS')

  openr,1,calfile
  wave2=fltarr(1000) & mag2=fltarr(1000) & i=0 & lin=''
  readf,1,lin & print,lin
  while not EOF(1) do begin
    readf,1,w2,f2,d2
    wave2(i)=w2 & mag2(i)=f2 & i=i+1
    endwhile
  close,1

  wave2=wave2(0:i-1) & mag2=mag2(0:i-1)
  flux21=10^((mag2+48.59d)/(-2.5))
  nAng=wave2*0+50
  nHz=(3d18/(wave2-25)-3d18/(wave2+25))
  flux2=flux21*nHz/nAng

; ----------------------------------------------------------------------------

  openr,1,'/astro/iraf/noao/lib/onedstds/kpnoextinct.dat'
  extinc=fltarr(2,81)
  readf,1,extinc & close,1

; ----------------------------------------------------------------------------
  s=size(rtnspec)
  if (s(1) gt 790) then goto,REDPART

; ----------------------------------------------------------------------------

  wave1=rtnspec(*,0)
  cnts1=rtnspec(*,1)/exptime

  plot,wave1,cnts1/max(cnts1),yr=[-0.05,1.05],ysty=1
  oplot,wave2,flux2/max(flux2)
  print,'Press any key....' & key=get_kbrd(1)

  minwl=fix(min(wave1)/50)*50.0>3300-25 & maxwl=fix(max(wave1)/50)*50.0+25

  wave5=findgen(maxwl-minwl+1)+minwl
  cnts5=interpol(cnts1,wave1,wave5)

  extincfac=interpol(10^(0.4*airmass*extinc(1,*)),extinc(0,*),wave5)
  cnts5=cnts5*extincfac

  wave6=fltarr(n_elements(wave5)/50-1) & cal6=wave6
  for i=0,n_elements(wave6)-1 do begin
    rng=where((wave5 ge i*50+minwl) and (wave5 le i*50+minwl+49))
    totc=total(cnts5(rng))/50
    wave6(i)=i*50+minwl+25
    tmp1=where(wave2 eq i*50+minwl+25)
    if (tmp1(0) ne -1) then cal6(i)=totc/flux2(tmp1)
    endfor

  plot,wave6,cal6/max(cal6),psym=-4
  oplot,wave2,flux2/max(flux2)
  print,'Press any key....' & key=get_kbrd(1)

  print,'Writing bcal.dat....'
  openw,1,'bcal.dat'
  for i=0,n_elements(wave6)-1 do begin
    if (cal6(i) ne 0) then printf,1,wave6(i),cal6(i)
    endfor
  close,1

  return

; ----------------------------------------------------------------------------

REDPART:

  rev=819-indgen(820)
  wave1=reverse(rtnspec(*,0))
  cnts1=reverse(rtnspec(*,1))/exptime

  plot,wave1,cnts1/max(cnts1),yr=[-0.05,1.05],ysty=1
  oplot,wave2,flux2/max(flux2)
  print,'Press any key....' & key=get_kbrd(1)

  minwl=fix(min(wave1)/50)*50.0>5100-25 & maxwl=fix(max(wave1)/50)*50.0<10200+25

  wave5=findgen(maxwl-minwl+1)+minwl
  cnts5=interpol(cnts1,wave1,wave5)

  extincfac=interpol(10^(0.4*airmass*extinc(1,*)),extinc(0,*),wave5)
  cnts5=cnts5*extincfac

  wave6=fltarr(n_elements(wave5)/50-1) & cal6=wave6
  for i=0,n_elements(wave6)-1 do begin
    rng=where((wave5 ge i*50+minwl) and (wave5 le i*50+minwl+49))
    totc=total(cnts5(rng))/50
    wave6(i)=i*50+minwl+25
    tmp1=where(wave2 eq i*50+minwl+25)
    if (tmp1(0) ne -1) then cal6(i)=totc/flux2(tmp1)
    endfor

  plot,wave6,cal6/max(cal6),psym=-4
  oplot,wave2,flux2/max(flux2)
  print,'Press any key....' & key=get_kbrd(1)

  print,'Writing rcal.dat....'
  openw,1,'rcal.dat'
  for i=0,n_elements(wave6)-1 do begin
    if (cal6(i) ne 0) then printf,1,wave6(i),cal6(i)
    endfor
  close,1

  return

end
