pro dismkspcal3,rtnspec,calfile,h

;+
; No formal header yet.  See the documentation in
;   http//www.astro.washington.edu/deutsch/apoinfo.html
;-

  if (n_params(0) ne 3) then begin
    print,"Call> dismkspcal3,rtnspec,calfile"
    print,"e.g.> calfile=['/host/dione/d3/deutsch/apo/hstcal/feige67_001.tab',$"
    print,"e.g.>          '/astro/iraf/noao/lib/onedstds/spec50cal/feige67.dat']"
    print,"e.g.> disread,img,h,'n1.0026r,/spflat"
    print,"e.g.> disspec,img,h,420,/tot,/wcal,rtn=rtnspec"
    print,"e.g.> dismkspcal3,rtnspec,calfile,h"
    return
    endif

  exptime=sxpar(h,'EXPOSURE')
  airmass=sxpar(h,'AIRMASS')

; ----------------------------------------------------------------------------

  tab_read,calfile(0),tcb,table,hdr
  refwave1=tab_val(tcb,table,1)
  refflux1=tab_val(tcb,table,2)
  tmp1=(where(refwave1 gt 9000))(0)
  refwave1=[refwave1,11000]
  refflux1=[refflux1,refflux1(tmp1)/2.5]		; wild extrapolation!

; ----------------------------------------------------------------------------

  openr,1,'/astro/iraf/noao/lib/onedstds/kpnoextinct.dat'
  extinc=fltarr(2,81)
  readf,1,extinc & close,1

; ----------------------------------------------------------------------------
  s=size(rtnspec)
  if (s(1) gt 790) then goto,REDPART

; ----------------------------------------------------------------------------

  specwave1=rtnspec(*,0)
  speccnts1=rtnspec(*,1)/exptime

  plot,specwave1,speccnts1/max(speccnts1),yr=[-0.05,1.05],ysty=1,xsty=1
  oplot,refwave1,refflux1/(refflux1((where(refwave1 gt !x.crange(0)))(0)))
  print,'Press any key....' & key=get_kbrd(1)

  extincfac=interpol(10^(0.4*airmass*extinc(1,*)),extinc(0,*),specwave1)
  speccnts1=speccnts1*extincfac

  refflux2=interpol(refflux1,refwave1,specwave1)
  cal1=speccnts1/refflux2

  plot,specwave1,refflux2,xsty=1
  oplot,specwave1,speccnts1*5e-16,color=40
  contwls=[specwave1(3),4378,findgen((4816-4378)/40)*40+4378+40,4816,4890, $
    findgen((5118-4890)/40)*40+4890+40,specwave1(n_elements(specwave1)-4)]
  for i=0,n_elements(contwls)-1 do oplot,[1,1]*contwls(i),[0,1e-10],color=20
  print,'Press any key....' & key=get_kbrd(1)


  plot,specwave1,cal1,xsty=1
  oplot,specwave1,speccnts1*1e13,color=!d.n_colors-2

  contcals=contwls*0
  for i=0,n_elements(contwls)-1 do begin
    oplot,[1,1]*contwls(i),[0,1e18],color=20
    refel=(where(specwave1 ge contwls(i)))(0)
    contcals(i)=median(cal1(refel-2:refel+2))
    endfor
  oplot,contwls,contcals,psym=1,color=40,symsize=3

  minwl=contwls(0) & maxwl=contwls(n_elements(contwls)-1)
  minel=(where(specwave1 ge minwl))(0)
  maxel=(where(specwave1 ge maxwl))(0)

  quadterp,contwls,contcals,specwave1(minel:maxel),yterp
  oplot,specwave1(minel:maxel),yterp,color=20

  cal1(minel:maxel)=yterp
  cal2=abs(smooth(cal1,5))
  plot,specwave1,cal2,xsty=1

  print,'Writing bcal3.dat....'
  openw,1,'bcal2.dat'
  for i=0,n_elements(specwave1)-1 do begin
    if (cal2(i) ne 0) then printf,1,specwave1(i),cal2(i)
    endfor
  close,1

  return

; ----------------------------------------------------------------------------

REDPART:

  specwave1=reverse(rtnspec(*,0))
  speccnts1=reverse(rtnspec(*,1))/exptime

  plot,specwave1,speccnts1/max(speccnts1),yr=[-0.05,1.05],ysty=1,xsty=1
  oplot,refwave1,refflux1/(refflux1((where(refwave1 gt !x.crange(0)))(0)))
  print,'Press any key....' & key=get_kbrd(1)

  extincfac=interpol(10^(0.4*airmass*extinc(1,*)),extinc(0,*),specwave1)
  speccnts1=speccnts1*extincfac

  refflux2=interpol(refflux1,refwave1,specwave1)
  cal1=speccnts1/refflux2
  plot,specwave1,cal1,xsty=1

  plot,specwave1,refflux2,xsty=1
  oplot,specwave1,speccnts1*10e-16,color=40
  contwls=[specwave1(8),5925,findgen((6520-5925)/30)*30+5925+40,6520, $
    6633,findgen((6856-6633)/30)*30+6633+30,6856,specwave1(n_elements(specwave1)-4)]
  for i=0,n_elements(contwls)-1 do oplot,[1,1]*contwls(i),[0,1e-10],color=20
  print,'Press any key....' & key=get_kbrd(1)


  plot,specwave1,cal1,xsty=1
  oplot,specwave1,speccnts1*0.7e13,color=!d.n_colors-2

  contcals=contwls*0
  for i=0,n_elements(contwls)-1 do begin
    oplot,[1,1]*contwls(i),[0,1e18],color=20
    refel=(where(specwave1 ge contwls(i)))(0)
    contcals(i)=median(cal1(refel-2:refel+2))
    endfor
  oplot,contwls,contcals,psym=1,color=40,symsize=3

  minwl=contwls(0) & maxwl=contwls(n_elements(contwls)-1)
  minel=(where(specwave1 ge minwl))(0)
  maxel=(where(specwave1 ge maxwl))(0)

  quadterp,contwls,contcals,specwave1(minel:maxel),yterp
  oplot,specwave1(minel:maxel),yterp,color=20

  cal1(minel:maxel)=yterp
  cal2=abs(smooth(cal1,5))
  plot,specwave1,cal2,xsty=1


  print,'Writing rcal3.dat....'
  openw,1,'rcal2.dat'
  for i=0,n_elements(specwave1)-1 do begin
    if (cal2(i) ne 0) then printf,1,specwave1(i),cal2(i)
    endfor
  close,1


  return

end
