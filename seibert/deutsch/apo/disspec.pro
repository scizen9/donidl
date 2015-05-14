;+
; No formal header yet.  See the documentation in
;   http//www.astro.washington.edu/deutsch/apoinfo.html
;-

pro fitskyline,xax,skyspec,fitskyline1
  if (n_elements(xax) gt 790) then begin
    refline=5577.
    refline=7244.9
    refline=8827.0
    refline=9376.0
    refline=5892.
    refline=6300.3
    refline=5577.0
    tmp1=refline-xax
  endif else begin
    refline=5577.0
    tmp1=xax-refline
    endelse
  xaxorig=xax
  width=15
  stdev=!d.name

TRYAGAIN:
  tmp2=where(tmp1 gt 0)
  cen=tmp2(0)

  if (cen lt 30) or (cen gt n_elements(xax)-30) then begin
    print,'FITSKYLINE: out of range'
    return
    endif
  xax1=xaxorig(cen-width:cen+width)
  sky1=skyspec(cen-width:cen+width)

  if (fitskyline1 eq 1) then fit=gaussfit(xax1,sky1,coeff) $
    else coeff=[0,fitskyline1+refline]
  print,'FITSKYLINE: shifting X-axis ',strn(coeff(1)-refline),' Angstroms'

  xax=xaxorig-(coeff(1)-refline)

  if (fitskyline1 eq 1) then begin
    if (!d.name ne 'X') then set_plot,'X'
    plot,xax1,sky1,title='Fit to '+strn(refline),xsty=1
    oplot,xax1,fit
    print,'Press any key to continue, or hit r to retry on a feature,'
    print,'m to manually enter a shift'
    key1=get_kbrd(1)
    if (strupcase(key1) eq 'R') then begin
      print,'Click on feature with mouse...'
      cursor,x,y,/data
      width=8
      if (n_elements(xax) gt 790) then tmp1=x-xaxorig $
      else tmp1=xaxorig-x
      goto,TRYAGAIN
      endif
    if (strupcase(key1) eq 'M') then begin
      fitskyline1=1.0
      read,'Enter new value to offset: ',fitskyline1
      goto,TRYAGAIN
      endif
    endif

    if (stdev ne !d.name) then set_plot,stdev

  return
end


; =============================================================================

pro disspec,img,hdr,x,peak=peak,total=tot,wcal=wcal,step=step1,yrange=yrange,$
            smooth=smth,rtnspec=rtnspec,xrange=xrange,fitskyline=fitskyline, $
            noise=noise,skyspec=skyspecflag,replot=replot, $
            extracmeth=extracmeth,crashearly=crashearly

  COMMON DISFUDGE,forceangle

  if (n_params(0) lt 2) then begin
    print,"Call> disspec,image,header,xcen,[/peak,/total,/wcal,/step,yrange=,smooth=,rtnspec=]"
    print,""
    print,"e.g.> disread,img,h,'/host/bluemoon/scr/tmp/deutsch/n3/n3.0002b'
    print,"e.g.> disspec,img,h,279,/peak,/wcal"
    print,"e.g.> disspec,img,h,279,/tot,/wcal"
    print,""
    return
    endif


  if (n_elements(peak) eq 0) then peak=0
  if (n_elements(tot) eq 0) then tot=0
  if (n_elements(skyspecflag) eq 0) then skyspecflag=0
  if (n_elements(wcal) eq 0) then wcal=0
  if (n_elements(step1) eq 0) then step1=0
  if (n_elements(yrange) eq 0) then yrange=0
  if (n_elements(smth) eq 0) then smth=0
  if (n_elements(fitskyline) eq 0) then fitskyline=0
  if (n_elements(noise) eq 0) then noise=0
  if (n_elements(replot) eq 0) then replot=0
  if (n_elements(extracmeth) eq 0) then extracmeth=0
  if (n_elements(crashearly) eq 0) then crashearly=0

  if (n_elements(forceangle) ne 2) then forceangle=[-999,-999]

  if (peak+tot eq 0) then peak=1


; ----------------------------------------------------------------------------

  disinfo,img,hdr,inf
  ngoodrows=inf.NAXIS2-inf.topbadrows-inf.botbadrows
  goodrows=indgen(ngoodrows)+inf.botbadrows

  specstrip=img(x-20:x+20,goodrows)
  skystrip=specstrip(0:30,*) & skystrip(15:30,*)=specstrip(25:40,*)
  len=(size(specstrip))(2)
  skyspec=fltarr(len) & skyrms=skyspec
  maxpos=skyspec & maxval=skyspec & specwid=skyspec
  maxspec=skyspec & totspec=maxspec


  for i=0,len-1 do begin
    strp=skystrip(*,i)
    tmp1=median(strp)
    good=where(strp-tmp1 lt sqrt(tmp1>1)>(15*3))
    if (good(0) eq -1) then good=where(strp ne 999)
    avsky=avg(strp(good)) & avskyrms=stdev(strp(good))
    skyspec(i)=avsky & skyrms(i)=avskyrms
    strp=specstrip(*,i)-avsky
    cstrp=strp(15:25)
    maxpos(i)=(where(cstrp eq max(cstrp)))(0)+15
    maxspec(i)=max(cstrp)
    bsky=where(strp lt 2*sqrt(tmp1>1)>(15*2))-maxpos(i)
    pos1=where(bsky gt 0) & neg1=where(-bsky gt 0)
    if (pos1(0)<neg1(0) ge 0) then $
      specwid(i)=bsky(pos1(0))-bsky(neg1(n_elements(neg1)-1))-1
    endfor


  maxval=median(maxspec,9)
  medv=median(skyrms)
  bad=where(skyrms gt 2*medv)
  if (bad(0) ne -1) then maxval(bad)=0
  coeff=polyfitw(indgen(len),maxpos,maxval,1,trace)
  specangle=atan(-coeff(1))*!radeg


  if (inf.chip eq 'RED') then cam=1 else cam=0
  if (forceangle(cam) ne -999) then begin
    specangle=forceangle(cam)
    print,'Forcing spectrum trace angle=',specangle
    endif
  print,'Spectrum angle is ',strn(specangle), $
    ' for length ',strn(n_elements(maxval))


; ----------------------------------------------------------------------------

  exwid=(specwid((sort(specwid))(len*.95))>1)*1.0
  exwid=fix(exwid/2)*2+1.0
  if (exwid lt 5) and (n_elements(maxval) eq 820) then exwid=5.0
  exsize=fix(exwid/2)
;exwid=9.0 & exsize=4
  print,'Extraction width: ',strn(exwid)

  rotstrip=rot(specstrip,specangle,/interp)
  speccol=indgen(exwid)+20-exsize
  skycol=[20-2-exsize-indgen(7),20+2+exsize+indgen(7)]
  for i=0,len-1 do begin
    maxspec(i)=max(specstrip(speccol,i))
    strp=rotstrip(speccol,i)
    skyline,rotstrip(skycol,i),skyv,rmsv
    good=where(rotstrip(skycol,i) lt skyv+3*rmsv)
    skyv=avg((rotstrip(skycol,i))(good))
    totspec(i)=total(strp)-skyv*(exsize*2+1)
    skyspec(i)=skyv
    endfor


; ----------------------------------------------------------------------------

  xax=indgen(len)
  if (inf.chip eq 'RED') then xax=reverse(xax)

  if (wcal eq 1) then begin
    if (inf.chip eq 'RED') then begin
      if (inf.grating eq 'low') then begin
        angperpix=6.986
;        xax=xax*angperpix + inf.glambda - 406*angperpix
        xax=xax*angperpix + inf.glambda - 402*angperpix
        c=[-22.789356,0.015251082,-2.8178986e-06,1.5371504e-10]
        xax=xax+c(0)+c(1)*xax+c(2)*xax^2+c(3)*xax^3
        if (fitskyline ne 0) then fitskyline,xax,skyspec,fitskyline
        endif
      if (inf.grating eq 'high') then begin
        angperpix=1.29952
        xax=xax*angperpix + inf.glambda - 458.077*angperpix
;	  good to ~0.1 ang. 2nd order might be better..
        if (fitskyline ne 0) then fitskyline,xax,skyspec,fitskyline
        endif
      endif
    if (inf.chip eq 'BLUE') then begin
      if (inf.grating eq 'low') then begin
        angperpix=6.2652		; perfect above 4500 Ang. 1 Ang error at 4000
;        xax=xax*angperpix + inf.glambda - 245*angperpix
        xax=xax*angperpix + inf.glambda - 260*angperpix
        if (fitskyline ne 0) then fitskyline,xax,skyspec,fitskyline
        endif
      if (inf.grating eq 'high') then begin
        angperpix=1.60749		; good to ~0.2 ang. 2nd order might be better..
        xax=xax*angperpix + inf.glambda - 217.062*angperpix
        if (fitskyline ne 0) then fitskyline,xax,skyspec,fitskyline
        endif
      endif

    endif

; ----------------------------------------------------------------------------

  if (crashearly ne 0) then stop

  if (!d.name eq 'X') then window,8

  if (n_elements(xrange) ne 2) then xrange=[min(xax),max(xax)]
  rtnspec=fltarr(n_elements(totspec),2)
  rtnspec(0,0)=xax

  if (skyspecflag eq 1) then begin
    peak=1
    maxspec=skyspec
    endif

  if (peak eq 1) then begin
    rtnspec(0,1)=maxspec
    if (n_elements(yrange) eq 2) then $
      plot,xax,maxspec,title='Peak Counts Spectrum',yr=yrange,ysty=1, $
      ytitle='Peak Counts',xrange=xrange,xsty=1 $
    else plot,xax,maxspec,title='Peak Counts Spectrum', $
      ytitle='Peak Counts',xrange=xrange,xsty=1
    endif

  if (tot eq 1) then begin
    if (smth ne 0) then totspec=smooth(totspec,smth)
    rtnspec(0,1)=totspec
    if (n_elements(yrange) ne 2) then yrange=[max(totspec)/(-10),max(totspec)/.95]
    plot,xax,totspec,title='Extracted Sky-subtracted Spectrum',$
    yr=yrange,ysty=1,ytitle='Total Counts',xrange=xrange,xsty=1,pos=[.097,0.12,.97,.94]
    endif

  oplot,[0,20000],[0,0]
  if (noise eq 1) then oplot,xax,skyrms*3

  if (total(xrange-[min(xax),max(xax)]) ne 0) then return

  speccol=indgen(11)+15
  dispstrip=rotstrip(speccol,*)
  if (tot eq 1) then for i=0,10 do dispstrip(i,*)=dispstrip(i,*)-skyspec
  flipangle=3 & if (inf.chip eq 'RED') then flipangle=1
  dispstrip=rotate(dispstrip,flipangle)

  hist1=histogram(dispstrip,min=0,max=30000,binsiz=5)
  tmp1=where(hist1 gt 5) & lim1=max(tmp1)*5/.75
  if (!d.name eq 'X') then $
    tv,congrid(bytscl(dispstrip,-lim1*0.1,lim1,top=(!d.n_colors<256)-2),563,21),60,6
  if (!d.name eq 'PS') then begin
    tmpim=bytarr((size(dispstrip))(1),11+3+2)
    tmpim(*,5:15)=bytscl(dispstrip,-lim1*0.1,lim1)
    endif

  tmp8=skyspec & if (inf.chip eq 'RED') then tmp8=rotate(tmp8,2)
  tmp8=reform(tmp8,n_elements(tmp8),1)
  tmp3=histogram(tmp8,min=0,max=30000,binsiz=5)
  tmp4=where(tmp3 gt 5) & tmp5=max(tmp4)*5/.75
  tmp5=max(tmp8)*.70
  if (!d.name eq 'X') then $
    tv,bytscl(congrid(tmp8,563,5),0,tmp5,top=(!d.n_colors<256)-2),60,0
  if (!d.name eq 'PS') then begin
    for i=0,2 do tmpim(*,i)=bytscl(tmp8,0,tmp5)
    plot,indgen(100),pos=[.088,0,.974,.040],/noerase,xsty=5,ysty=5,/nodata
    device,bits=8
    imgunder,255b-tmpim

    if (replot eq 1) then begin
      plot,xax,maxspec,title='Peak Counts Spectrum',yr=yrange,ysty=1, $
      ytitle='Peak Counts',xrange=xrange,xsty=1,/noerase
      endif

    endif

  return

end
