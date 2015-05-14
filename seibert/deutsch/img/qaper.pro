pro qaper,img,xc,yc,cts,cerr,skyv,serr,gain,apers,skyaper,goodvals, $
      exptime=exptime,zpt=zpt,gaincorr=gaincorr,apercor=apercor, $
      photflam=photflam,silent=silent,flux=flux,counts=counts,mags=mags, $
      rawcounts=rawcounts,skyfrac=skyfrac,square=square,noskycalc=noskycalc, $
      rtnmags=rtnmags,rtnmerrs=rtnmerrs

  if (n_params(0) lt 11) then begin
    print,'Call> qaper,img,xc,yc,cts,cerr,skyv,serr,gain,apers,skyaper,goodvals, $'
    print,'        exptime=,zpt=,gaincorr=,apercor=,photflam=,/silent,/flux,/counts,/mags'
    print,'e.g.> qaper,img,xc,yc,cts,cerr,skyv,serr,8.0,[2,3,4],[10,15],[-32700,32700], $'
    print,'        exptime=500,zpt=18.66,againcorr=1.987,percor=[.7,.8,.85],/mags'
    return
    endif

  if (n_elements(flux) eq 0) then flux=0
  if (n_elements(counts) eq 0) then counts=0
  if (n_elements(rawcounts) eq 0) then rawcounts=0
  if (n_elements(mags) eq 0) then mags=0
  if (n_elements(exptime) eq 0) then exptime=1.0
  if (n_elements(gaincorr) eq 0) then gaincorr=1.0
  if (n_elements(zpt) eq 0) then zpt=0.0
  if (n_elements(apercor) eq 0) then apercor=1.0
  if (n_elements(photflam) eq 0) then photflam=1.0
  if (n_elements(silent) eq 0) then silent=0
  if (n_elements(skyfrac) eq 0) then skyfrac=1.0
  if (n_elements(square) eq 0) then square=0
  if (n_elements(noskycalc) eq 0) then noskycalc=0
  if (n_elements(rtnmags) eq 0) then rtnmags=0
  if (n_elements(rtnmerrs) eq 0) then rtnmerrs=0
  testing=0

  ixc=fix(xc) & iyc=fix(yc)

  if (noskycalc ne 0) then begin
    if (n_elements(skyv) eq 0) then skyv=0.0
    if (n_elements(serr) eq 0) then serr=0.0
    if (n_elements(srms) eq 0) then srms=0.0
    print,'     Sky value is ',strn(skyv),' with rms ',strn(srms),' and error ',strn(serr)
    endif


  if (noskycalc eq 0) and (((n_elements(skyaper) eq 2) or (n_elements(skyaper) eq 3))) then begin
;   if (not silent) then print,'(inner,outer) radius=',vect(skyaper)
    if (skyaper(0) lt 3) then begin & print,'skyaper less than 3!' & goto,BAIL & endif
    sz=fix(skyaper(1)+3)
    tmp1=extrac(img,ixc-sz,iyc-sz,2*sz,2*sz)
    dist_circle,mask,2*sz,xc-(ixc-sz),yc-(iyc-sz)

    imsz=size(img)
    absxs=tmp1 & absys=tmp1
    for i=0,2*sz-1 do begin
      absxs(i,*)=ixc-sz+i & absys(*,i)=iyc-sz+i
      endfor
    tmpxx=where((absxs lt 0) or (absxs gt imsz(1)))
    if (tmpxx(0) ne -1) then mask(tmpxx)=-1
    tmpyy=where((absys lt 0) or (absys gt imsz(2)))
    if (tmpyy(0) ne -1) then mask(tmpyy)=-1

    good=where((mask ge skyaper(0)) and (mask le skyaper(1)))
    if (good(0) eq -1) then begin & print,'No elements in the sky!' & goto,BAIL & endif
    if (skyfrac lt 1.0) then begin
      tmp10=tmp1(good)
      srt10=sort(tmp10)
      cutoff=(tmp10(srt10))(n_elements(tmp10)*skyfrac)
      bcut=where(tmp10 le cutoff)
      good=good(bcut)
      endif
    if (n_elements(good) lt 5) then begin
      print,'Less than 5 elements in the sky!' & goto,BAIL & endif
    sky1=tmp1(good) & nsky=n_elements(sky1)
;   if (not silent) then print,strn(nsky),' elements in the sky'
    print,'Sky region avg is ',strn(avg(sky1)),' with rms ', $
      strn(stdev(sky1)),' and error ',strn(stdev(sky1)/sqrt((nsky-1)*1.0))

    skyline,sky1,skyv,srms
    serr=srms/sqrt((nsky-1)*1.0)
    print,'     Sky value is ',strn(skyv),' with rms ',strn(srms),' and error ',strn(serr)

    if (n_elements(skyaper) eq 3) then begin
      skyvarr=fltarr(skyaper(2))
;      usepts=indgen(n_elements(sky1)/skyaper(2))*skyaper(2)
      usepts=indgen(n_elements(sky1)/skyaper(2))
      for i=0,skyaper(2)-1 do begin
;        skyline,sky1(usepts+i),s1,r1
        skyline,sky1(usepts+i*(n_elements(sky1)/skyaper(2))),s1,r1
        print,s1,r1
        skyvarr(i)=s1
        endfor
      skyv=median(skyvarr)
      serr=stdev(skyvarr)
      print,'     Sky value is ',strn(skyv),' with rms ',strn(srms),' and error ',strn(serr)
      endif
    endif


  napers=n_elements(apers)
  cts=fltarr(napers) & cerr=cts & nbad=cts


  if (square eq 0) then begin
    fac=9.0d
    sz=fix(max(apers)+2)
    tmp1=extrac(img,ixc-sz,iyc-sz,2*sz,2*sz)
  ;  tmp2=congrid(tmp1*1d,2*sz*fac,2*sz*fac)		; produces bad results!!!!
    tmp2=rebin(tmp1*1d,2*sz*fac,2*sz*fac,/sample)
    xcen=(xc-(ixc-sz))*fac+fix(fac/2)
    ycen=(yc-(iyc-sz))*fac+fix(fac/2)
    dist_circle,mask,2*sz*fac,xcen,ycen
    if (testing eq 1) then begin
      ex=10.0
      wsz=2*sz*fac*ex
      window,xs=wsz,ys=wsz
      tv,congrid(tmp2/max(tmp2)*80,wsz,wsz)
      for i=0,2*sz*fac-1 do plots,[0,wsz],[i*ex,i*ex],color=150,/device
      for i=0,2*sz*fac-1 do plots,[i*ex,i*ex],[0,wsz],color=150,/device
      endif
    for ap=0,napers-1 do begin
      complete=where(mask le apers(ap)*fac-0.5)
      frac=where((mask gt apers(ap)*fac-0.5) and (mask le apers(ap)*fac+0.5))
      allpix=[complete,frac]
      pixfrac=(0.5-(mask(frac)-apers(ap)*fac))
      if (testing eq 1) then begin
        tmp3=tmp2/max(tmp2)*80
        tmp3(complete)=255
        tmp3(frac)=255*pixfrac
        tv,congrid(tmp3,wsz,wsz)
        for i=0,2*sz*fac-1 do plots,[0,wsz],[i*ex,i*ex],color=150,/device
        for i=0,2*sz*fac-1 do plots,[i*ex,i*ex],[0,wsz],color=150,/device
        tvcircle,apers(ap)*fac*ex,xcen*ex+fix(ex/2),ycen*ex+fix(ex/2),color=50
        endif
      npix=(n_elements(complete)+total(pixfrac))/fac^2
      cts1=(total(tmp2(complete))+total(tmp2(frac)*pixfrac))/fac^2
      bads=where((tmp2(allpix) lt goodvals(0)) or (tmp2(allpix) gt goodvals(1)))
      scts=skyv*npix
      cts(ap)=cts1-scts
      cerr(ap)=sqrt(abs(cts1*gain))/gain
      if (bads(0) ne -1) then begin
        cerr(ap)=cerr(ap)+total(abs(tmp2(allpix(bads))))/fac^2
        nbad(ap)=n_elements(bads)/fac^2
        endif
      cerr(ap)=sqrt(cerr(ap)^2 + (serr*npix)^2)
      if (testing eq 1) then key1=get_kbrd(1)
      endfor
    endif


  if (square eq 1) then begin
    fac=9.0d
    sz=fix(max(apers)+2)
    tmp1=extrac(img,ixc-sz,iyc-sz,2*sz,2*sz)
    tmp2=rebin(tmp1*1d,2*sz*fac,2*sz*fac,/sample)
    xcen=(xc-(ixc-sz))*fac+fix(fac/2)
    ycen=(yc-(iyc-sz))*fac+fix(fac/2)
    maskx=fltarr(2*sz*fac,2*sz*fac) & masky=maskx
    for i=0,2*sz*fac-1 do begin
      maskx(i,*)=i-xcen
      masky(*,i)=i-ycen
      endfor
    if (testing eq 1) then begin
      ex=10.0
      wsz=2*sz*fac*ex
      window,xs=wsz,ys=wsz
      tv,congrid(tmp2/max(tmp2)*80,wsz,wsz)
      for i=0,2*sz*fac-1 do plots,[0,wsz],[i*ex,i*ex],color=150,/device
      for i=0,2*sz*fac-1 do plots,[i*ex,i*ex],[0,wsz],color=150,/device
      endif
    for ap=0,napers-1 do begin
      complete=where((abs(maskx) le apers(ap)*fac-0.5) and (abs(masky) le apers(ap)*fac-0.5))
      fracx=where((abs(maskx) gt apers(ap)*fac-0.5) and (abs(maskx) lt apers(ap)*fac+0.5) and $
        (abs(masky) le apers(ap)*fac-0.5))
      if (fracx(0) ne -1) then pixfracx=(0.5-(abs(maskx(fracx))-apers(ap)*fac))
      fracy=where((abs(masky) gt apers(ap)*fac-0.5) and (abs(masky) lt apers(ap)*fac+0.5) and $
        (abs(maskx) le apers(ap)*fac-0.5))
      if (fracy(0) ne -1) then pixfracy=(0.5-(abs(masky(fracy))-apers(ap)*fac))
      if (testing eq 1) then begin
        tmp3=tmp2/max(tmp2)*80
        tmp3(complete)=255
        if (fracx(0) ne -1) then tmp3(fracx)=255*pixfracx
        if (fracy(0) ne -1) then tmp3(fracy)=255*pixfracy
        tv,congrid(tmp3,wsz,wsz)
        for i=0,2*sz*fac-1 do plots,[0,wsz],[i*ex,i*ex],color=150,/device
        for i=0,2*sz*fac-1 do plots,[i*ex,i*ex],[0,wsz],color=150,/device
        endif
      if (fracx(0) eq -1) and (fracy(0) eq -1) then begin
        frac=[0] & pixfrac=0.0 & endif
      if (fracx(0) ne -1) and (fracy(0) eq -1) then begin
        frac=fracx & pixfrac=pixfracx & endif
      if (fracx(0) eq -1) and (fracy(0) ne -1) then begin
        frac=fracy & pixfrac=pixfracy & endif
      if (fracx(0) ne -1) and (fracy(0) ne -1) then begin
        frac=[fracx,fracy] & pixfrac=[pixfracx,pixfracy] & endif

      npix=(n_elements(complete)+total(pixfrac))/fac^2
      cts1=(total(tmp2(complete))+total(tmp2(frac)*pixfrac))/fac^2
      scts=skyv*npix
      cts(ap)=cts1-scts
      cerr(ap)=sqrt(abs(cts1*gain))/gain
      cerr(ap)=sqrt(cerr(ap)^2 + (serr*npix)^2)
      if (testing eq 1) then key1=get_kbrd(1)
      endfor
    endif


  if (flux+counts+mags eq 0) then begin
    if (zpt ne 0) then mags=1 $
    else if (photflam ne 1) then flux=1
    if (flux+counts+mags eq 0) then counts=1
    endif



  print,''
  print,'Apertures ',apers*1.0


  if (rawcounts ne 0) then begin
    print,'Raw Counts',cts
    print,'Raw Errors',cerr
    endif


  if (counts ne 0) then begin
    print,'ApCor Cnts',cts/apercor
    print,'Apcor Errs',cerr/apercor
    endif


  if (mags ne 0) then begin
    if (photflam ne 1) then begin
      rtnmags=flux2mag(abs(float(cts/apercor/exptime/gaincorr*photflam)))
      rtnmerrs=-1*flux2mag(1+abs(cerr/cts),0)
      print,'Calib Mags',rtnmags
      print,'Calib Errs',rtnmerrs
    endif else begin
      rtnmags=flux2mag(abs(float(cts/apercor/exptime/gaincorr)),-zpt)
      rtnmerrs=-1*flux2mag(1+abs(cerr/cts),0)
      print,'Calib Mags',rtnmags
      print,'Calib Errs',rtnmerrs
      endelse
    endif


  if (flux ne 0) then begin
    print,'Calib Flux',cts/apercor/exptime/gaincorr*photflam
    print,'Calib FErr',cerr/exptime/gaincorr*photflam
    endif


  if (total(nbad) ne 0) then begin
    print,'# Bad Pix ',nbad
    endif


  return

BAIL:

  cts=apers*0-1
  return

end
