pro radprof2,image,xcen,ycen,radius,oplot=oplot1,expand=expand1, $
  counts=counts,fitparams=fitparams,silent=silent,noplot=noplot, $
  subtract=subtract,powerlaw=powerlaw,ellipmask=ellipmask,$
  subparams=subparams
;+
;NAME:
;      RADPROF
;PURPOSE:
;      Plot a radial profile of a (stellar) object and overplot a gaussian
;      fit.  Also prints out some fit parameters, e.g. Sky, FWHM, Max, Total.
;CALLING SEQUENCE:
;      RADPROF,image,xcen,ycen,radius,/oplot,expand=nn
;INPUTS:
;      IMAGE  - Input image
;      XCEN   - Centroid (exact!) X position of star (scalar!)
;      YCEN   - Centroid (exact!) Y position of star (scalar!)
;      RADIUS - Radius to examine (and fit) star (scalar)
;OPTIONAL INPUTS:
;      OPLOT  - Do an OPLOT instead of a PLOT
;      EXPAND - Does a cubic interpolative expansion of the object before
;                 doing the radial plot.  This has the advantage of generating
;                 more data points, but is not particularly honest.  It
;                 also doesn't gain you much, and probably should be avoided.
;OUTPUTS:
;      Plots a radial profile to graphics channel with overlay fit.
;      Prints in a row:
;        X,Y  - X,Y centroid of star (AS GIVEN!) This must be exact
;        Sky  - Sky value derived from fit.  may be too high if radius too smal
;        FWHM - Derived FWHM from fitted gaussian
;        Max  - Derived Maximum of fitted gaussian (not max pixel value!)
;        Total- Derived total volume of fitted gaussian 
;PROCEDURE:
;      Generate a plot of pixel value versus radius of the star.  Symmetrize
;      the plot and fit a gaussian using USERLIB GAUSSFIT.  Plot final fit
;      and print out some useful fitted parameters
;MODIFICATION HISTORY
;      06-JUN-94  Written by Eric W. Deutsch
;-


  if (n_params(0) lt 4) then begin
    print,'Call> RADPROF,image,xcen,ycen,radius,/oplot,expand=nn'
    print,'e.g.> RADPROF,img,xc,yc,6'
    return
    endif


  if (n_elements(oplot1) eq 0) then oplot1=0
  if (n_elements(noplot) eq 0) then noplot=0
  if (n_elements(silent) eq 0) then silent=0
  if (n_elements(powerlaw) eq 0) then powerlaw=0
  if (n_elements(subtract) eq 0) then subtract=0
  if (n_elements(expand1) eq 0) then expand1=1
  if (n_elements(ellipmask) eq 0) then ellipmask=[1.0,0]


  COMMON FITPHOT,SKYV1,SIGMAV1,BETA4,BETA6
  if (n_elements(SKYV1) eq 0) then SKYV1=-999
  if (n_elements(SIGMAV1) eq 0) then SIGMAV1=-999
  if (n_elements(BETA4) eq 0) then BETA4=-999
  if (n_elements(BETA6) eq 0) then BETA6=-999


  xc=fix(xcen) & yc=fix(ycen) & siz=fix(radius+2)
  img1=extrac(image,xc-siz,yc-siz,siz*2,siz*2)*1.0d


  expand1=fix(expand1)
  if (expand1 gt 1) then begin
    img2=interpolate(img1,findgen(siz*2*expand1)/expand1, $
      findgen(siz*2*expand1)/expand1,/grid,/cubic)
;                          tmp2(where(tmp2 lt min(tmp)))=min(tmp)
    dist_ellipse,mask,siz*2*expand1,(xcen-xc+siz)*expand1, $
      (ycen-yc+siz)*expand1,ellipmask(0),ellipmask(1)
  endif else begin
    img2=img1
    dist_ellipse,mask,siz*2,xcen-xc+siz,ycen-yc+siz,ellipmask(0),ellipmask(1)
    endelse

  tt1=where(mask le radius*expand1)
  mask=mask(tt1)
  img2=img2(tt1)

  s=size(mask)
  srt=sort(mask)
  x=mask(srt)/expand1
  y=img2(srt)
  mxv=max(y) & mnv=min(y)
  tmp1=abs(y-avg([mxv,mnv]))
  hw1=where(min(tmp1) eq tmp1)
  sig1=x(hw1(0))*2/2.45
  w=y*0+1
  w(0:5)=5
;  w(0:hw1(0)*3)=sqrt(y(0:hw1(0)*3)>1)>1


  a=[mxv-mnv]
  if (SKYV1 eq -999) then a=[a,mnv]
  if (SIGMAV1 eq -999) then a=[a,sig1]
  if (n_elements(a) eq 1) then a=[a,1.0]
  x2=dindgen(radius*100)/100


  if (powerlaw eq 0) then begin
    if (n_elements(subparams) ne 5) then begin
      yfit=curvefit(x,y,w,a,sigmaa,function_name='radprof2_fn')
      if ((SKYV1 ne -999) and (SIGMAV1 ne -999)) then a=[a(0),SKYV1,SIGMAV1]
      if ((SKYV1 eq -999) and (SIGMAV1 ne -999)) then a=[a,SIGMAV1]
      if ((SKYV1 ne -999) and (SIGMAV1 eq -999)) then a=[a(0),SKYV1,a(1)]
    endif else a=subparams
    radprof2_fn,x2,a,f
    counts=a(0)*2*!dpi*a(2)^2/ellipmask(0)
    fitparams=a				; [amplitude,sky level,sigma]
    if (subtract ne 0) then begin
      img1=extrac(image,xc-subtract,yc-subtract,subtract*2,subtract*2)*1.0d
      dist_ellipse,mask1,subtract*2,xcen-xc+subtract,ycen-yc+subtract,ellipmask(0),ellipmask(1)
      radprof2_fn,mask1,a,f1
      img1=img1-(f1-a(1))
      imgput,image,img1,xc-subtract,yc-subtract
      endif
    endif


  if (powerlaw eq 1) then begin
    if (n_elements(subparams) ne 5) then begin
      if (BETA4 eq -999) then a=[a,1.0]
      if (BETA6 eq -999) then a=[a,1.0]
      print,'input a:     ',a,format='(a,6f13.7)'
      yfit=curvefit(x,y,w,a,sigmaa,function_name='radprof2_fn2',iter=iter,chi2=chi2)
      print,'output a:    ',a,format='(a,6f13.7)'
      print,'output siga: ',sigmaa,format='(a,6f13.7)'
      print,'iter, chi2: ',iter,chi2
      if ((SKYV1 ne -999) and (SIGMAV1 ne -999)) then a=[a(0)*a(1),SKYV1,SIGMAV1]
      if ((SKYV1 eq -999) and (SIGMAV1 ne -999)) then a=[a,SIGMAV1]
      if ((SKYV1 ne -999) and (SIGMAV1 eq -999)) then a=[a(0),SKYV1,a(1)]
      if (BETA4 ne -999) then a=[a,BETA4]
      if (BETA6 ne -999) then a=[a,BETA6]
    endif else a=subparams
    x2=dindgen((sig1>1.0)*5*1000)/1000/ellipmask(0)
    radprof2_fn2,x2,a,f
    tt1=where(f lt (max(f)-min(f))/1000+min(f))
    minpt=tt1(0)
    counts=!dpi*total( (x2(1:minpt-1)^2 - x2(0:minpt-2)^2 ) * (f(0:minpt-1)-f(minpt)) )
    if (subtract ne 0) then begin
      img1=extrac(image,xc-subtract,yc-subtract,subtract*2,subtract*2)*1.0d
      dist_ellipse,mask1,subtract*2,xcen-xc+subtract,ycen-yc+subtract,ellipmask(0),ellipmask(1)
      radprof2_fn2,mask1,a,f1
      img1=img1-(f1-a(1))
      imgput,image,img1,xc-subtract,yc-subtract
      endif
    print,a,format='(6f13.7)'
    fitparams=a				; [amplitude,sky level,sigma,beta4,beta6]
    endif


  if (noplot eq 0) then begin
    if (oplot1 eq 0) then $
      plot,mask/expand1,img2,psym=4,xr=[0,radius], $
	yr=[mnv<min(f),mxv>max(f)],ysty=1, $
	xtit='Radius (Pixels)',ytit='Counts' $
      else oplot,mask/expand1,img2,psym=2
    oplot,x2,f,color=!d.n_colors-2<255
    endif


  if (silent eq 0) then begin
    print,'    X       Y         Sky        FWHM       Max           Total'
    print,'--------  ------  ----------  --------  ----------  -------------'
    print,format='(2f8.2,f12.3,f10.3,f12.2,f15.2)', $
      xcen,ycen,a(1),a(2)*2.35,a(0),counts
    endif


  return

end


