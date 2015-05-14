pro plot_bestfit_zpeg,ID,zpegres,$
                      _extra=_extra,error=error,fnu=fnu,nufnu=nufnu,reread=reread,ylog=ylog,$
                      zpeg_scen=zpeg_scen,onlyphot=onlyphot,mygalin=mygalin,nolegende=nolegende,$
                      index=index,myyr=myyr,$
                      xrange=xrange,savescen=savescen,unusedphot=unusedphot,$
                      flines_mod=flines_mod,lambdalines_mod=lambdalines_mod,residuals=residuals                      
;+
; NAME: plot_bestfit_zpeg
;
; PURPOSE: 
;     Plot the ZPEG best fits for a given object in a
;     zpeg output catalog
;
; CALLING SEQUENCE:  
;    plot_bestfit_zpeg,ID,zpegres,$
;             psout=psout,_extra=_extra,error=error,fnu=fnu
;
; INPUTS: 
;    - ID: string or array of strings, containing the ID of
;      objects to plot
;    - index : if set, supersides ID
;    - zpegres: filename of the ZPEG output catalog
;
; keyword:
;   - fnu: if set, fnu=f(lambda) and AB magnitudes are plotted. If not
;     (default), then flambda is plotted.
;
; MODIFICATION HISTORY:
;
;-

  if (not(keyword_set(fnu)) and not(keyword_set(nufnu))) then flambda=1 else flambda=0

  error=0
  realid=id

  if (not(keyword_set(reread))) then reread=0
  if (not(keyword_set(ylog))) then ylog=0


  zpeg_read_infos,zpegres,zpeginfo,error=error,$
                  nlines_header=nlines_header

  version=zpeginfo.version
  templtab=zpeginfo.templates

  zpeg_get_fitinfo,ID,zpegres,$
                   mygal,stfilters,pho,pho_err,$
                   models,lambda_mod,flux_mod,lambdalines_mod,flines_mod,$
                   pho_obscalib,pho_obscalib_err,$ ;; real photometry as in zpeg catalog (can be mag AB)
                   modelmags,modelmagerrs,$
                   error=error,fnu=fnu,nufnu=nufnu,reread=reread,zpeg_scen=zpeg_scen,mygalin=mygalin,index=index,savescen=savescen

  nfilters=n_elements(stfilters.abvega)

  realid=mygal.id

  loadct,39,/SILENT


;;********************************************************************************
; plot

; X RANGE --------------------------------------------------
  if not(keyword_Set(xrange)) then $
     xminmax=minmax(stfilters.lambdamean)+[-1000.,1000.] else $
        xminmax=xrange

  ilambdao=where(lambda_mod ge xminmax(0) and lambda_mod le xminmax(1) and flux_mod gt 0.)

  if (ilambdao[0] ne -1) then begin
     modmin=min(flux_mod(ilambdao))
     modmax=max(flux_mod(ilambdao))
  endif else begin
     modmin=0.
     modmax=0.
  endelse


  if (not(keyword_set(unusedphot))) then begin
     iok=where(pho_err(1,*) gt 0. and $
               stfilters.lambdamean ge xminmax(0) and stfilters.lambdamean le xminmax(1))
  endif else begin
     iok=where(pho ne -1. and $
               stfilters.lambdamean ge xminmax(0) and stfilters.lambdamean le xminmax(1))
  endelse

; Y RANGE --------------------------------------------------
  if keyword_set(residuals) then begin
     yminmax=[-1,1] 
     myys=1
     myytit='Residual (model mag - obs mag)'
     ylog=0
  endif else begin
     if (iok(0) ne -1) then begin
        yminmax=[min([pho(iok),modmin]),$
                 max([pho(iok),modmax])]    
     endif else begin
        yminmax=[min(pho),max(pho)]
     endelse

     if (ylog) then begin
        yminmax=yminmax*[0.5,10.] 
     endif else begin
        yminmax=[yminmax(0)-0.2*(yminmax(1)-yminmax(0)),$
                 yminmax(1)+0.5*(yminmax(1)-yminmax(0))] 
        if (yminmax(0) gt 0.) then yminmax(0)=-yminmax(0)
     endelse

     if (keyword_set(fnu)) then begin
        myys=9
        myytit='Fnu (Jy)'       ;=1e-23*erg/s/cm^2/Hz)'
     endif else begin
        if keyword_set(nufnu) then begin
           myys=9
           myytit='nu*Fnu'      ;=1e-23*erg/s/cm^2/Hz)'
        endif else begin
           myys=1
           myytit='Flambda (erg/s/cm^2/A)'
        endelse
     endelse
  endelse

  if (n_elements(myyr) eq 2) then begin
     if (myyr(0) ne 0 and myyr(1) ne 0) then yminmax=myyr
  endif

  plot,[0,1],[0,1],/nodata,xr=xminmax,yr=yminmax,$
       xtit='Lambda (A)',ytit=myytit,xs=1,ys=myys,title=realID,ylog=ylog,_extra=_extra

  if not keyword_set(residuals) then begin
     if (keyword_set(fnu)) then begin
        my_yminmax=yminmax
        if (yminmax(0) le 0) then my_yminmax(0)=1e-2*my_yminmax(1)
        mag_minmax=minmax(-2.5*alog10(my_yminmax*1e-23)-48.60)+[-1.,1]
        dmposs=[0.01,0.02,0.05,0.1,0.2,0.5,1.,2.,5.,10.]
        for l=0,n_elements(dmposs)-1 do begin
           if (round(mag_minmax(1)-mag_minmax(0))/dmposs(l) ge 12) then $
              dm=dmposs(l)        
        endfor
        mini=dm*round(mag_minmax(0)/dm)
        V = mini+indgen(round((mag_minmax(1)-mag_minmax(0))/dm))*dm
        L = string(format='(f5.1)',V)
        V=1d23*10.^(-0.4*(V+48.60))
        n=n_elements(L) 
        axis, yaxis=1, yticks=N-1, ytickv=V, ytickname=L, ytitle='AB mag'
     endif
  endif
  if not keyword_set(nolegende) then begin
     if (mygal.xpix gt 0.) then legende,0.8,'zspec='+strtrim(string(format='(f6.3)',mygal.xpix),2),_extra=_extra
  endif


;--------------------------------------------------------------------------------
;oplot models

  nsols=mygal.nsol
  for i=0,nsols-1 do begin

     lambda=lambda_mod(*,i)
     lambdalines=lambdalines_mod(*,i)

     ;; DATA SPECTRUM --------------------------------------------------
     mycolor=i*40+40
     if not keyword_set(residuals) then begin
        ;; plot spectrum
        oplot,lambda,flux_mod(*,i),color=mycolor,thick=2.

        ilok=where(lambda ge min(stfilters.lambdamean) and $
                   lambda le max(stfilters.lambdamean))
        
        for l=0,n_elements(lambdalines)-1 do begin
           iok=(where(lambda ge lambdalines(l)))(0)
           oplot,lambdalines(l)*[1,1],flux_mod(iok,i)+flines_mod(l,i)/200.*[0,1],$
                 color=mycolor,thick=2.,linestyle=1
        endfor

        ;; ---------------------------------------
        ;; photometry
        for ifi=0,nfilters-1 do begin
           if (not(keyword_set(onlyphot)) or $
               (keyword_set(onlyphot) and pho_err(0,ifi) gt 0.)) then $
                  oplot,[stfilters(ifi).lambdaeff],[models(ifi,i)],psym=4,color=mycolor
        endfor        
     endif else begin
        oplot,[1,1d6],[0,0]
        for ifi=0,nfilters-1 do begin
           if (pho_err(0,ifi) gt 0) then begin ; filter used
              vsym,8,/fill
              ;;model mags from zpeg - obs orig
              oploterror,[stfilters(ifi).lambdaeff],-[pho_obscalib(ifi)-modelmags(ifi,i)],[pho_obscalib_err(ifi)],psym=8,symsize=0.6,color=mycolor,errcolor=mycolor
              oploterror,[stfilters(ifi).lambdaeff],-[pho_obscalib(ifi)-modelmags(ifi,i)],[modelmagerrs(ifi,i)],psym=8,symsize=0.6,color=mycolor,errcolor=mycolor,hat=2.*!D.X_VSIZE / 100

              ;; mags from pegaseutils - obs in Flux density,
              ;;                         all converted in magnitudes
              oplot,[stfilters(ifi).lambdaeff],$
                    [-2.5*alog10(models(ifi,i)/pho(ifi))],$
                    psym=4,symsize=1.6,color=mycolor

           endif else begin
              if keyword_set(unusedphot) then begin
                 vsym,8
                 oplot,[stfilters(ifi).lambdaeff],-[pho_obscalib(ifi)-modelmags(ifi,i)],psym=8,symsize=0.6,color=mycolor
                 oploterror,[stfilters(ifi).lambdaeff],-[pho_obscalib(ifi)-modelmags(ifi,i)],[abs(modelmagerrs(ifi,i))],psym=8,symsize=0.6,color=mycolor,errcolor=mycolor,hat=2.*!D.X_VSIZE / 100
              endif
           endelse       
        endfor
     endelse
     ;; LEGEND--------------------------------------------------
     if not keyword_set(nolegende) then begin
        ;; legend
        x0=!x.crange(0)+0.1*(!x.crange(1)-!x.crange(0))
        iok=(where(lambda ge x0))(0)
        mycolor=i*40+40

        if (version le 4.0) then begin
           mass=mygal.zphots(i).m 
           massmin=mygal.zphots(i).mmin 
           massmax=mygal.zphots(i).mmax
        endif else begin
           mass=mygal.zphots(i).mass(0)
           massmin=mygal.zphots(i).mass(1)
           massmax=mygal.zphots(i).mass(2)
        endelse

        if version lt 5.03 then di=1.8 else di=2.7
        if version ge 5.03 then begin
           gmt=(1d0/10d0^(mygal.zphots(i).specsfr(0)+9.))<99. ; growth time in Gyr
           gmtmax=(1d0/10d0^(mygal.zphots(i).specsfr(1)+9.))<99.+gmt
           gmtmin=(1d0/10d0^(mygal.zphots(i).specsfr(2)+9.))<gmt
        endif
        legende,1.5+i*di+0,$
                strtrim(string(format='("#",i2,"; z=",f5.3,"!S!U+",f5.3,"!R!D-",f5.3,"!N; n!Dbands!N= ",i2,"; E(B-V)!Dadd!N=",f3.1)',$
                               i+1,mygal.zphots(i).z(0),$
;                               max(mygal.zphots(0:mygal.nsol-1).z(2))-mygal.zphots(i).z(0),$
;                               mygal.zphots(i).z(0)-min(mygal.zphots(0:mygal.nsol-1).z(1)),$
                               mygal.zphots(i).z(2)-mygal.zphots(i).z(0),$
                               mygal.zphots(i).z(0)-mygal.zphots(i).z(1),$
                               mygal.nbands,$
                               mygal.zphots(i).ebv),2),$
                linestylep1=1,color=mycolor,charsize=0.65,linelen=0.7,thick=2.,_extra=_extra
        
        
        legende,1.5+i*di+0.9,'   '+$
                strtrim(string(format='(a7," @",f6.3," Gyr; M*=",f5.2,"!S!U+",f5.2,"!R!D-",f5.2,"!N")',$
                               templtab(mygal.zphots(i).itemplate-1),$
                               mygal.zphots(i).age,$
                               mass,massmax-mass,mass-massmin),2),$
                charsize=0.65,thick=1.,_extra=_extra
        if (version ge 5.03) then begin
           legende,1.5+i*di+1.8,'   '+$
                   string(format='("<age>!DL!N=",f6.3,"!S!U+",f6.3,"!R!D-",f6.3,"!N Gyr; DMT=",f6.3,"!S!U+",f6.3,"!R!D-",f6.3,"!N Gyr")',$
                          mygal.zphots(i).agestars(0),$
                          mygal.zphots(i).agestars(2)-mygal.zphots(i).agestars(0),$
                          mygal.zphots(i).agestars(0)-mygal.zphots(i).agestars(1),$
                          gmt,gmtmax-gmt,gmt-gmtmin),$
                   charsize=0.55,thick=1.,_extra=_extra
        endif
     endif
  endfor

;--------------------------------------------------------------------------------
;oplot photometry
  if not keyword_set(residuals) then begin
     for i=0,nfilters-1 do begin
        if (pho_err(0,i) gt 0) then begin
           vsym,8,/fill
           oploterror,[stfilters(i).lambdaeff],[pho(i)],[pho_err(0,i)],psym=8,symsize=0.6,/lobar
           oploterror,[stfilters(i).lambdaeff],[pho(i)],[pho_err(1,i)],psym=8,symsize=0.6,/hibar
        endif else begin
           if keyword_set(unusedphot) then begin
              vsym,8
              oplot,[stfilters(i).lambdaeff],[pho(i)],psym=8,symsize=0.6
           endif
        endelse
     endfor
  endif



  if (keyword_set(psout)) then begin
     device,/close
     set_plot,'x'
  endif


end
 
