pro zpeg_get_fitinfo,ID,zpegres,$
                     mygal,stfilters,$
                     pho,pho_err,$ ; in units if Flambda or Fnu only !
                     models,$
                     lambda_mod,flux_mod,lambdalines_mod,flines_mod,$
                     pho_obscalib,pho_obscalib_err,$ ;; real photometry as in zpeg catalog (can be mag AB)
                     modelmags,modelmagerrs,$        ; from zpeg
                     zpeg_scen=zpeg_scen,$
                     error=error,fnu=fnu,nufnu=nufnu,reread=reread,mygalin=mygalin,$
                     index=index,verbose=verbose,$
                     savescen=savescen

;+
; NAME: 
;
; PURPOSE: 
;
; CALLING SEQUENCE:  
;
; INPUTS: 
;    - ID: string or array of strings, containing the ID of
;      objects to plot
;    - zpegres: filename of the ZPEG output catalog
;
; OUTPUTS:
;   - mygal: zpeg galaxy structure for object described by ID
;   - stfilters: (nfilters) array of filter structures.
;   - pho: (nfilters) array with observed photometry in flux density (Fnu or
;     flambda, depending on fnu keyword)
;   - pho_err: (2,nfilters) array with errors on the observed photometry
;   - models: (nfilters,nsols) array with synthetic
;     photometry
;   - lambda_mod: (nlambda,nsols)  wavelength scale of synthetic spectra
;   - flux_mod: (nlambda,nsols) array with best fit spectra
;   - lambdalines_mod  (nlambdalines,nsols): wavelength scale of lines in synthetic spectra
;   - flines_mod: (nlambdalines,nsols) array with best fit
;     spectra lines
;   - residual mags: array (nfilters,nsols) of residual mags between model and
;     photometry form zpeg
;
; keyword:
;   - fnu: if set, fnu=f(lambda) and AB magnitudes are plotted. If not
;     (default), then flambda is plotted.
;   - savescen=0,1 or 2. If 0, the templates are read. If 1, the
;     templates are read and saved in a variable. if savescen=2, the
;     templates are not read, but the saved variable is restored.
; MODIFICATION HISTORY:
;
;-

;t=systime(1)

  if (not(keyword_set(reread))) then reread=0
  if (not(keyword_set(savescen))) then savescen=0

  if (savescen eq 0 or savescen eq 1) then begin

     ;;read filters and template info
     zpeg_read_infos,zpegres,zpeginfo,error=error

     templtab=zpeginfo.templates
     ;;checks templtab all exist in current directory. If not, then search
     ;;them in the zpeg ROOT directory if zpeg_scen is set
     if (keyword_set(zpeg_scen)) then begin
        for ite=0,n_elements(templtab)-1 do begin
           openr,utest,templtab(ite),/get_lun,error=error
           if (error ne 0) then begin            
              templtab(ite)=getenv('ZPEG_ROOT')+'/data/templates/'+templtab(ite)
              if keyword_set(verbose) then print,'Scenario not found in current directory. Trying to find the scenario in ZPEG root...'            
              openr,utest,templtab(ite),/get_lun,error=error
              if (error ne 0) then begin            
                 print,'file '+templtab(ite)+' does not exist either... too bad.'
                 free_lun,utest
                 return
              endif else begin
                 close,utest
                 free_lun,utest
              endelse
           endif else begin
              close,utest
              free_lun,utest
           endelse
;            print,templtab(ite)
        endfor
     endif

     filters=zpeginfo.filters
     ;;checks filters all exist in current directory. If not, then search
     ;;them in the zpeg ROOT directory if zpeg_scen is set
     if (keyword_set(zpeg_scen)) then begin
        for ite=0,n_elements(filters)-1 do begin
           openr,utest,filters(ite),/get_lun,error=error
           if (error ne 0) then begin            
              filters(ite)=getenv('ZPEG_ROOT')+'/data/filters/'+filters(ite)
              if keyword_set(verbose) then print,'Filter not found in current directory. Trying to find the scenario in ZPEG root...'            
              free_lun,utest
              openr,utest,filters(ite),/get_lun,error=error
              if (error ne 0) then begin            
                 print,'file '+filters(ite)+' does not exist either... too bad.'
                 free_lun,utest
                 return
              endif else begin
                 close,utest
                 free_lun,utest
              endelse                
           endif else begin
              close,utest
              free_lun,utest
           endelse
        endfor
     endif

     obs_calib=zpeginfo.obs_calib

                                ;calibrate filters
     nfilters=n_elements(filters) 
     for i=0,nfilters-1 do begin
        pegase_read_onefilter,filters(i),afilter
        case obs_calib of
           'AB': afilter.calibtype=2
           'VEGA': afilter.calibtype=1
           else: afilter.calibtype=1
        endcase

        pegase_calibrate_filter,afilter
        if (n_elements(stfilters) eq 0) then begin
           stfilters=replicate(afilter,nfilters)
        endif
        stfilters(i)=afilter
     endfor    


     if savescen eq 1 then begin
        save,filename=zpegres+'zpegtemplates1.save',$
             zpeginfo,templtab,filters,obs_calib,stfilters,nfilters
     endif
  endif 
  if savescen eq 2 then begin
     restore,zpegres+'zpegtemplates1.save'
  endif

  if (n_elements(mygalin) eq 0) then begin
     ;;read data
     read_zpegres,zpegres,data,reread=reread,/nosave
     if (n_elements(index) eq 0) then begin
        iok=(where(strtrim(data.id,2) eq strtrim(id,2)))(0)
        if (iok eq -1) then begin
           print,'ID not found....'
           return
        endif
     endif else begin
        iok=index
     endelse
     mygal=data(iok)
  endif else begin
     mygal=mygalin
  endelse

;print,'      zpeg_getfitinfo: tout debut=',systime(1)-t
;t=systime(1)

;;--------------------------------------------------------------------------------
;prepare photometry

  pho=dblarr(nfilters)
  pho_err=dblarr(2,nfilters)    ; [lower error bar(positive), upper error bar (positive)]

  pho_obscalib=dblarr(nfilters)
  pho_obscalib_err=dblarr(nfilters)

  for i=0,nfilters-1 do begin
     pho_obscalib(i)=mygal.obs(2*i)     
     pho_obscalib_err(i)=mygal.obs(2*i+1)
  endfor

  for i=0,nfilters-1 do begin
;    print,'ifil=',i
     ;;convert obs_calib magnitudes to flux density flambda
     ;;(erg/s/cm2/A)
     pho_orig=pho_obscalib(i)
     if (obs_calib eq 'AB' or obs_calib eq 'VEGA') then begin        
        if (pho_orig eq 99.) then begin
           pho(i)=-1.
           pho_err(0,i)=-1.
           pho_err(1,i)=-1.
        endif else begin
           mycalib=stfilters(i).calib ; to convert from obs_calib mag to flambda (erg/s/cm2/A)
           pho(i)=mag_to_flux(pho_orig,mycalib)
           errs_save=pho_obscalib_err(i)
           pho_err(1,i)=pho(i)*(10.^(+0.4*errs_save)-1.) 
;            pho_err(0,i)=pho(i)*(1.-10.^(-0.4*errs_save(1))); if convserion from mag to flux. But in zpeg, the err bar is defined on the flux density and is symetrical. So:
           pho_err(0,i)=pho_err(1,i)
        endelse
     endif
     if (obs_calib eq 'FLAMBDA') then begin        
                                ; do nothing !
     endif
     if (obs_calib eq 'FNU_HZ' or obs_calib eq 'FNU_JY') then begin        
        if (obs_calib eq 'FNU_JY') then begin ; back to FNU_HZ...
           pho(i)=1d-23*pho(i)
           pho_err(*,i)=1d-23*pho_err(*,i)
        endif
                                ;Fnu to Flambda
        pho(i)=pho(i)*stfilters(i).areanu/stfilters(i).area
        pho_err(*,i)=pho_err(*,i)*stfilters(i).areanu/stfilters(i).area
     endif 

     ;;offsets
     if (pho_err(0,i) gt 0.) then begin
        pho(i)=pho(i)*(10.d0)^(-0.4*zpeginfo.mag_offsets(i))
        pho_err(*,i)=pho_err(*,i)*(10.d0)^(-0.4*zpeginfo.mag_offsets(i))
     endif

     ;; now convert Flambda to fnu_jy if necessary
     if (keyword_set(fnu)) then begin
        pho(i)=1d23*pho(i)*stfilters(i).area/stfilters(i).areanu
        pho_err(*,i)=1d23*pho_err(*,i)*stfilters(i).area/stfilters(i).areanu
     endif

     ;; now convert Flambda to fnu_jy if necessary
     if (keyword_set(nufnu)) then begin
        nu=3d8/(stfilters(i).lambdamean*1d-10)
        pho(i)=nu*1d23*pho(i)*stfilters(i).area/stfilters(i).areanu
        pho_err(*,i)=nu*1d23*pho_err(*,i)*stfilters(i).area/stfilters(i).areanu
     endif

     ;;last check
     if (obs_calib eq 'AB' or obs_calib eq 'VEGA') then begin        
        if (pho_orig eq 99.) then begin
           pho(i)=-1.
           pho_err(0,i)=-1.
           pho_err(1,i)=-1.
        endif 
     endif
  endfor

;print,'      zpeg_getfitinfo: filter calibs=',systime(1)-t
;t=systime(1)

;models
  if (savescen eq 0 or savescen eq 1) then begin
;    print,'READING TEMPLATES'
     ;; read all templates
     ntemplates=n_elements(templtab)
     pegase_read_lambdaline,templtab(0),nlambda,lambda,nlines,lambdalines

     nagesmax=200
     nlambdamax=nlambda
     nlinesmax=nlines

     flux=dblarr(nagesmax,nlambdamax)
     flines=dblarr(nagesmax,nlinesmax)
     lambda=dblarr(nlambdamax)
     lambdalines=dblarr(nlinesmax)
     tabage=findgen(nagesmax)
     onetemplate={nages:0L, nlambda:0L, nlines:0L, $
                  tabage:tabage, lambda:lambda, lambdalines:lambdalines, flux:flux, flines:flines}
     templates=replicate(onetemplate,ntemplates)

     for itempl=0,ntemplates-1 do begin        
        pegase_read_ages,templtab(itempl),tabage,nages
        templates(itempl).nages=nages
        templates(itempl).tabage(0:nages-1)=tabage                

        pegase_read_lambdaline,templtab(itempl),nlambda,lambda,nlines,lambdalines
        templates(itempl).nlambda=nlambda
        templates(itempl).lambda(0:nlambda-1)=lambda
        templates(itempl).nlines=nlines
        templates(itempl).lambdalines(0:nlines-1)=lambdalines
        
        pegase_read_all_fluxesline,templtab(itempl),fluxes,flines ;(nages, nlambda)
        for ia=0,nages-1 do begin
           templates(itempl).flux(ia,0:nlambda-1)=fluxes(ia,*)
           templates(itempl).flines(ia,0:nlines-1)=flines(ia,*)
        endfor
     endfor
     if savescen ge 1 then save,filename='zpegtemplates_temp2.save',templates
  endif
  if savescen eq 2 then begin
;    print,'RESTORING...'
     restore,'zpegtemplates_temp2.save' ;;templates
  endif
;print,'      zpeg_getfitinfo: tempaltes read or write=',systime(1)-t
;t=systime(1)

  itempl=mygal.zphots(0).itemplate-1 ; to idl indices
  if (itempl lt 0) then begin
     print,'ERROR: no best fit for this object...'
     error=1
     return
  endif
  if (mygal.nsol ge 1) then begin
     nlambda=templates(itempl).nlambda
     lambda=templates(itempl).lambda(0:nlambda-1)
     nlines=templates(itempl).nlines
     lambdalines=templates(itempl).lambdalines(0:nlines-1)
     models=dblarr(nfilters,mygal.nsol)
     modelmags=dblarr(nfilters,mygal.nsol)
     modelmagerrs=dblarr(nfilters,mygal.nsol)
     flux_mod=dblarr(nlambda,mygal.nsol)
     flines_mod=dblarr(nlines,mygal.nsol)
     lambda_mod=dblarr(nlambda,mygal.nsol)
     lambdalines_mod=dblarr(nlines,mygal.nsol)
  endif else begin
     models=dblarr(nfilters,1)
     modelmags=dblarr(nfilters,1)
     modelmagerrs=dblarr(nfilters,1)
     nlambda=2
     nlines=2
     flux_mod=dblarr(nlambda,1)
     flines_mod=dblarr(nlines,1)
     lambda_mod=dblarr(nlambda,1)
     lambdalines_mod=dblarr(nlines,1)    
  endelse
;print,'      zpeg_getfitinfo: templates first init=',systime(1)-t



  for i=0,mygal.nsol-1 do begin
     
     z=mygal.zphots(i).z(0)
     
     itempl=mygal.zphots(i).itemplate-1 ; to idl indices
     if (itempl lt 0) then begin
        error=1
        return
     endif

;    pegase_read_lambdaline,templtab(itempl),nlambda,lambda,nlines,lambdalines
     nlambda=templates(itempl).nlambda
     lambda=templates(itempl).lambda(0:nlambda-1)
     nlines=templates(itempl).nlines
     lambdalines=templates(itempl).lambdalines(0:nlines-1)



;    pegase_iage,templtab(itempl),mygal.zphots(i).age*1000.,iage,agetrue
;    pegase_read_fluxline,templtab(itempl),iage,flux,fluxlines
     agediff=abs(templates(itempl).tabage(0:templates(itempl).nages-1)$
                 -mygal.zphots(i).age*1000.)
     iok=(where(agediff eq min(agediff)))(0)
     flux=templates(itempl).flux(iok,0:nlambda-1)
     fluxlines=templates(itempl).flines(iok,0:nlines-1)
     
     il=where(lambda ge 6000 and lambda le 7000)
;    print,minmax(flux(il))
     
                                ;redden
     if (mygal.zphots(i).ebv ne 0.) then begin
        calz_unred,lambda,flux,-mygal.zphots(i).ebv,fred ,/SILENT
        flux=fred
        calz_unred,lambdalines,fluxlines,-mygal.zphots(i).ebv,fred ,/SILENT
        fluxlines=fred
     endif
;    print,minmax(flux(il)),mygal.zphots(i).ebv
     
     flux=flux/(1.+z)
     fluxlines=fluxlines
     
;    print,minmax(flux(il))
     
     flux=flux*mygal.zphots(i).norm
     fluxlines=fluxlines*mygal.zphots(i).norm
     
;    print,minmax(flux(il))
     
     lambda=lambda*(1.+z)
     lambdalines=lambdalines*(1.+z)
     
     trans=igm_transmission(z,lambda)
     flux=flux*trans
     trans=igm_transmission(z,lambdalines)
     fluxlines=fluxlines*trans

     
;    print,lambda(200)
;    print,minmax(flux(il))
;    print,'             zpeg_getfitinfo: in minima _ debut=',systime(1)-t
;    t=systime(1)

     ;; ---------------------------------------
     ;; photometry
     for ifi=0,nfilters-1 do begin
        error=0
        pegase_compute_mag_filter,stfilters(ifi),$
                                  lambda,flux,lambdalines,fluxlines,mag,fluxfilter,error=error
        if (keyword_set(fnu)) then begin
           fluxfilter=fluxfilter/stfilters(ifi).areanu*1e23
        endif else begin
           if keyword_set(nufnu) then begin
              fluxfilter=(3d8/(stfilters(ifi).lambdamean*1d-10))*$
                         fluxfilter/stfilters(ifi).areanu*1e23
           endif else begin               
              fluxfilter=fluxfilter/stfilters(ifi).area                  
           endelse
        endelse
        models(ifi,i)=fluxfilter
     endfor


     ;; ---------------------------------------
     ;; model mags from zpeg
                                ;print, nfilters,i,mygal.nsol
     tagnames=tag_names(mygal.zphots(0))
     ii=where(tagnames eq 'MODELMAGS',ni)
     if ni ne 0 then begin
        for ifi=0,nfilters-1 do begin
           modelmags(ifi,i)=mygal.zphots(i).modelmags(ifi)
           modelmagerrs(ifi,i)=mygal.zphots(i).modelmagerrs(ifi)
        endfor
     endif


     ;; ---------------------------------------
     ;; spectrum :
     ;; convert Flambda(erg/s/cm2/A) to Fnu(Jy) if necessary
     if (keyword_set(fnu)) then begin 
        flux=1d23*convert_flambda_to_fnu(lambda,flux)
        fluxlines=1d23*convert_flambda_to_fnu(lambdalines,fluxlines)
     endif       

     if (keyword_set(nufnu)) then begin 
        flux=(3d8/(lambda*1d-10))*1d23*convert_flambda_to_fnu(lambda,flux)
        fluxlines=(3d8/(lambda*1d-10))*1d23*convert_flambda_to_fnu(lambdalines,fluxlines)
     endif       

     flux_mod(*,i)=flux
     flines_mod(*,i)=fluxlines
     lambda_mod(*,i)=lambda
     lambdalines_mod(*,i)=lambdalines


     
  endfor




end
