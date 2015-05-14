pro zpeg_read_infos,zpegres,zpeginfo,error=error,$
                    nlines_header=nlines_header
;+
; NAME: zpeg_read_infos
;
; PURPOSE: 
;    Read the basic information regarding the
;    templates and filtesr, contained in a ZPEG catalog.
;
; CALLING SEQUENCE:
;    zpeg_read_infos,zpegres,templates,filters,error=error
;
; INPUTS:
;  - zpegres: input filename fo ZPEG catalog. 
;    It can also be just a file containig just a header.
;
; OUTPUTS:
;  zpeginfo is a structure containing:
;   - templates: string array of template names
;   - filters:  string array of filters
;   - calibs:  string containing the calibration of
;           magnitudes(or fluxes) in the zpeg file.
;   - obsout_calib: 'AB' or 'VEGA' or 'FLAMBDA'...
;   - magabs_calib: 'AB' or 'VEGA'
;
; OPTIONAL OUTPUT:
;  - error: set to 1 in case of error
;  - nlines_header: number of lines before data
;    - inputheader: name of the file containing the header only. 
;      Useful if the .zpeg file is not available.
;
; MODIFICATION HISTORY:
;   V1.0
;-

  zpeginfo=create_struct('filename','','version','0.')
  zpeginfo.filename=zpegres
  openr,ufi,zpegres,/get_lun,error=error
  if (error ne 0) then begin
     print,'error=',error
  endif
  bestfits='#'
  nlines_header=0
;looks into the '#' header
  while(not(eof(ufi)) and (strmid(bestfits,0,1) eq '#')) do begin
     readf,ufi,bestfits
     if (strmid(bestfits,0,1) eq '#') then begin
        nlines_header=nlines_header+1
        if (stregex(bestfits,'Templates[:=]',/boolean)) then begin
           mych=strtrim((stregex(bestfits,'Templates[:=](.*)',/extract,/subexpr))(1),2)
           templ=str2arr(mych,delim=' ',/nomult)
           zpeginfo=create_struct(zpeginfo, 'templates',templ)
        endif
        if (strpos(bestfits,'Filters') ne -1) then begin
           myst=str2arr(bestfits,delim=':')
           mych=myst(1)
           if (stregex(bestfits,',',/boolean)) then mydelim=',' else mydelim=' '
           filters=str2arr(mych,delim=mydelim,/nomult)
           zpeginfo=create_struct(zpeginfo, 'filters',strtrim(filters,2))
        endif
        if (strpos(bestfits,'Absolute mag calibration') ne -1) then begin
           myst=str2arr(bestfits,delim=':')
           mych=myst(1)
           calibs=strtrim(mych,2)
           zpeginfo=create_struct(zpeginfo, 'absmag_calib',calibs)
        endif
        if (strpos(bestfits,'Photometry calibration') ne -1) then begin
           myst=str2arr(bestfits,delim=':')
           mych=myst(1)
           calibs=strtrim(mych,2)
           zpeginfo=create_struct(zpeginfo, 'obs_calib',calibs)
        endif
        if (strpos(bestfits,'Calibrations (1=Vega, 2=AB)') ne -1) then begin
           myst=str2arr(bestfits,delim=':')
           mych=myst(1)
           calibs=str2arr(mych,delim=',',/nomult)*1
           if (calibs(0) eq 2) then cal='AB' else cal='VEGA'
           zpeginfo=create_struct(zpeginfo, 'absmag_calib',cal)
           zpeginfo=create_struct(zpeginfo, 'obs_calib','FNU_HZ')
        endif
        if (strpos(bestfits,'Mag offsets') ne -1) then begin
           myst=str2arr(bestfits,delim=':')
           mych=myst(1)
           calibs=str2arr(mych,delim=' ',/nomult)*1.
           zpeginfo=create_struct(zpeginfo, 'mag_offsets',calibs)
        endif
        if (strpos(bestfits,'Input mag offsets') ne -1) then begin
           myst=str2arr(bestfits,delim=':')
           mych=myst(1)
           calibs=str2arr(mych,delim=' ',/nomult)*1.
           zpeginfo=create_struct(zpeginfo, 'input_mag_offsets',calibs)
        endif
        if (strpos(bestfits,'Mag offsets before pass') ne -1) then begin
           myst=str2arr(bestfits,delim=':')
           mych=myst(1)
           calibs=str2arr(mych,delim=' ',/nomult)*1.
           zpeginfo=create_struct(zpeginfo, 'mag_offsets_before_pass',calibs)
        endif
        if (strpos(bestfits,'1-sigma stdev before pass') ne -1) then begin
           myst=str2arr(bestfits,delim=':')
           mych=myst(1)
           calibs=str2arr(mych,delim=' ',/nomult)*1.
           zpeginfo=create_struct(zpeginfo, 'sigma_before_pass',calibs)
        endif
        if (strpos(bestfits,'Version') ne -1) then begin
           myst=str2arr(bestfits,delim=':')
           mych=myst(1)
           calibs=strtrim(mych,2)
           zpeginfo.version=calibs
        endif
        if (strpos(bestfits,'Input photometry catalog :') ne -1) then begin
           myst=str2arr(bestfits,delim=':')
           mych=myst(1)
           calibs=strtrim(mych,2)
           zpeginfo=create_struct(zpeginfo, 'inputcat',calibs)
        endif
        if (strpos(bestfits,'zmax') ne -1) then begin
           myst=str2arr(bestfits,delim='=')
           mych=myst(1)
           calibs=strtrim(mych,2)
           zpeginfo=create_struct(zpeginfo, 'zmax',calibs)
        endif
        if (strpos(bestfits,'Area') ne -1) then begin
           myst=str2arr(bestfits,delim='=')
           mych=myst(1)
           calibs=strtrim(mych,2)
           zpeginfo=create_struct(zpeginfo, 'area',calibs)
        endif
        if (strpos(bestfits,'IMFs') ne -1) then begin
           readf,ufi,bestfits
           nlines_header=nlines_header+1
           myst=str2arr(bestfits,delim=':')
           mych=myst(1)
           myst=str2arr(mych,delim='from')
           mych=myst(0)
           calibs=strtrim(mych,2)
           zpeginfo=create_struct(zpeginfo, 'IMF',calibs)
        endif
     endif
  endwhile

;patch for old version
  iok=where(tag_names(zpeginfo) eq 'MAG_OFFSETS',nok)
  if (nok eq 0) then begin
     zeros=fltarr(n_elements(zpeginfo.filters))
     zpeginfo=create_struct(zpeginfo, 'mag_offsets',zeros)
  endif




;; calibrate the filters
  nfil=n_elements(zpeginfo.filters)
  lambda0=dblarr(nfil)
  for i=0,nfil-1 do begin
        pegase_read_onefilter,zpeginfo.filters(i),afilter
        case zpeginfo.obs_calib of
            'AB': afilter.calibtype=2
            'VEGA': afilter.calibtype=1
            else: afilter.calibtype=1
        endcase
     pegase_calibrate_filter,afilter
     lambda0(i)=afilter.lambdaeff

  endfor
     zpeginfo=create_struct(zpeginfo, 'lambda0',lambda0)

  close,ufi
  free_lun,ufi
end
