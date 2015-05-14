 
;*******************************************************************************
pro fiberfit, wl, flux, err, objinfo, linepars, indexpars, noplot = noplot, $
    data = fiber, interact = interact
;*******************************************************************************


;*******************************************************************************
; Extract spectrum, errors, and create wavelength array 
;*******************************************************************************

fiber.fiberid = objinfo.fiberid
fiber.photoid = objinfo.objid
fiber.ra = objinfo.ra
fiber.dec = objinfo.dec
fiber.xfocal = objinfo.xfocal
fiber.yfocal = objinfo.yfocal
fiber.mag = objinfo.mag
fiber.starl = objinfo.starl
fiber.devaucl = objinfo.devaucl
fiber.expl = objinfo.expl
fiber.primtarget = objinfo.primtarget
fiber.sectarget = objinfo.sectarget
fiber.targettype = objinfo.objtype

fiber.wl = wl
fiber.flux = flux

if fiber.spectrotype ne 'GALAXY ' then return
if avg(flux) eq 0.0 then return

;*******************************************************************************
; Fit continuum, continuum subtract, & create error & fit arrays
;*******************************************************************************

continuum = smooth(medsmooth(flux, 150), 50, /edge_truncate)
fiber.fit = continuum

;*******************************************************************************
; Print column titles to screen
;*******************************************************************************

angstrom = string("305b) ;"
fid = strtrim(string(fiber.fiberid), 2)

label = [fid, 'LINE', 'EQWIDTH', 'EQ_ERR', 'EM_EQW', 'FLUX', 'FLUX_ERR', $
         'EM_FWHM', 'S/N', 'ITER']
print, ' '
print, format = '(A4,A7,A9,A9,A8,A9,A10,A8,A7,A6)', label
print, '---------------------------------------------------------------' + $
       '---------------' 

if not keyword_set(noplot) then !P.MULTI = [0,4,7]

;*******************************************************************************
; Loop through lines one by one, fitting, printing, & plotting output
;*******************************************************************************

nlines = n_elements(linepars.line)
first_tag = where(tag_names(fiber) eq linepars[0].line)

for i = 0, nlines - 1 do begin
  linedata = {ldata}

  ;*****************************************************************************
  ; Get linefitting regions
  ;*****************************************************************************

  lowerbound = linepars[i].llim
  upperbound = linepars[i].ulim
  local = where(wl le upperbound and wl ge lowerbound)
  localc = where(wl le (upperbound + 50) and wl ge (lowerbound-50))

  title=linepars[i].line + '(' + strn(linepars[i].centr) + ')'
  title = string(title, format='(A30)')
  
  ;*****************************************************************************
  ; Line region exists  
  ;*****************************************************************************

  if (n_elements(local) gt 10) then begin  

    ;***************************************************************************
    ; Compute local error in continuum fit  
    ;***************************************************************************

    meanclip, flux[localc] - continuum[localc], cmean, cerr, maxiter = 10
 
    ;***************************************************************************
    ; Plot data & errors  
    ;***************************************************************************

    if not keyword_set(noplot) then begin  
      plot, wl - linepars[i].centr, flux, xrange=[-25, 25], psym = 10, $
            charsize = 1.75, title = title, $
            yrange = [0.9 * min(flux[local]), 1.1 * max(flux[local])]

      oplot, wl - linepars[i].centr, continuum
      oplot, wl - linepars[i].centr, continuum - sqrt(err^2 + cerr^2), $
             color = !dgray
      oplot, wl - linepars[i].centr, continuum + sqrt(err^2 + cerr^2), $
             color = !dgray
      oplot, wl - linepars[i].centr, continuum - err, color = !dgray
      oplot, wl - linepars[i].centr, continuum + err,  color = !dgray
    endif
   
    ;***************************************************************************
    ; Do actual line fitting, & save fit spectrum
    ;***************************************************************************

    mpgaussfit, wl[local], flux[local], err[local], continuum[local], cerr, $
                linepars[i], linedata = linedata, fit = fit, noplot = noplot

    fiber.fit[local] = fiber.fit[local] + fit

    ;***************************************************************************
    ; Save linefit in appropriate structure -- advance 1 if balmer line
    ;***************************************************************************

    fiber.(first_tag[0] + i) = linedata[0]
    if linepars[i].type eq 'both' then begin
      i = i + 1
      fiber.(first_tag[0] + i) = linedata[1]
    endif

  ;*****************************************************************************
  ; Line region does not exist -- make empty plot
  ;*****************************************************************************
     
  endif else begin

    if not keyword_set(noplot) then begin
      plot,[-25, 25], [0,1], title = tile, charsize=1.75, /nodata
      xyouts, -5, 0.5, "No Data"
    endif

  endelse    

endfor

print, ' '

;*******************************************************************************
; Plot full spectrum with continuum fit over top
;*******************************************************************************

if not keyword_set(noplot) then begin
  ll = avg(continuum)-2.*stddev(flux) > min(continuum) - 1.*stddev(flux)
  ul = avg(continuum)+3.*stddev(flux)
 
  plot, wl, flux, charsiz=1.75, yrange=[ll,ul], ystyle=1, $
        xrange = [min(wl),max(wl)], xstyle=1, ytitle = 'Counts', $
        xtitle = 'Rest Wavelength' + ' (' + angstrom + ')', $
        position=[.10,.05,.95,.14], /norm, title = 'fiber = ' + strn(fid) + $
        '  (z = ' + string(fiber.z, format = '(f5.3)') + ')' 
 
  oplot, wl, continuum,color=!red
endif

;*******************************************************************************
; Measure line indicies
;*******************************************************************************

first_tag = where(tag_names(fiber) eq strupcase(indexpars[0].index))

for i = 0, n_elements(indexpars.index) - 1 do begin
  indexdata = {idata}

  lowerbound = indexpars[i].blue_continuum[0]
  upperbound = indexpars[i].red_continuum[1]
  local = where(wl le upperbound + 5 and wl ge lowerbound - 5)

  if local[0] ne -1 then begin
    ok = wl[min(local)] le lowerbound and wl[max(local)] ge upperbound
  endif else ok = 0

  if ok then lineindex,  wl[local], flux[local], err[local], indexpars[i], $
     indexdata = indexdata

  fiber.(first_tag[0] + i) = indexdata
endfor

;*******************************************************************************
; Measure 4000 A break
;*******************************************************************************

minus1 = value_to_index(wl, 3750)
minus2 = value_to_index(wl, 3950)
plus1 = value_to_index(wl, 4050)
plus2 = value_to_index(wl, 4250)

fiber.D4000 = (wl[minus2] - wl[minus1]) / (wl[plus2] - wl[plus1])  * $
              total(flux[plus1:plus2]) / total(flux[minus1:minus2]) 
 
;*******************************************************************************

end

