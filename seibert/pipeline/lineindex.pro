 
;*******************************************************************************
pro lineindex, wavescale, flux, err, indexinfo, indexdata = indexdata
;*******************************************************************************

;*******************************************************************************
; Find average red & blue continuum fluxes
;*******************************************************************************

blue = where(wavescale gt indexinfo.blue_continuum[0] and $
             wavescale lt indexinfo.blue_continuum[1])
wl_blue = avg(wavescale[blue])
f_blue_cont = avg(flux[blue])
err_blue_cont = sqrt(total(err[blue]^2)) / n_elements(blue)

red = where(wavescale gt indexinfo.red_continuum[0] and $
             wavescale lt indexinfo.red_continuum[1])
wl_red = avg(wavescale[red])
f_red_cont = avg(flux[red])
err_red_cont = sqrt(total(err[red]^2)) / n_elements(red)

;*******************************************************************************
; Compute continuum for the index (different for BH than Lick & DTT)
;*******************************************************************************

lick = strpos(indexinfo.index, 'Lick_') & lick = lick[0] ne -1
dtt = strpos(indexinfo.index, 'DTT_') & dtt = dtt[0] ne -1
bh = strpos(indexinfo.index, 'BH_') & bh = bh[0] ne -1

if bh then begin
  f_cont = (f_red_cont + f_blue_cont) / 2.0
  err_cont = sqrt(err_red_cont^2 + err_blue_cont^2) / 2.0
endif else begin
  wl0 = wavescale[0]
  coef = svdfit([wl_blue, wl_red] - wl0, [f_blue_cont, f_red_cont], $
         2, weights = 1.0/[err_blue_cont, err_red_cont]^2, sigma = sigma)

  f_cont = coef[0] + coef[1] * (wavescale - wl0)
  err_cont = sqrt(sigma[0]^2 + sigma[1]^2 * (wavescale - wl0)^2)
endelse

;*******************************************************************************
; Compute equivalent width 
;*******************************************************************************

bandpass = where(wavescale gt indexinfo.bandpass[0] and $
                 wavescale lt indexinfo.bandpass[1])

dw = wavescale - shift(wavescale, 1) 
dw[0] = dw[1]-(dw[2]-dw[1])

if indexinfo.units eq 'A' then begin
  eqwidth = total((1 - flux[bandpass]/f_cont[bandpass]) * dw[bandpass])
  nerri =  (flux / f_cont) * sqrt((err / flux)^2 + (err_cont / f_cont)^2)
  eqwidth_err=sqrt(total(nerri[bandpass]^2 * dw[bandpass]^2))
  indexdata.value = eqwidth
  indexdata.err = eqwidth_err
endif

;*******************************************************************************
; Compute magnitudes
;*******************************************************************************

if indexinfo.units eq 'mag' then begin
  if lick then nflux = total(flux[bandpass]/f_cont[bandpass] * dw[bandpass]) $
  else nflux = total(flux[bandpass]/f_cont * dw[bandpass])
  wli = wavescale[min(bandpass)]
  wlf = wavescale[max(bandpass)]

  mag = -2.5 * alog10(nflux / (wlf - wli)) 

  nerri =  (flux / f_cont) * sqrt((err / flux)^2 + (err_cont / f_cont)^2)
  nerr =  sqrt(total(nerri[bandpass]^2 * dw[bandpass]^2)) 
  mag_err = 2.5 / alog(10) * nerr / nflux

  indexdata.value = mag
  indexdata.err = mag_err
endif

;*******************************************************************************
; Plot (for debugging)
;*******************************************************************************

;plot, wavescale, flux
;oplot, wavescale, flux - abs(err), linestyle = 1
;oplot, wavescale, flux + abs(err), linestyle = 1
;oplot, wavescale[blue], flux[blue], color = 50
;oplot, wavescale[red], flux[red], color = 250
;oplot, [wl_blue, wl_red], [f_blue_cont, f_red_cont], psym = 4
;oploterr, [wl_blue, wl_red], [f_blue_cont, f_red_cont], $
;          [err_blue_cont, err_red_cont]
;if n_elements(f_cont) gt 1 then begin
;  oplot, wavescale, f_cont
;  oplot, wavescale, f_cont - err_cont, linestyle = 3
;  oplot, wavescale, f_cont + err_cont, linestyle = 3
;endif
;
;wait, 5

;*******************************************************************************

end
