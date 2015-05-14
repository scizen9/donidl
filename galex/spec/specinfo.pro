;Computes the GALEX magnitude of each CALSPEC source
;
;morrissey, 2/22/07

fn = findfile('*.fits')

openw,unit,'GLX_CalSpec_Mags.txt',/get_lun
printf,unit,format='(A30,2(A10))','File','FUV mab','NUV mab'

for i=0,n_elements(fn)-1 do begin
   ;HST Reference Info
   hst = mrdfits(fn(i),1,/silent)
   fuv = float(readcols('/users/patrick/galex/35FlightCal/GroundCal/EA-fuv_im.tbl',2,1))
   nuv = float(readcols('/users/patrick/galex/35FlightCal/GroundCal/EA-nuv_im.tbl',2,1))

   if min(hst.wavelength) lt 1400 then begin
      ;Convert f_lam to f_nu,
      ;then determine Mab through GALEX bandpass
      dlam      = hst.wavelength - shift(hst.wavelength,1)
      dlam(0)   = dlam(1)
      hst_nu    = 2.99792458e18/hst.wavelength ;Hz
      dnu       = shift(hst_nu,1) - hst_nu
      dnu(0)    = dnu(1)
      dlognu    = shift(alog10(hst_nu),1) - alog10(hst_nu)
      dlognu(0) = dlognu(1)
      dlognu    = median(dlognu,3)
      dlnnu     = shift(alog(hst_nu),1) - alog(hst_nu)
      dlnnu(0)  = dlnnu(1)
      dlnnu     = median(dlnnu,3)

      ;Compute reference magnitudes   
      hst_flam  = hst.flux
      hst_fnu   = hst_flam*(dlam/dnu)
      hst_mab   = -2.5*alog10(hst_fnu)-48.60 ;Oke 1974
      fuv_pass  = interpol(fuv(1,*),fuv(0,*),hst.wavelength)
      nuv_pass  = interpol(nuv(1,*),nuv(0,*),hst.wavelength)
      reffuvmab = -2.5*alog10(total(dlognu*hst_fnu*fuv_pass)/total(dlognu*fuv_pass))-48.60
      refnuvmab = -2.5*alog10(total(dlognu*hst_fnu*nuv_pass)/total(dlognu*nuv_pass))-48.60
      fuvefflam = exp(total(dlnnu*fuv_pass*alog(hst.wavelength))/total(dlnnu*fuv_pass))
      fuvfwidth = sqrt(total(dlnnu*fuv_pass*(alog((hst.wavelength)/fuvefflam))^2)/total(dlnnu*fuv_pass))
      nuvefflam = exp(total(dlnnu*nuv_pass*alog(hst.wavelength))/total(dlnnu*nuv_pass))
      nuvfwidth = sqrt(total(dlnnu*nuv_pass*(alog((hst.wavelength)/nuvefflam))^2)/total(dlnnu*nuv_pass))

      printf,unit,format='(A30,2(F10.3))',fn(i),reffuvmab,refnuvmab
   endif

endfor

free_lun,unit

end
