
pro pfitstab, plate, outstruct = p, stats = stats

;*******************************************************************************
; Add plate specific tags to new structure
;*******************************************************************************

ptags = tag_names(plate)
ptags[where(ptags eq 'RA')] = 'PLATE_RA'
ptags[where(ptags eq 'DEC')] = 'PLATE_DEC'
np = n_elements(ptags) - 1

platedata = create_struct(ptags[0], plate.(0))
for i = 1, np - 1 do  $
  platedata = create_struct(platedata, ptags[i], plate.(i))

;*******************************************************************************
; Add fiber specific tags 
;*******************************************************************************

ftags = tag_names(plate.fiber)
ltagi = where(ftags eq 'OII_3727') & ltagi = ltagi[0]
ltagf = where(ftags eq 'SII_6731') & ltagf = ltagf[0]

for i = 0, ltagi - 1 do  $
  platedata = create_struct(platedata, ftags[i], plate.fiber[0].(i))

platedata = create_struct(platedata, 'e_bv', 0.0)
platedata = create_struct(platedata, 'e_bv_err', 0.0)

;*******************************************************************************
; Add line specific tags 
;*******************************************************************************

nlines = n_elements(ftags[ltagi:ltagf])

for i = ltagi, ltagf do begin
  platedata = create_struct(platedata, $
              ftags[i] + '_ctr',    0.0, ftags[i] + '_ctr_err',    0.0, $
              ftags[i] + '_fwhm',   0.0, ftags[i] + '_fwhm_err',   0.0, $
              ftags[i] + '_flux',   0.0, ftags[i] + '_flux_err',   0.0, $
              ftags[i] + '_geqw',   0.0, ftags[i] + '_geqw_err',   0.0, $
              ftags[i] + '_cont',   0.0, ftags[i] + '_cont_err',   0.0, $
              ftags[i] + '_eqw',    0.0, ftags[i] + '_eqw_err',    0.0, $
              ftags[i] + '_drflux', 0.0, ftags[i] + '_drflux_err', 0.0)
endfor

;*******************************************************************************
; Add index specific tags 
;*******************************************************************************

itagf = where(ftags eq 'D4000') & itagf = itagf[0]

for i = ltagf + 1, itagf do begin
  platedata = create_struct(platedata, ftags[i], 0.0, ftags[i] + '_err', 0.0)
endfor

;*******************************************************************************
; Create Array of structures
;*******************************************************************************

gal = where(plate.fiber.spectrotype eq 'GALAXY ')
ngal = n_elements(gal)
p = make_array(value = platedata, dimension = ngal)

;*******************************************************************************
; Populate Structures
;*******************************************************************************

for i = 0, ltagi - 1 do p.(np + i) = plate.fiber[gal].(i)

j = np + ltagi + 2
for i = ltagi, ltagf do begin
   p.(j + 0) = plate.fiber[gal].(i).ctr
   p.(j + 1) = plate.fiber[gal].(i).ctr_err
   p.(j + 2) = plate.fiber[gal].(i).fwhm
   p.(j + 3) = plate.fiber[gal].(i).fwhm_err
   p.(j + 4) = plate.fiber[gal].(i).flux
   p.(j + 5) = plate.fiber[gal].(i).flux_err
   p.(j + 6) = plate.fiber[gal].(i).geqwidth
   p.(j + 7) = plate.fiber[gal].(i).geqwidth_err
   p.(j + 8) = plate.fiber[gal].(i).continuum
   p.(j + 9) = plate.fiber[gal].(i).cerr
   p.(j + 10) = plate.fiber[gal].(i).eqwidth
   p.(j + 11) = plate.fiber[gal].(i).eqwidth_err
   j = j + 14
endfor

for i = ltagf + 1, itagf - 1 do begin
   p.(j + 0) = plate.fiber[gal].(i).value
   p.(j + 1) = plate.fiber[gal].(i).err
  j = j + 2
endfor

p.(j) = plate.fiber[gal].D4000

;*******************************************************************************; Compute E(B - V) from H_alpha & H_beta
;*******************************************************************************

noabs = where(p.H_alpha_abs_flux eq 0.0 and p.H_alpha_em_flux gt 0.0)
H_alpha_abs = p.H_alpha_abs_flux
H_alpha_abs_err = p.H_alpha_abs_flux_err
H_alpha_abs[noabs] = -0.90 * p[noabs].H_beta_abs_geqw * $
                     p[noabs].H_alpha_abs_cont
H_alpha_abs_err[noabs] = -0.90 * p[noabs].H_beta_abs_geqw_err * $
                         p[noabs].H_alpha_abs_cont
H_alpha_em = p.H_alpha_em_flux
H_alpha_em_err = p.H_alpha_em_flux_err
H_alpha_em[noabs] = H_alpha_em[noabs] - H_alpha_abs[noabs]
H_alpha_em_err[noabs] = sqrt(H_alpha_em_err[noabs]^2 + H_alpha_abs_err[noabs]^2)

Tau_balmer = alog((H_alpha_em/p.H_beta_em_flux)/2.86)
ratio_err = (H_alpha_em/p.H_beta_em_flux) * $
            sqrt((H_alpha_em_err/H_alpha_em)^2 + $
                 (p.H_beta_em_flux_err/p.H_beta_em_flux)^2)
Tau_balmer_err = ratio_err / (H_alpha_em/p.H_beta_em_flux) 

ccm_unred, 6564.63, 1.0, 2.5, tenk
k_ha = alog10(tenk)

ccm_unred, 4862.69, 1.0, 2.5, tenk
k_hb = alog10(tenk)

E_BV = 1.086 * Tau_balmer / (k_hb[0] - k_ha[0])
E_BV_err = 1.086 * Tau_balmer_err / (k_hb[0] - k_ha[0])

bad = where(finite(E_BV) eq 0 or E_BV lt 0)
E_BV[bad] = 0.0
E_BV_err[bad] = 0.0

p.e_bv = E_BV
p.e_bv_err = E_BV_err

;*******************************************************************************
; De-redden emission line fluxes
;*******************************************************************************

ebvok = where(E_BV ne 0.0)
wl = fltarr(nlines)
flux = fltarr(nlines)
for i = 0, n_elements(ebvok) - 1 do begin
  j = ebvok[i]
  ctrs = where(strpos(tag_names(p[0]), '_CTR_ERR') ne -1) - 1
  flx = where(strpos(tag_names(p[0]), '_FLUX_ERR') ne -1) - 1
  drf = where(strpos(tag_names(p[0]), '_DRFLUX_ERR') ne -1) - 1
  for k = 1, nlines - 1 do begin
    wl[k] = p[j].(ctrs[k])
    flux[k] = p[j].(flx[k])
  endfor
  ccm_unred, wl, flux, E_BV[j], funred
  ccm_unred, wl, flux, E_BV_err[j], funred_err
  for k = 0, nlines - 1 do begin
    p[j].(drf[k]) = funred[k]
    p[j].(drf[k] + 1) = sqrt(funred_err[k]^2 + p[j].(flx[k] + 1)^2)
  endfor
endfor

;*******************************************************************************
; Print plate stats
;*******************************************************************************

if keyword_set(stats) then begin
  ngal = where(p.f.class eq 'GALAXY ')

  print, 'Galaxies =', n_elements(ngal)
  print, 'H-alpha = ', n_elements(where(p.f.flux.H_alpha_em gt 0.0))
  print, 'OII = ', n_elements(where(p.f.flux.OII_3727 gt 0.0))
  print, 'H-alpha + H-beta =', n_elements(where(p.f.flux.H_alpha_em gt 0.0 $
         and p.f.flux.H_beta_em gt 0.0))
  print, 'H-alpha + H-beta + OII=', n_elements(where(p.f.flux.H_alpha_em $
          gt 0.0 and p.f.flux.H_beta_em gt 0.0 and p.f.flux.OII_3727 gt 0.0))
  print, 'H-alpha + H-beta + OII + OIII =', n_elements(where( $
          p.f.flux.H_alpha_em gt 0.0 and p.f.flux.H_beta_em gt 0.0 and $
          p.f.flux.OII_3727 gt 0.0 and p.f.flux.OIII_5007 gt 0.0))
  print, 'H-alpha + H-beta + OII + OIII + NII =', $
         n_elements(where(p.f.flux.H_alpha_em gt 0.0 and p.f.flux.H_beta_em $
         gt 0.0 and p.f.flux.OII_3727 gt 0.0 and p.f.flux.OIII_5007 gt $
         0.0 and p.f.flux.NII_6584 gt 0.0))

endif

;*******************************************************************************
; Save structure as biniary fits table
;*******************************************************************************

fitsfile = 'p' + string(p[0].plateid, format = '(i4.4)') + 'tab.fit'
mwrfits, p, fitsfile, /create

;*******************************************************************************
; Save data and fits to data as a fits file
;*******************************************************************************

wl = findgen(11000) / 2.0  + 3600
flux = fltarr(11000, 640)
fit = fltarr(11000, 640)
for i = 1, 640 do begin
  ok = where(finite(plate.fiber[i].wl))
  nwl = plate.fiber[i].wl[ok]
  nwl = [min(nwl) - 2, min(nwl) - 1, nwl, max(nwl) + 1, max(nwl) + 2]
  nfx = plate.fiber[i].flux[ok]
  nfxi = median(nfx[0:100])
  nfxf = median(nfx[n_elements(nfx) - 101:n_elements(nfx) - 1])
  nfx = [nfxi, nfxi, nfx, nfxf, nfxf]
  flux[*,i - 1] = interpol(nfx, nwl, wl)
  fit[*,i - 1] = interpol(plate.fiber[i].fit[ok], plate.fiber[i].wl[ok], wl)
endfor

hdr = strarr(5)
hdr[0] = "CRVAL1  =        3600 /Central wavelength of first pixel"
hdr[1] = "CD1_1   =        0.50 /Dispersion per pixel"
hdr[2] = "CRPIX1  =           1 /Starting pixel (1-indexed)"
hdr[3] = "CTYPE1  = 'LINEAR  '  /"
hdr[4] = "DC-FLAG =           0 /Log-linear flag"
              
fitsfile = 'p' + string(p[0].plateid, format = '(i4.4)') + '.dat.fit'
mwrfits, flux, fitsfile, hdr, /create
mwrfits, fit, fitsfile, hdr

end

