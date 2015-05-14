
;******************************************************************************
pro platefit, plate, subset = subset, interact = interact, noplot = noplot, $
             mjd = mjd, save = save, outfile = outfile 
;******************************************************************************

if (n_params() LT 1) then begin
  print, 'Syntax: platefit, plate, [subset=, /interact, /noplot, mjd=, out= ]'
  return
endif

starttime = systime(1)

;******************************************************************************
; Hard coded variables
;******************************************************************************

linelist = '/home/cat/idl/pipeline/linelist.txt'
indexlist = '/home/cat/idl/pipeline/indexlist.txt'
datadir = '/home/cat/plates'

;linelist = '/home/mseibert/bin/myidlpro/pipeline/linelist.txt'
;datadir = '/home/mseibert/ssb/plates'

;linelist = '/home/tremonti/idl/sdss/pipeline/linelist.txt'
;indexlist = '/home/tremonti/idl/sdss/pipeline/indexlist.txt'
;datadir = '/home/tremonti/sdss'

window_xsize = 800
window_ysize = 700

;setplot,'ps'
;device,filename='spec.ps',$
;xsize=21,ysize=26,xoffset=0,yoffset=0,/color

;******************************************************************************
; Determine names of fits files from plate 
;******************************************************************************

platestr = string(plate, format = '(i4.4)') 
datafile = findfile(datadir+ '/' + platestr + '/' + 'spPlate-' + platestr + '*')
binfile = findfile(datadir + '/' + platestr + '/' + 'spZbest-' + platestr + '*')

;******************************************************************************
; Read in fluxes, errs, & binary structure file
;******************************************************************************

flux = mrdfits(datafile[0], 0, hdr, /silent) ; * 1e-17

inverse_variance = mrdfits(datafile[0], 1, /silent) 
i = where(inverse_variance gt 0)
flux_err = 0 * inverse_variance
if (i[0] ne -1) then flux_err[i] = 1.0 / sqrt(inverse_variance[i]); * 1e-17
flux_err[where(flux_err eq 0.0)] = 9999.99

objinfo = mrdfits(datafile[0], 5, /silent)
zinfo = mrdfits(binfile[0], 1, /silent)

;******************************************************************************
; Make twod array of wavelengths for each spectrum
;******************************************************************************

npix = sxpar(hdr, 'NAXIS1')
nfib = sxpar(hdr, 'NAXIS2')
wl0 = sxpar(hdr, 'COEFF0')
dw = sxpar(hdr, 'COEFF1')

logwl = findgen(npix) * dw + wl0
wl = rebin(10.0^logwl, npix, nfib)
restwl = wl / (1 + rebin(transpose(zinfo.z), npix, nfib))

;******************************************************************************
; Read in info about lines that you want to fit 
;******************************************************************************

readcol, linelist, line, ctr, low, hi, typ, format = 'A,F,F,F,A', /silent

lineinfo = {linfo, line: ' ', centr: 0.0, llim: 0.0, ulim: 0.0, type: ' ' }
nlines = n_elements(line)
linepars = replicate(lineinfo, nlines)
;linepars = {lpars, pars: replicate(lineinfo, nlines)}

linepars.line = line
linepars.centr = ctr
linepars.llim = low
linepars.ulim = hi
linepars.type = typ

;******************************************************************************
; Read info about indicies
;******************************************************************************

readcol, indexlist, index, wli, wlf, bc_wli, bc_wlf, rc_wli, rc_wlf, units, $
         format = 'A,F,F,F,F,F,F,A', /silent

indexinfo = {iinfo, index: ' ', bandpass: [0.0, 0.0], $
             blue_continuum: [0.0, 0.0], red_continuum: [0.0, 0.0], units: ' '} 
nindicies = n_elements(index)
indexpars = replicate(indexinfo, nindicies)

indexpars.index = index
indexpars.bandpass = transpose([[wli], [wlf]])
indexpars.blue_continuum = transpose([[bc_wli], [bc_wlf]])
indexpars.red_continuum = transpose([[rc_wli], [rc_wlf]])
indexpars.units = units

;******************************************************************************
; Create structures
;******************************************************************************

linedata = {ldata, continuum: 0.0,  cerr: 0.0, $
            eqwidth: 0.0, eqwidth_err: 0.0, ctr: 0.0, ctr_err: 0.0, $
            fwhm: 0.0, fwhm_err: 0.0, flux: 0.0, flux_err:0.0, $
            geqwidth: 0.0, geqwidth_err: 0.0, sn: 0.0}

indexdata = {idata, value: 0.0, err: 0.0}

fiber = {fiberid: 0, photoid: [0,0,0,0,0], ra: 0.0, dec: 0.0, xfocal: 0.0, $
         yfocal: 0.0, mag: [0.0, 0.0, 0.0, 0.0, 0.0], starl: 0.0, $
         devaucl: 0.0, expl: 0.0, primtarget: 0, sectarget: 0, $
         targettype: ' ', spectrotype: ' ', subclass: ' ', z: 0.0}

for i = 0, nlines - 1 do fiber = create_struct(fiber, line[i], linedata)
for i = 0, nindicies - 1 do fiber = create_struct(fiber, index[i], indexdata)
fiber = create_struct(fiber, 'D4000', 0.0)

empty_arr = fltarr(3870) + 'NaN'
fiber = create_struct(name = 'onefiber', fiber, 'flux', empty_arr, $ 
                      'wl', empty_arr, 'fit', empty_arr)
     
plate = {oneplate, plateid: 0, tile: 0, mjd: 0L, date: ' ', altitude: 0.0, $
           ra: 0.0, dec: 0.0, fiber: replicate(fiber, 641)}

;******************************************************************************
; Populate plate specific structs 
;******************************************************************************

plate.plateid = fix(sxpar(hdr, 'PLATEID'))
plate.tile = fix(sxpar(hdr, 'TILEID'))
plate.mjd = long(sxpar(hdr, 'MJD'))
plate.date = sxpar(hdr, 'DATE-OBS')
plate.altitude = float(sxpar(hdr, 'ALT'))
plate.ra = float(sxpar(hdr, 'RADEG'))
plate.dec = float(sxpar(hdr, 'DECDEG'))

;******************************************************************************
; Set up plot window
;******************************************************************************

if not keyword_set(noplot) then begin
  loadct, 39
  window, 0, xsize = window_xsize, ysize = window_ysize
  setplotcolors
  !p.background = 255
  !color = 0
endif

;******************************************************************************
; Loop through all galaxies (remember IDL is zero indexed ...)
;******************************************************************************

if not keyword_set(subset) then subset = indgen(640) + 1
n = n_elements(subset)

for i = 0, n - 1 do begin
  j = subset[i]
  newfiber = {onefiber}
  newfiber.spectrotype = zinfo[j - 1].class
  newfiber.subclass = zinfo[j - 1].subclass
  newfiber.z = zinfo[j - 1].z
  newfiber.wl = 'NAN' 
  newfiber.flux = 'NAN' 
  newfiber.fit = 'NAN' 

  fiberfit, restwl[*,j - 1], flux[*,j - 1], flux_err[*,j - 1], $
            objinfo[j - 1], linepars, indexpars, noplot = noplot, $
            data = newfiber, interact = interact 
  plate.fiber(j) = newfiber

  if keyword_set(interact) then begin
    cursor, x, y, 3
    IF !mouse.button ne 1 then return
  endif
endfor

print, 'RUN TIME = ' + string((systime(1) - starttime) / 60, $
  format = '(F6.3)') + ' minutes'

if keyword_set(save) then save, plate, filename = 'plate' + platestr + '.sav'

outfile = plate

!P.MULTI = 0

;devise, /close
;set_plot, 'x'

end
