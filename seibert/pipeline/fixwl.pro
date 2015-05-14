
pro fixwl, plate, outfile

;*******************************************************************************
; Read in plate & z file
;*******************************************************************************

datadir = '/data1/sx/sdssdata/spectro/2d_v4/'
platestr = string(plate, format = '(i4.4)') 
datafile = findfile(datadir + platestr + '/' + 'spPlate-' + platestr + '*')
binfile = findfile(datadir + platestr + '/' + 'spZbest-' + platestr + '*')

flux = mrdfits(datafile[0], 0, hdr, /silent) ; * 1e-17
zinfo = mrdfits(binfile[0], 1, /silent)

;*******************************************************************************
; De-redshift
;*******************************************************************************

npix = sxpar(hdr, 'NAXIS1')
nfib = sxpar(hdr, 'NAXIS2')
wl0 = sxpar(hdr, 'COEFF0')
dw = sxpar(hdr, 'COEFF1')

logwl = findgen(npix) * dw + wl0
wl = rebin(10.0^logwl, npix, nfib)
restwl = wl / (1 + rebin(transpose(zinfo.z), npix, nfib))

;*******************************************************************************
; Resample data to linear wavelength grid
;*******************************************************************************

linearwl = findgen(11000) / 2.0  + 3600
flx = fltarr(11000, 640)
for i = 0, 639 do begin
  flx[*,i] = interpol(flux[*,i], restwl[*,i], linearwl)
endfor
              
;*******************************************************************************
; Write array to fits file with correct header pars
;*******************************************************************************

hdr = strarr(5)
hdr[0] = "CRVAL1  =        3600 /Central wavelength of first pixel"
hdr[1] = "CD1_1   =        0.50 /Dispersion per pixel"
hdr[2] = "CRPIX1  =           1 /Starting pixel (1-indexed)"
hdr[3] = "CTYPE1  = 'LINEAR  '  /"
hdr[4] = "DC-FLAG =           0 /Log-linear flag"

mwrfits, flx, outfile, hdr, /create

end

