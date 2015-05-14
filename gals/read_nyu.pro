function read_nyu,silent=silent,verbose=verbose
;+
;	read_nyu - read the NYU value-added catalog struct (currently DR6)
;-
; failure to read setting
sdss = -1
;
; get SDSS integrated host data from NYU value-added catalog
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Reading lowz_plus_ned.dr6.fits'
if file_test(!SDSS_DATA+'lowz_plus_ned.dr6.fits') then begin
	sdss=mrdfits(!SDSS_DATA+'lowz_plus_ned.dr6.fits',1,slhdr,/sile)
endif else print,'lowz_plus_ned.dr6.fits not found'
;
return,sdss
end
