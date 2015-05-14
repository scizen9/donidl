pro wcs_flip, file,silent=silent,force=force
;+
; wcs_flip - flip wcs in image so x is on RA and y is on Dec
;-
if not file_test(file) then begin
	if not keyword_set(silent) then $
		print,'File not found: ',file
	return
endif
;
inim = mrdfits(file,0,hdr,silent=silent)
extast,hdr,astr,err
if astr.cd[0] lt 0. and not keyword_set(force) then begin
	if not keyword_set(silent) then $
		print,'Left-handed coords (correct), use /force to flip'
	return
endif
;
oim  = transpose(inim)
ohdr = hdr
sxaddpar,ohdr,'CTYPE1',sxpar(hdr,'CTYPE2')
sxaddpar,ohdr,'CUNIT1',sxpar(hdr,'CUNIT2')
sxaddpar,ohdr,'CRVAL1',sxpar(hdr,'CRVAL2')
sxaddpar,ohdr,'CRPIX1',sxpar(hdr,'CRPIX2')
sxaddpar,ohdr,'CD1_1' ,sxpar(hdr,'CD2_2')
sxaddpar,ohdr,'CD1_2' ,sxpar(hdr,'CD2_1')
;
sxaddpar,ohdr,'CTYPE2',sxpar(hdr,'CTYPE1')
sxaddpar,ohdr,'CUNIT2',sxpar(hdr,'CUNIT1')
sxaddpar,ohdr,'CRVAL2',sxpar(hdr,'CRVAL1')
sxaddpar,ohdr,'CRPIX2',sxpar(hdr,'CRPIX1')
sxaddpar,ohdr,'CD2_1' ,sxpar(hdr,'CD1_2')
sxaddpar,ohdr,'CD2_2' ,sxpar(hdr,'CD1_1')
;
filestamp,file
mwrfits,oim,file,ohdr
if not keyword_set(silent) then $
	print,'Wrote fits file: ',file
;
return
end
