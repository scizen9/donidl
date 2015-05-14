pro glga_sdss_fix_wcs, host, ra, update_jpg=update_jpg
;
flts=['u','g','r','i','z']
for i=0,4 do begin
	wcs_flip,host+'_'+flts[i]+'.fits'
	wcs_flip,host+'_'+flts[i]+'_ivar.fits'
endfor
if not file_test('bad_wcs',/directory) then $
	file_mkdir,'bad_wcs'
spawn,'mv *.fits_* bad_wcs'

if keyword_set(update_jpg) then $
	glga_make_jpg_set,host,ra,/sdss,/update
;
return
end
