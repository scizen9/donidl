pro mktp_script
;+
; make a astrometry.net solve-field script for tpoint images
;-
flist = file_search('rc*.fits', count=nf)
;
openw,ol,'tp_solve.csh',/get_lun
printf,ol,'# MKPT_SCRIPT - run: '+systime(0)
;
for i=0,nf-1 do begin
	h=headfits(flist[i])
	ra = sxpar(h,'RA')
	dec = sxpar(h,'DEC')
	printf,ol,"solve-field --ra '"+ra+"' --dec '"+dec+ $
		"' --radius 0.2 "+flist[i]
endfor
free_lun,ol
return
end
