pro get_imqual,fspec,noout=noout
;
fwhm=0.
back=0.
flist = findfile(fspec,count=nfound)
if nfound gt 0 then begin
	if not keyword_set(noout) then begin
		openw,olun,'imqual.list',/get_lun
		printf,olun,'# GET_IMQUAL: '+systime(0)
	endif
	for i=0,nfound-1 do begin
		;im=readfits(flist(i),h)
		irafread,im,h,flist(i)
		exptime = sxpar(h,'EXPTIME')
		get_imstats,im,flist(i),fwhm,back,nf
		if not keyword_set(noout) then $
		    printf,olun,flist(i),fwhm,nf,back,exptime,$
		    	form='(a20,f9.2,i4,2f9.2)'
		print,i+1,'/',nfound,flist(i),fwhm,nf,back,exptime, $
			form='(i3,a,i3,2x,a20,f9.2,i4,2f9.2)'
		print,' '
	endfor
	if not keyword_set(noout) then free_lun,olun
endif
;
return
end	; pro get_imqual
