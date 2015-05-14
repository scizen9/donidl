pro get_fwhms,mxl,fspec,standards=standards,individual=individual
;
if n_params(0) lt 1 then begin
	print,'get_fwhms, max_limit [filespec, /standards, /individual]'
	return
endif
;
if n_params(0) lt 2 then begin
	fspec = '*.imh'
	append = (1 eq 0)
endif else $
	append = (1 eq 1)
;
if strpos(fspec,'imh',/REVERSE_SEARCH) gt 0 then $
	fitsio = (1 eq 0) $
else	fitsio = (1 eq 1)
;
flist = findfile(fspec,count=nfound)
print,nfound,' files found.'
;flist = findfile('*.fits',count=nfound)
if nfound gt 0 then begin
	if append then $
		openw,olun,'fwhms.list',/get_lun,/append $
	else	openw,olun,'fwhms.list',/get_lun
	if append then $
		openw,blun,'bad.list',/get_lun,/append $
	else	openw,blun,'bad.list',/get_lun
	printf,olun,'# GET_FWHMS: '+systime(0)+'  MAXL: ',strn(mxl)
	printf,blun,'# GET_FWHMS (BAD GUYS): '+systime(0)+'  MAXL: ',strn(mxl)
	for i=0,nfound-1 do begin
		if (fitsio) then $
			im=readfits(flist(i),h) $
		else	irafread,im,h,flist(i)
		exptime = sxpar(h,'EXPTIME')
		if keyword_set(standards) then $
			get_fwhm2,im,fwhm,nstar,maxlim=mxl,backg=back, $
				backsig=bsig,nsig=0.5 $
		else	get_fwhm2,im,fwhm,nstar,maxlim=mxl,backg=back, $
				backsig=bsig
		if fwhm gt 0.0 and fwhm lt 20.0 then begin
			printf,olun,flist(i),fwhm,exptime,back,bsig,nstar, $
				form='(a,4f9.2,i5)'
			print,i+1,'/',nfound,flist(i),fwhm,exptime,back,bsig, $
				nstar, form='(i3,a,i3,2x,a,4f9.2,i5)'
			if keyword_set(individual) then begin
			    fl = flist(i)
			    root = gettok(fl,'.')
			    openw,ilun,root+'.fwhm',/get_lun
			    printf,ilun,fwhm,form='(f9.2)'
			    free_lun,ilun
			endif
		endif else begin
			print,i+1,'/',nfound,flist(i),fwhm,' BAD!! ',nstar, $
				form='(i3,a,i3,2x,a,f9.2,a,i5)'
			printf,blun,i+1,'/',nfound,flist(i),fwhm,' BAD!! ', $
				nstar, form='(i3,a,i3,2x,a,f9.2,a,i5)'
		endelse
	endfor
	free_lun,olun,blun
endif
;
return
end	; pro get_fwhms
