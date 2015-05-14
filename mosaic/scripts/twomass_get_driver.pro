pro twomass_get_driver,lfile,verbose=verbose,nosizelim=nosizelim
;+
; twomass_get_driver - use the url below to fetch files in data/sort dir:
;  	http://irsa.ipac.caltech.edu/cgi-bin/2MASS/IM/nph-im_sia
; lfile - galaxy list with one entry per galaxy with the following columns:
;	id
;	ra,dec	- decimal degrees
;	majdiam,mindiam	- arcminutes
;	pa	- position angle in degrees
;	type	- galaxy Hubble type (string)
;-
; read input
readcol,lfile,id,ra,dec,majdiam,mindiam,pa,format='a,d,d,f,f,f'
nobj = n_elements(ra)	; number of galaxies
name = strtrim(id,2)	; trim names
;
; check size limit
if keyword_set(nosizelim) then $
	szlim = 1.e9 $
else	szlim = 60.
;
; convert from arcmin to degrees: 3 arcmin is min and szlim is max size
r=((majdiam*4.)>3.<szlim) / 60.
;
; log of images for each object
openw,ol,'objmultipos.list',/get_lun
;
; SIA url for IRSA
surl = '"http://irsa.ipac.caltech.edu/cgi-bin/2MASS/IM/nph-im_sia?FORMAT=image/fits&'
;
; loop over each object
for j=0L,nobj-1L do begin
	; print status
	printf,ol,name[j]
	print,name[j],j+1,' /',nobj,form='(a-25,2x,i5,a2,i5)'
	; sort directory
	sdir=!GLGA_2MASS_DATA+'data/sort/'+name[j]
	; make dir if doesn't exist
	if file_test(sdir) eq 0 then spawn,'mkdir '+sdir
	; go there
	cd,sdir,curr=cwd
	; build wget commed for file list
	cmd = 'wget -O - '+surl+'POS='+ $
		strtrim(string(ra[j],form='(f13.8)'),2)+','+$
		strtrim(string(dec[j],form='(f13.8)'),2) + '&SIZE=' + $
		strtrim(string(r[j],form='(f9.5)'),2) + '"'
	; file list goes into out
	spawn,cmd,out,err
	; find records with url for fits files
	t=where(strpos(out,'CDATA') gt 0 and strpos(out,'fits') gt 0, nt)
	; loop over fits file urls
	for i=0,nt-1 do begin
		; trim record
		rec = out[t[i]]
		for k=0,1 do jnk = gettok(rec,'[')
		furl = gettok(rec,']')
		; get output fits file name
		fits = strmid(furl,strpos(furl,'name=')+5)+'.gz'
		; record file
		printf,ol,fits
		; have we downloaded it before?
		if file_test(fits) eq 0 then begin
			cmd = 'wget -O '+fits+' "'+furl+'"'
			if keyword_set(verbose) then $
				spawn,cmd,jnk $
			else	spawn,cmd,jnk,err
			print,i+1,'/ ',nt,' downloaded: ',fits, $
				form='(i4,a2,i4,a,a)'
		endif else print,'previously downloaded: ',fits
	endfor
	cd,cwd
endfor
;
free_lun,ol
;
return
end
