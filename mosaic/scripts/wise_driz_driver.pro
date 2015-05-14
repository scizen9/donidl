pro wise_driz_driver,lfile,update=update,clean=clean,shrink=shrink,nobp16=nobp16
;
; get list
readcol,lfile,names,sra,sdec,majdiam,form='a,d,d,f',com='#'
;
; 3 arcmin is minimum size
sz = (majdiam * 4.0)>3.
bg = where(majdiam ge 50., nbg)
if nbg gt 0 then sz[bg] = majdiam[bg] * 1.5
bg = where(majdiam lt 50. and majdiam ge 30., nbg)
if nbg gt 0 then sz[bg] = majdiam[bg] * 2.0
bg = where(majdiam lt 30 and majdiam ge 20., nbg)
if nbg gt 0 then sz[bg] = majdiam[bg] * 2.5
bg = where(majdiam lt 20 and majdiam ge 10., nbg)
if nbg gt 0 then sz[bg] = majdiam[bg] * 3.0
;
; list of spectral bands
band=['w1','w2','w3','w4']
nband = n_elements(band)
;
; pixel size
psc = 1.0
;
; directories
root = !GLGA_WISE_DATA + 'data/sort/'
;
; loop over hosts
ng=n_elements(names)
for ig=0,ng-1 do begin
    ;
    ; current coordinates
    ra=sra[ig]
    dec=sdec[ig]
    ;
    ; current directory
    dir=root+names[ig]+'/'
    ;
    ; does directory exist?
    if file_test(dir,/directory) then begin
      ;
      ; Do only if not processed already or requesting update
      if not file_test(dir+'driz1/'+names[ig]+'_w1.fit*') or $
	 keyword_set(update) then begin
	;
	; go to directory
	cd,dir,curr=cwd
	;
	; are there any files here?
	if file_test('*-int-1b.fit*') then begin
		;
		; gunzip if we've done this before
		spawn,'gunzip *-1b.*gz'
		;
		; get to work and print our status
		print,ig+1,'/',ng,names[ig],form='(i5,a1,i5,2x,a)'
		;
		; are we updating?
		if keyword_set(update) then begin
			spawn,'rm -rf icore_?.log driz?'
		endif
		;
		; get maximum image size based on image collection
		imsize = max(mosaic_max_imsize('./','*-int-1b.fit*',/verbose))
		;
		; size cannot exceed capacity of images
		size = min([sz[ig]*60.,imsize])	; size in arcseconds
		;
		; test if not resizing
        	if not keyword_set(shrink) then begin
	    		pxsiz = size / psc
	    		if pxsiz gt 3600 then $
		    	    print,'Warning - image will have dimensions: ', $
			    	pxsiz,' by ', pxsiz
			shfact = 1.0
		;
		; calculate shrink factor
        	endif else begin
			; automatic shrink calculation
			if shrink le 1. then begin
				shfact = 1.0
				pxsiz = size / psc
				; shrink until under maximum size
				while pxsiz gt 3600 do begin
					shfact = shfact + 1.0
					pxsiz = size / ( psc * shfact )
				endwhile
			; or use keyword shrink factor
			endif else begin
				shfact = shrink
			endelse
		endelse
		if shfact ne 1. then $
			print,'Shrink factor = ',shfact,form='(a,f7.2)'
		;
		; loop over bands
		for iband=0,nband-1 do begin
			print,'Processing ',band[iband],' data...'
			cmd = 'icore_driz_wise ' + names[ig] + ' ' + $
				string(ra,format='(f13.8)') + ' ' + $
				string(dec,format='(f13.8)') + ' ' + $
				string(size/3600.,format='(f8.5)') + ' ' + $
				strn(iband+1) + ' ' + strn(psc*shfact) + $
				' 2 | tee icore_' + strn(iband+1) + '.log'
			print,cmd
			spawn,cmd
			print,'Done processing ',names[ig],': ',band[iband], $
				' data.'
		endfor	; loop over bands
		if not keyword_set(nobp16) then $
			wise_driz_bp16,names[ig] $
		else	spawn,'gzip driz?/'+names[ig]+'_*.fits'
		if keyword_set(clean) then begin
			print,'Cleaning up...'
			spawn,'rm driz?/*.fits'
			print,'Done cleaning up.'
		endif
		spawn,'gzip *-1b.fits &'
	endif
	cd,cwd
      endif else print,dir+names[ig]+' already processed.'
    endif else print,'No directory for host: ',names[ig]
endfor	; loop over hosts
;
return
end
