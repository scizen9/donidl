pro wise_atlas_driver,lfile,pxscl=pxscl,nolimit=nolimit, $
	update=update, shrink=shrink, clean=clean
;+
; wise_atlas_driver - create coadded wise images from atlas images
; lfile - list of hosts with basic info: 
;	one entry per line, ra, dec, pa in degress, diams in arcmin:
;	name ra dec majdiam, mindiam, pa
; pxscl - pixel scale in arcsec per pix (defaults to 0.5)
; nolimit - set to remove limit on number of images to combine (30)
; update - set to re-do existing mosaics
; shrink - set to shrink factor or 1.0 for automatic shrink
;-
; get list
readcol,lfile,names,sra,sdec,majdiam,mindiam,pa, $
	format='a,d,d,f,f,f'
nobj = n_elements(sra)
name = strtrim(names,2)
;
; check size limit
;
; 3 arcmin is minimum size
r=((majdiam*4.)>3.) / 60.
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
bandi = ['1','2','3','4']
nband = n_elements(band)
;
; pixel scale
if keyword_set(pxscl) then $
	psc = pxscl $
else	psc = 1.375
print,'Starting with pixel scale ("/px) of ',psc
;
; directories
root = !GLGA_WISE_DATA + 'data/sort/'
;
; loop over hosts
nobj=n_elements(names)
for ig=0,nobj-1 do begin
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
      if not file_test(dir+'atlas1/'+names[ig]+'_w1.fit*') or $
	 keyword_set(update) then begin
	;
	; go to directory
	cd,dir,curr=cwd
	;
	; are there any files here?
	if file_test('*-int-3.fit*') then begin
		;
		; get to work and print our status
		print,ig+1,'/',nobj,names[ig],form='(i5,a1,i5,2x,a)'
		;
		; are we updating?
		if keyword_set(update) then begin
			print,'Starting with a clean slate...'
			spawn,'rm -rf atlas.log atlas_?.log atlas?'
			print,'Done.'
		endif
		;
		; make a log file
		openw,ol,'atlas.log',/get_lun
		t0 = systime(1)
		printf,ol,'Start: '+systime(0)
		print,'Start: '+systime(0)
		;
		; get maximum image size based on image collection
		imsize = max(mosaic_max_imsize('./','*-int-3.fit*',/verbose))
		;
		; size cannot exceed capacity of images
		size = min([sz[ig]*60.,imsize])	; size in arcseconds
		;
		; test if not resizing
		shfact = 1.
        	if not keyword_set(shrink) then begin
	    		pxsiz = size / psc
	    		if pxsiz gt 6000 then $
		    	    print,'Warning - image will have dimensions: ', $
			    	pxsiz,' by ', pxsiz
		;
		; calculate shrink factor
        	endif else begin
			; automatic shrink calculation
			if shrink le 1. then begin
				shfact = 1.0
				pxsiz = size / psc
				; shrink until under maximum size
				while pxsiz gt 6000 do begin
					shfact = shfact + 1.0
					pxsiz = size / ( psc * shfact )
				endwhile
			; or use keyword shrink factor
			endif else 	shfact = shrink
		endelse
		print,'Shrink factor = ',shfact,form='(a,f7.2)'
		;
		; loop over bands
		for iband=0,nband-1 do begin
			print,'Processing ',band[iband],' data...'
			cmd = 'icore_atlas_wise ' + name[ig] + ' ' + $
				string(ra,format='(f13.8)') + ' ' + $
				string(dec,format='(f13.8)') + ' ' + $
				string(size/3600.,format='(f8.5)') + ' ' + $
				bandi[iband] + ' ' + strn(psc*shfact) + $
				' 2 | tee atlas_' + strn(iband+1) + '.log'
			print,cmd
			spawn,cmd
			print,'Done processing ',name[ig],': ',band[iband], $
				' data.'
		endfor
		tim = systime(1) - t0
		printf,ol,'Done : '+systime(0)
		printf,ol,'Time : ',tim
		free_lun,ol
		print,'Done : '+systime(0)
		print,'Time : ',tim
		spawn,'gzip atlas?/'+names[ig]+'_*.fits &'
		if keyword_set(clean) then begin
			print,'Cleaning up...'
			spawn,'rm atlas?/*-w?-*-3.fit*'
			print,'Done cleaning up.'
		endif
		spawn,'gzip *-3.fits &'
	endif
	cd,cwd
      endif else print,dir+names[ig]+' already processed.'
    endif else print,'No directory for host: ',names[ig]
endfor	; loop over hosts
;
return
end
