pro sdss_mega_driver,lfile,clean=clean,pxscl=pxscl,nolimit=nolimit, $
	noskytweak=noskytweak, update=update, shrink=shrink, $
	fast=fast,medium=medium,slow=slow
;+
; sdss_mega_driver - create coadded sdss images
; lfile - list of hosts with basic info: 
;	one entry per line, ra, dec, pa in degress, diams in arcmin:
;	name ra dec majdiam, mindiam, pa
; clean - set to clean temporary files
; pxscl - pixel scale in arcsec per pix (defaults to 0.5)
; nolimit - set to remove limit on number of images to combine (30)
; noskytweak - set to disable sky matching
; update - set to re-do existing mosaics
; shrink - set to shrink factor or 1.0 for automatic shrink
; fast - use only sky from MMM for initial image differences
; medium - start with MMM sky, but reproject and calc overlap for baddies
; slow - reproject and calc overlap for all images
;-
; get list
readcol,lfile,names,sra,sdec,majdiam,mindiam,pa, $
	format='a,d,d,f,f,f'
;
; 3 arcmin, is minimum size
msize=(majdiam*4.0)>3.
;
; list of spectral bands
band=['u','g','r','i','z']
nband = n_elements(band)
;
; pixel scale
if keyword_set(pxscl) then $
	psc = pxscl $
else	psc = 0.5
print,'Using pixel scale ("/px) of ',psc
;
; skytweak?
if keyword_set(noskytweak) then $
	skytweak = 0 $
else	skytweak = 1
;
; directories
root = !GLGA_SDSS_DATA + 'data/sort/'
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
      if not file_test(dir+names[ig]+'_g.fit*') or $
	 keyword_set(update) then begin
	;
	; go to directory
	cd,dir,curr=cwd
	;
	; are there any files here?
	if file_test('fpC*fit*') then begin
		;
		; get to work and print our status
		print,ig+1,'/',ng,names[ig],form='(i5,a1,i5,2x,a)'
		;
		; are we updating?
		if keyword_set(update) then begin
			print,'Starting with a clean slate...'
			spawn,'rm -f '+names[ig]+'_*fit* *mimap* *.swarp *.tweaks'
			print,'Done.'
		endif
		;
		; get maximum image size based on image collection
		imsize = max(mosaic_max_imsize('./','fpC*fit*',/verbose))
		;
		; size cannot exceed capacity of images
		asize = min([msize[ig]*60.,imsize])	; size in arcseconds
		;
		; test if not resizing
        	if not keyword_set(shrink) then begin
	    		pxsiz = asize / psc
	    		if pxsiz gt 6000 then $
		    	    print,'Warning - image will have dimensions: ', $
			    	pxsiz,' by ', pxsiz
			dorebin = (1 eq 0)
		;
		; calculate shrink factor
        	endif else begin
			; automatic shrink calculation
			if shrink le 1. then begin
				shfact = 1.0
				pxsiz = asize / psc
				; shrink until under maximum size
				while pxsiz gt 6000 do begin
					shfact = shfact + 1.0
					pxsiz = asize / ( psc * shfact )
				endwhile
				if shfact le 1.0 then $
					dorebin = (1 eq 0) $
				else	dorebin = (1 eq 1)
			; or use keyword shrink factor
			endif else begin
				shfact = shrink
				dorebin = (1 eq 1)
			endelse
		endelse
		if dorebin then print,'Shrink factor = ',shfact,form='(a,f7.2)'
		;
		; loop over bands
		for iband=0,nband-1 do begin
			print,'Processing ',band[iband],' data...'
			mimap_sdss_combine,'',band[iband], $
				names[ig]+'_'+band[iband]+'.fits',$
                         	ra,dec,ps=[psc,psc],size=[asize,asize], $
				skytweak=skytweak,nolimit=nolimit, $
				fast=fast,medium=medium,slow=slow
			;
			; rebin if needed
			if dorebin then begin
				; image file
				imfl = names[ig]+'_'+band[iband]+'.fits'
				im = mrdfits(imfl,0,hdr)
				nx = fix(sxpar(hdr,'NAXIS1')/shfact)
				ny = fix(sxpar(hdr,'NAXIS2')/shfact)
				print,'Rebinning to ',nx,ny,' : ',imfl
				hrebin,im,hdr,out=[nx,ny]
				sxaddpar,hdr,'SHFACT',shfact,'Shrink Factor'
				; keep photometry correct
				mwrfits,im*shfact^2,imfl,hdr,/create
				; inverse variance file
				imfl = names[ig]+'_'+band[iband]+'_ivar.fits'
				im = mrdfits(imfl,0,hdr,/silent)
				hrebin,im,hdr,out=[nx,ny]
				mwrfits,im,imfl,hdr,/create
			endif else print,'No rebinning required...'
			print,'Done processing ',names[ig],': ',band[iband], $
				' data.'
		endfor
		if keyword_set(clean) then begin
			print,'Cleaning up...'
			spawn,'rm mimap_*fit*'
			print,'Done cleaning up.'
		endif
		spawn,'gzip '+names[ig]+'_*.fits &'
	endif
	cd,cwd
      endif else print,dir+names[ig]+' already processed.'
    endif else print,'No directory for host: ',names[ig]
endfor	; loop over hosts
;
return
end
