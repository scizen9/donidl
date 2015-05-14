pro wise_atlas_driver,lfile,pxscl=pxscl,nolimit=nolimit, $
	update=update, shrink=shrink
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
;
; 3 arcmin, is minimum size
sz=(majdiam*4.0)>3.
;
; list of spectral bands
band=['w1','w2','w3','w4']
nband = n_elements(band)
;
; pixel scale
if keyword_set(pxscl) then $
	psc = pxscl $
else	psc = 1.375
print,'Using pixel scale ("/px) of ',psc
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
      if not file_test(dir+'atlas/'+names[ig]+'_w1.fit*') or $
	 keyword_set(update) then begin
	;
	; go to directory
	cd,dir,curr=cwd
	;
	; are there any files here?
	if file_test('*-int-3.fit*') then begin
		;
		; get to work and print our status
		print,ig+1,'/',ng,names[ig],form='(i5,a1,i5,2x,a)'
		;
		; are we updating?
		if keyword_set(update) then begin
			print,'Starting with a clean slate...'
			spawn,'rm -rf atlas*'
			spawn,'mkdir atlas'
			print,'Done.'
		endif else begin
			if file_test('atlas',/directory) then begin
				print,'Dir atlas already exists.'
			endif else begin
				print,'Making atlas dir.'
				spawn,'mkdir atlas'
			endelse
		endelse
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
        	if not keyword_set(shrink) then begin
	    		pxsiz = size / psc
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
				pxsiz = size / psc
				; shrink until under maximum size
				while pxsiz gt 6000 do begin
					shfact = shfact + 1.0
					pxsiz = size / ( psc * shfact )
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
			wise_extract,band[iband], $
				names[ig]+'_'+band[iband]+'.fits',$
                         	ra,dec,ps=psc,size=size, $
				nolimit=nolimit,/verbose
			;
			; rebin if needed
			if dorebin then begin
				; image file
				imfl = names[ig]+'_'+band[iband]+'.fits'
				im = mrdfits(imfl,0,hdr,/fscale,/silent)
				nx = fix(sxpar(hdr,'NAXIS1')/shfact)
				ny = fix(sxpar(hdr,'NAXIS2')/shfact)
				print,'Rebinning to ',nx,ny,' : ',imfl
				hrebin,im,hdr,out=[nx,ny]
				sxaddpar,hdr,'SHFACT',shfact,'Shrink Factor'
				; keep photometry correct
				mwrfits,im*shfact^2,imfl,hdr,/iscale,/create
				; uncertainty file
				imfl = names[ig]+'_'+band[iband]+'_unc.fits'
				im = mrdfits(imfl,0,hdr,/fscale,/silent)
				hrebin,im,hdr,out=[nx,ny]
				sxaddpar,hdr,'SHFACT',shfact,'Shrink Factor'
				mwrfits,im,imfl,hdr,/iscale,/create
				; coverage file
				imfl = names[ig]+'_'+band[iband]+'_cov.fits'
				im = mrdfits(imfl,0,hdr,/fscale,/silent)
				hrebin,im,hdr,out=[nx,ny]
				sxaddpar,hdr,'SHFACT',shfact,'Shrink Factor'
				mwrfits,im,imfl,hdr,/iscale,/create
			endif else print,'No rebinning required...'
			print,'Done processing ',names[ig],': ',band[iband], $
				' data.'
		endfor
		tim = systime(1) - t0
		printf,ol,'Done : '+systime(0)
		printf,ol,'Time : ',tim
		free_lun,ol
		print,'Done : '+systime(0)
		print,'Time : ',tim
		spawn,'mv '+names[ig]+'_*.fits atlas'
		spawn,'gzip atlas/'+names[ig]+'_*.fits &'
		spawn,'gzip *-3.fits &'
	endif
	cd,cwd
      endif else print,dir+names[ig]+' already processed.'
    endif else print,'No directory for host: ',names[ig]
endfor	; loop over hosts
;
return
end
