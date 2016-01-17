pro wise_driz_proc,lfile,update=update,clean=clean,nobp16=nobp16, $
	shrink=shrink, limit=limit
;+
; wise_driz_proc - process WISE images with drizzling using ICORE
;
; lfile - galaxy list with one entry per galaxy with the following columns:
;	id
;	ra,dec	- decimal degrees
;	majdiam,mindiam	- arcminutes
;	pa	- position angle in degrees
;	type	- galaxy Hubble type (string)
;
; KEYWORDS:
;	UPDATE	- set to update previous run, otherwise will not re-process
;	CLEAN	- set to remove intermediate files (recommended)
;	NPBP16	- set to prevent FITS scaling into 16 bit integers
;	SHRINK	- set to limit image size by adjusting pixel scale
;	LIMIT   - set to number limit per band of images retrieved (def: 200)
;
; Version Info: $Id: wise_driz_proc.pro,v 1.10 2015/03/01 02:20:20 neill Exp $
;-
; read input
readcol,lfile,id,sra,sdec,majdiam,mindiam,pa,format='a,d,d,f,f,f'
nobj = n_elements(sra)
name = strtrim(id,2)
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
nband = n_elements(band)
;
; nframes
if keyword_set(limit) then begin
	if limit gt 1 then begin
		lstr = strtrim(strn(limit),2)
		nframes = lstr+','+lstr+','+lstr+','+lstr
	endif else $
		nframes = '200,200,200,200'
endif else	nframes = 'all,all,all,all'
;
; pixel size
psc = 1.0
;
; directories
root = !GLGA_WISE_DATA + 'data/sort/'
;
; loop over objects
for ig=0L,nobj-1L do begin
    ; check for existing directory
    if file_test(root+name[ig]) ne 0 then begin
	cmd = 'mv '+root+name[ig]+' '+root+name[ig]+'_old'
	spawn,cmd
    endif
    ; get the frames
    cmd = 'getframesAllSky -ra ' + $
	strtrim(string(sra[ig],form='(f13.6)'),2) + ' -dec ' + $
	strtrim(string(sdec[ig],form='(f13.6)'),2) + ' -sx ' + $
	strtrim(string(r[ig],form='(f9.3)'),2) + ' -sy ' + $
	strtrim(string(r[ig],form='(f9.3)'),2) + ' -outd ' + root+name[ig] + $
	' -bands 1,2,3,4 -num '+nframes+' -moo 25 -ant 2000 -log ' + $
	name[ig] + '_fetch.log'
    spawn,cmd
    ;
    ; current coordinates
    ra=sra[ig]
    dec=sdec[ig]
    ;
    ; current directory
    dir=root+name[ig]+'/'
    ;
    ; does directory exist?
    if file_test(dir,/directory) then begin
      ;
      ; Do only if not processed already or requesting update
      if not file_test(dir+'driz1/'+name[ig]+'_w1.fit*') or $
	 keyword_set(update) then begin
	;
	; go to directory
	cd,dir,curr=cwd
	;
	; are there any files here?
	if file_test('*-int-1b.fit*') then begin
		;
		; get to work and print our status
		print,ig+1,'/',nobj,name[ig],form='(i5,a1,i5,2x,a)'
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
			cmd = 'icore_driz_wise ' + name[ig] + ' ' + $
				string(ra,format='(f13.8)') + ' ' + $
				string(dec,format='(f13.8)') + ' ' + $
				string(size/3600.,format='(f8.5)') + ' ' + $
				strn(iband+1) + ' ' + strn(psc*shfact) + $
				' 2 | tee icore_' + strn(iband+1) + '.log'
			print,cmd
			spawn,cmd
			print,'Done processing ',name[ig],': ',band[iband], $
				' data.'
		endfor	; loop over bands
		if not keyword_set(nobp16) then $
			wise_driz_bp16,name[ig] $
		else	spawn,'gzip driz?/'+name[ig]+'_*.fits'
		if keyword_set(clean) then begin
			print,'Cleaning up...'
			spawn,'rm driz?/*.fits'
			print,'Done cleaning up.'
		endif
		spawn,'gzip *-1b.fits &'
	endif
	cd,cwd
      endif else print,dir+name[ig]+' already processed.'
    endif else print,'No directory for host: ',name[ig]
endfor	; loop over hosts
;
return
end
