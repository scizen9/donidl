pro wise_extract,band,ofil,ra,dec,ps=ps,size=size, $
	nolimit=nolimit,verbose=verbose
;+
; extract from wise atlas images (*-3.fits*)
;
; band - w1,w2,w3,w4
; ofil - output file name for intensity image
; ra,dec - j2000 coord (decimal degrees)
; 
; size - diameter in arcminutes 
; verbose - set to get more output
;
; not yet used:
; ps = arcsecs per pixel (default is native scale)
; nolimit
;
;-
; 
; check inputs
flist_int=file_search('*-'+band+'-int-3.fit*',count=nint)
if nint le 0 then begin
	print,'WISE_EXTRACT: Error - no files found'
	return
endif
;
; verbose?
verb = keyword_set(verbose)
if verb then print,'Processing ',nint,' ',band,' images'
;
; get image size
inthdr=headfits(flist_int[0])
getrot,inthdr,rot,cdelt
as_pix = abs(cdelt[0])*3600.
npix = ceil(size/as_pix)
;
; make ref header
refhdr = inthdr
sxaddpar,refhdr,'NAXIS1',npix
sxaddpar,refhdr,'NAXIS2',npix
sxaddpar,refhdr,'CRPIX1',npix/2.
sxaddpar,refhdr,'CRPIX2',npix/2.
sxaddpar,refhdr,'CRVAL1',ra
sxaddpar,refhdr,'CRVAL2',dec
;
; construct output images
into = fltarr(npix,npix)
covo = fltarr(npix,npix)
unco = fltarr(npix,npix)
;
; loop over input images
nadd = 0
for i=0,nint-1 do begin
	;
	; read in images
	fint= flist_int[i]
	fcov= repstr(fint,'-int-','-cov-')
	func= repstr(fint,'-int-','-unc-')
	int = mrdfits(fint,0,ihdr,/fscale,/silent)
	cov = mrdfits(fcov,0,chdr,/fscale,/silent)
	unc = mrdfits(func,0,uhdr,/fscale,/silent)
	;
	; align images
	hastrom_err,int,ihdr,refhdr,missing=0,interp=0,errmsg=erri
	hastrom_err,cov,chdr,refhdr,missing=0,interp=0,errmsg=errc
	hastrom_err,unc,uhdr,refhdr,missing=0,interp=0,errmsg=erru
	;
	; check errors
	if strlen(erri) gt 0 or strlen(errc) gt 0 or strlen(erru) gt 0 then $
		badim = (1 eq 1) $
	else	badim = (1 eq 0)
	;
	; only add places where needed
	good = where(int gt 0 and into le 0, ngood)
	if ngood le 0 then badim = (1 eq 1)
	;
	; skip if image is bad
	if not badim then begin
		;
		; check overlap for background offset
		over = where(int gt 0 and into gt 0, nover)
		if nover gt 0 then $
			off = mode(int[over]) - mode(into[over]) $
		else	off = 0.
		;
		; add them in
		into[good] = int[good] - off[0]
		covo[good] = cov[good]
		unco[good] = unc[good]
		;
		; set up output headers
		if i eq 0 then begin
			inthdr = ihdr
			sxaddpar,inthdr,'NAXIS1',npix
			sxaddpar,inthdr,'NAXIS2',npix
			sxaddpar,inthdr,'CRPIX1',npix/2.
			sxaddpar,inthdr,'CRPIX2',npix/2.
			sxaddpar,inthdr,'CRVAL1',ra
			sxaddpar,inthdr,'CRVAL2',dec
			covhdr = chdr
			sxaddpar,covhdr,'NAXIS1',npix
			sxaddpar,covhdr,'NAXIS2',npix
			sxaddpar,covhdr,'CRPIX1',npix/2.
			sxaddpar,covhdr,'CRPIX2',npix/2.
			sxaddpar,covhdr,'CRVAL1',ra
			sxaddpar,covhdr,'CRVAL2',dec
			unchdr = uhdr
			sxaddpar,unchdr,'NAXIS1',npix
			sxaddpar,unchdr,'NAXIS2',npix
			sxaddpar,unchdr,'CRPIX1',npix/2.
			sxaddpar,unchdr,'CRPIX2',npix/2.
			sxaddpar,unchdr,'CRVAL1',ra
			sxaddpar,unchdr,'CRVAL2',dec
		endif
		;
		; increment count
		nadd = nadd + 1
		;
		; list input files
		sxaddpar,inthdr,'ATFIL'+string(nadd,format='(i2.2)'),fint
		sxaddpar,covhdr,'ATFIL'+string(nadd,format='(i2.2)'),fcov
		sxaddpar,unchdr,'ATFIL'+string(nadd,format='(i2.2)'),func
		if verb then print,i+1,'/',nint,' file: ',fint, $
			form='(i3,a,i3,2x,a,a)'
	endif else begin ; badim
		if verb then print,i+1,'/',nint,'  bad: ',fint, $
			form='(i3,a,i3,2x,a,a)'
		openw,ol,'bad.list',/get_lun,/append
		printf,ol,fint
		free_lun,ol
	endelse
endfor
;
; did we get any images?
if nadd gt 0 then begin
	;
	; write intensity image
	mwrfits,into,ofil,inthdr,/iscale,/create
	if verb then print,'Wrote int file: ',ofil
	;
	; write uncertainty image
	ofunc = repstr(ofil,'.fits','_unc.fits')
	mwrfits,unco,ofunc,unchdr,/iscale,/create
	if verb then print,'Wrote unc file: ',ofunc
	;
	; write coverage image
	ofcov = repstr(ofil,'.fits','_cov.fits')
	mwrfits,covo,ofcov,covhdr,/iscale,/create
	if verb then print,'Wrote cov file: ',ofcov
endif else print,'No good ims'
;
return
end
