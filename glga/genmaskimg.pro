pro genmaskimg,id,ra,starlist=starlist,verbose=verbose, $
	galex=galex,sdss=sdss,twomass=twomass,irac=irac,wise=wise
;+
;	genmaskimg - generate a mask image for a set of galaxies
;
; INPUTS:
;	id	- galaxy id
;	ra	- ra in decimal degrees of galaxy
;
; KEYWORDS:
;	starlist - copy the pointsource list as well
;	verbose  - print progress
;
; NOTE:
;	writes files <id>-mask.fits in local directory
;
; HISTORY:
;	04-aug-2011, jdn - kludged from a routine by Mark Seibert
;-
bdir='galex/'		; band-specific directory
fpx=''			; file postfix
itail='_NUV.fits.gz'	; image file tail
if keyword_set(sdss) then begin
	bdir='sdss/'
	fpx='sdss_'
	itail='_r.fits.gz'
endif
if keyword_set(galex) then begin
	bdir='galex/'
	fpx='galex_'
	itail='_NUV.fits.gz'
endif
if keyword_set(twomass) then begin
	bdir='2mass/'
	fpx='2mass_'
	itail='_k.fits.gz'
endif
if keyword_set(wise) then begin
	bdir='wise/'
	fpx='wise_'
	itail='_w1.fits.gz'
endif
if keyword_set(irac) then begin
	bdir='irac/'
	fpx='irac_'
	itail='_4p5um.fits.gz'
endif

gdir = !GLGA_ROOT + 'data/'
ng = n_elements(id)

for i = 0L, ng-1 do begin
	gal = strtrim(id[i],2)
	ddir = gdir + glga_degdir(ra[i]) + '/'
	mfil = ddir + 'aux/'+gal+'_'+fpx+'mask.fits.gz'
	sfil = ddir + 'aux/'+gal+'_'+fpx+'pointsrc.dat'
	nfil = ddir + bdir+'fits/'+gal+itail
	ofil = gal + '_'+fpx+'mask.fits'
	olab = ofil
	if file_test(mfil) then begin
		imsk=mrdfits(mfil,0,mhdr)
		idx = where(imsk eq 1, nidx)
		n=mrdfits(nfil,0,hdr,/fscale)
		sz = size(n,/dim)
		mask = intarr(sz) + 1
		;mask=n*0+1
		if nidx gt 0 then $
			mask[idx]=0
		mwrfits,mask,ofil,hdr,/create
		if keyword_set(starlist) then begin
			spawn,'cp '+sfil+' .'
			olab = olab + '  ' + sfil
		endif
	endif else begin
		if keyword_set(verbose) then $
			print,i+1,' / ',ng,'  no mask found for: ',gal
		olab = ''
	endelse
	if keyword_set(verbose) and strlen(olab) gt 0 then $
		print,i+1,' / ',ng,'  wrote: ',olab
endfor

end
