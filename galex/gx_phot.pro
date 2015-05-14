pro gx_phot,posf
;+
;	GALEX photometry
;
; INPUTS:
;	posf	- position file name with each line: gal id ra dec
;		  ra and dec in HH MM SS.SS and DD MM SS.SS
;
; PROCEDURE:
;	if a detection, centroids and does aperture photometry
;	if non-detection, just produces upper limit from aperture
;
; HISTORY:
;	jdn - 070827
;-
; get list of images
flist=file_search('*int.fits',count=nf)
if nf le 0 then begin
	print,'No files found: *int.fits'
	return
endif
;
; sort images on julian day
hjd=dblarr(nf)
for i=0,nf-1 do begin
	h=headfits(flist(i))
	dt = sxpar(h,'OBSDATIM')
	hjd(i) = julday(strmid(dt,2,2),strmid(dt,4,2),'20'+strmid(dt,0,2), $
			strmid(dt,7,2),strmid(dt,9,2),strmid(dt,11,2))
endfor
flist=flist(sort(hjd))
print,'Measuring ',nf,' files.'
;
; read positions
npos=0L
ad=[0.d0]
dd=[0.d0]
astr=['']
dstr=['']
id=['']
njd=['']
rec=''
;
openr,il,posf,/get_lun
while not eof(il) do begin
	readf,il,rec
	if strpos(rec,'#') lt 0 then begin
		sta=strsplit(rec,/extract)
		id=[id,sta(1)]
		ad=[ad,ten(sta(2),sta(3),sta(4))*15.d0]
		dd=[dd,ten(sta(5),sta(6),sta(7))]
		astr=[astr,sta(2)+':'+sta(3)+':'+sta(4)]
		dstr=[dstr,sta(5)+':'+sta(6)+':'+sta(7)]
		njd=[njd,sta(11)]
		npos=npos+1L
	endif
endwhile
free_lun,il
;
; trim
id=id[1:*]
ad=ad[1:*]
dd=dd[1:*]
astr=astr[1:*]
dstr=dstr[1:*]
njd=njd[1:*]
;
; write out coo file
openw,cl,'coo.dat',/get_lun
for j=0,npos-1 do printf,cl,astr(j),' ',dstr(j)
free_lun,cl
;
; setups
zps=[0.,20.08,18.82]	; photometric zeropoints for [null,NUV,FUV]
apcord = [0.,2.09,1.65]	; detections aperture corrections for [null,NUV,FUV]
apcorn = [0.,0.59,0.36]	; non-detections aperture corrections for [null,NUV,FUV]
;
; loop over images
for i=0,nf-1 do begin
	im = mrdfits(flist(i),0,h,/silent)
;
; get header info
	nx = sxpar(h,'NAXIS1')
	ny = sxpar(h,'NAXIS2')
	ext = sxpar(h,'EXPTIME')
	band = sxpar(h,'BAND')	; 1 - NUV, 2 - FUV
	im = im * ext		; total counts
	dt = sxpar(h,'OBSDATIM')
	jd = julday(strmid(dt,2,2),strmid(dt,4,2),'20'+strmid(dt,0,2), $
		    strmid(dt,7,2),strmid(dt,9,2),strmid(dt,11,2))
	bkg = sxpar(h,'PMEDBG')
	bke = sxpar(h,'PRMSBG')
	bkmg = -2.5 * alog10(bkg*!pi) + zps(band) - apcord(band)
	bkme = 1.08573632d0 * (bke/bkg)
	print,i+1,flist(i),jd,format='(i3,2x,a-32,2x,f13.4)'
;
; get x,y's
	cmd='sky2xy '+flist(i)+' @coo.dat'
	spawn,cmd,out,err
	nr=n_elements(out)
;
; did we get pos's for everyone?
	if nr eq npos then begin
		x=fltarr(npos) - 9999.
		y=fltarr(npos) - 9999.
		r=fltarr(npos) - 1.
		flag=intarr(npos) - 1
		for j=0,npos-1 do begin
			sta=strsplit(out(j),/extract)
			x(j)=float(sta(4))
			y(j)=float(sta(5))
			r(j)=sqrt( (x(j)-(nx/2.))^2 + (y(j)-(ny/2.))^2 )
		endfor
		rlim=0.605 * 3600. / 1.5	; 0.605 degree radius in pixels
		good = where(r le rlim, ng)
		if ng gt 0 then begin
			flag(good) = 0
		endif else begin
			print,'No good positions'
			print,'Returning...'
			return
		endelse
	endif else begin
		print,'position number mis-match: ',nr,npos
		print,'Returning...'
		return
	endelse
;
; first pass photometry
	phadu=1.
	apr=2.53333	; 3.8" radius aperture
	skyrad=[5.,15.]
	badpix=[-1.d0,1.d9]
	aper,im,x,y,flx,flxerr,sky,skyerr,phadu,apr,skyrad,badpix, $
		readnoise=0.00001,/exact,/silent,/flux
;
; which were detected?
	s2nlim = [0.,3.0,1.5]
	s2n = flx/flxerr
	det = where(s2n gt s2nlim(band) and flag ge 0,ndet)
	if ndet gt 0 then flag(det) = 1
;
; centroid positions
	fwhm = 3.0
	cntrd,im,x,y,xc,yc,fwhm,/silent
	badc = where(xc lt 0. or yc lt 0., nbadc)
	if nbadc gt 0 then begin
		xc(badc) = x(badc)
		yc(badc) = y(badc)
	endif
;
; second pass photometry
	apr=1.
	aper,im,xc,yc,cflx,cflxerr,csky,cskyerr,phadu,apr,skyrad,badpix, $
		readnoise=0.00001,/exact,/silent,/flux
;
; write out results
	for j=0,npos-1 do begin
		ofile=id(j)+'.pht'
		if not file_test(ofile,/write) then begin
			openw,ol,ofile,/get_lun
			printf,ol,'# GX_PHOT run on '+systime(0)
			printf,ol,'# Obj: '+id(j)+'  JD: '+njd(j)
			printf,ol,'# pos file: '+posf
			printf,ol,'#'
			printf,ol,'#     x        y        r       mag      merr     sky     skerr  apr  dt bnd  exptim       jd     date            file'
		endif else openw,ol,ofile,/get_lun,/append
		fmt='(7f9.2,f6.2,2i3,f9.2,f12.3,2x,a,2x,a)'
		if flag(j) ge 1 then begin
			mag = -2.5 * alog10(cflx(j)/ext) + $
				zps(band) - apcord(band)
			merr = 1.08573632d0 * (cflxerr(j)/cflx(j))
			skmg = -2.5 * alog10(csky(j)/ext) + $
				zps(band) + 0.88	; mag / asec^2
			skme = 1.08573632d0 * (cskyerr(j)/csky(j))
			printf,ol,xc(j),yc(j),r(j),mag,merr,skmg,skme,apr, $
				flag(j),band,ext,jd,dt,flist(i),format=fmt
		endif else if flag(j) eq 0 then begin
			mag = -2.5 * alog10(flx(j)/ext) + $
				zps(band) - apcorn(band)
			merr = 1.08573632d0 / s2n(j)
			skmg = -2.5 * alog10(sky(j)/ext) + $
				zps(band) + 0.88	; mag / asec^2
			skme = 1.08573632d0 * (skyerr(j)/sky(j))
			if not finite(mag) then mag = bkmg
			if not finite(merr) then merr = bkme
			printf,ol,x(j),y(j),r(j),mag,merr,skmg,skme,2., $
				flag(j),band,ext,jd,dt,flist(i),format=fmt
		endif else begin
			mag = 99.99
			merr = 9.99
			skmg = 99.99
			skme = 9.99
			printf,ol,x(j),y(j),r(j),mag,merr,skmg,skme,2., $
				flag(j),band,ext,jd,dt,flist(i),format=fmt
		endelse
		free_lun,ol
	endfor
endfor
;
return
end
