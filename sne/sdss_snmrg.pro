pro sdss_snmrg,silent=silent
;
; Merge photometry and analysis on 2mass SDSS sub images.
;
sbrto = 3.4787	; converts res ap to surf brightness (mags per square arcsec)
;
; open merge file
mfile=!SNE_DATA+'sdss_snmrg.log'
filestamp,mfile
openw,ol,mfile,/get_lun
printf,ol,'# SDSS_SNMRG run on '+systime(0)
printf,ol,'# SN          mu   muerr   mu.5  muerr    mu1  muerr    mu2  muerr   sbu   ufrac  udelp udelpe    mg   mgerr   mg.5  mgerr    mg1  mgerr    mg2  mgerr   sbg   gfrac  gdelp gdelpe    mr   mrerr   mr.5  mrerr    mr1  mrerr    mr2  mrerr   sbr   rfrac  rdelp rdelpe   mi   mierr   mi.5  mierr    mi1  mierr    mi2  mierr   sbi   ifrac  idelp idelpe    mz   mzerr   mz.5  mzerr    mz1  mzerr    mz2  mzerr   sbz   zfrac  zdelp zdelpe'
;
; read log file
maxrec=100000L
ldat=strarr(maxrec)
nrec = 0L
rec=''
lfile=!SNE_DATA+'sdss_snan.log'
filestamp,lfile
openr,il,lfile,/get_lun
while not eof(il) do begin
	readf,il,rec
	if strmid(rec,0,1) ne '#' then begin
		ldat(nrec) = rec
		nrec = nrec + 1L
	endif
endwhile
free_lun,il
ldat = ldat(0:(nrec-1))
pdat = fltarr(nrec,60)
ids  = strarr(nrec)
;
; loop over records converting data
for i=0,nrec-1 do begin
	sta=strsplit(ldat(i),/extract)
	ids(i) = sta(0)
	for j=1,60 do pdat(i,j-1) = float(sta(j))
endfor	; loop over records
;
; get unique SN ids
ids = ids(sort(ids))
ids = ids(uniq(ids))
nids = n_elements(ids)
;
; loop over SN ids
for i=0,nids-1 do begin
	avs = fltarr(60) - 9.00		; to store averages
;
; find the records for this SN
	s=where(strpos(ldat,ids(i)) ge 0, ns)
;
; a single entry
	if ns eq 1 then begin
		for j=0,59 do avs(j) = pdat(s,j)
;
; a multiple entry
	endif else if ns gt 1 then begin
;
; loop over SDSS filters
	    for l=0,4 do begin
;
; loop over aperture mags and errors (by two)
		for j=l*12,(l*12+7),2 do begin
			mags = pdat(s,j)
			merr = pdat(s,j+1)
;
; how many good ones do we have?
			good = where(mags gt 0., ngood)
;
; just one
			if ngood eq 1 then begin
				avs(j) = mags(good)
				avs(j+1) = merr(good)
;
; multiple ones
			endif else if ngood gt 1 then begin
				flxs = 10.0^(-0.4*(mags(good)-25.0))
				flxe = (flxs * merr(good))/1.0857362
				avs(j)   = -2.5 * alog10(wmean(flxs,flxe)) + 25.
				avs(j+1) = 1.0857362 * $
					(wstdev(flxs,flxe)/wmean(flxs,flxe))
;
; none
			endif else begin
				avs(j) = -99.0
				avs(j+1) = -9.0
			endelse
		endfor
;
; add surface brightness
		if avs(l*12) gt 0. then $
			avs(l*12+8) = avs(l*12) + sbrto $
		else	avs(l*12+8) = -99.00
;
; done with apertures, now profile data
;
; fractional light
		frac = pdat(s,l*12+9)
		good=where(frac gt 0., ngood)
		if ngood eq 1 then $
			avs(l*12+9) = frac(good) $
		else if ngood gt 1 then $
			avs(l*12+9) = avg(frac(good)) $
		else	avs(l*12+9)= -9.0
;
; delta from profile mag
		delp = pdat(s,l*12+10)
		delpe= pdat(s,l*12+11)
		good=where(delp gt 0., ngood)
		if ngood eq 1 then begin
			avs(l*12+10) = delp(good)
			avs(l*12+11)= delpe(good)
		endif else if ngood gt 1 then begin
			avs(l*12+10) = wmean(delp(good),delpe(good))
			avs(l*12+11)= wstdev(delp(good),delpe(good))
		endif else begin
			avs(l*12+10) = -99.0
			avs(l*12+11)= -9.0
		endelse
	    endfor	; loop over SDSS filters
	endif	; ns gt 1
;
; print out averages
	printf,ol,ids(i),avs, $
		format='(a-10,5(9f7.2,f7.3,2f7.2))'
endfor	; loop over SNe
;
; close file
free_lun,ol
;
return
end
