pro wise_find_bad,iband,siglim=siglim,sigthresh=sigthresh,skythresh=skythresh, $
	display=display
;+
;	find bad wise images for a given band: 1,2,3, or 4
;-
; check inputs
if iband lt 1 or iband gt 4 then begin
	print,'WISE_FIND_BAD: Error - band number must be 1,2,3, or 4: ',iband
	return
endif
;
; sigma threshhold
if keyword_set(sigthresh) then $
	sigth = sigthresh $
else	sigth = 1.e9
;
; sky threshholdd
if keyword_set(skythresh) then $
	skyth = skythresh $
else	skyth = 1.e9
;
; get file list
flist=file_search('*-w'+strn(iband)+'-int*.fits.gz',count=nf)
if nf le 0 then begin
	print,'WISE_FIND_BAD: Error - no files found'
	return
endif
;
; open log file
lfile='bad_images_band'+strn(iband)+'.log'
openw,ll,lfile,/get_lun,/append
printf,ll,'# WISE_FIND_BAD run on '+systime(0)
printf,ll,'# BAND : '+strn(iband)
printf,ll,'# FILES: '+strn(nf)
if keyword_set(siglim) then $
	printf,ll,'# SIGLM: '+strn(siglim) $
else	printf,ll,'# SIGLM: '+strn(3.0)
if keyword_set(skythresh) then $
	printf,ll,'# SKYTH: '+strn(skythresh)
if keyword_set(sigthresh) then $
	printf,ll,'# SIGTH: '+strn(sigthresh)
printf,ll,'# image                          SKY          SIG'
;
; get sky values
skys=fltarr(nf)
sigs=fltarr(nf)
for i=0,nf-1 do begin
	im=mrdfits(flist[i],0,hdr,/fscale,/silent)
	mmm,im,skymod,sigma
	print,i+1,'/',nf,skymod,sigma,flist[i],format='(i4,a,i4,2x,2f9.3,2x,a)'
	skys[i] = skymod
	sigs[i] = sigma
endfor
;
; check for NaNs or bad sigmas
bad = where(finite(skys) ne 1 or finite(sigs) ne 1 or $
	sigs lt 0. or sigs gt sigth or skys gt skyth, nbad)
if nbad gt 0 then begin
	print,'Files with NaNs, sig lt 0, sig gt sigth, sky gt skyth:'
	print,'bad image                        SKY          SIG'
	for i=0,nbad-1 do begin
		;
		; make sure we have a place to put the baddies
		if file_test('bad',/directory) ne 1 then $
			file_mkdir,'bad',/noexpand_path
		printf,ll,flist[bad[i]],skys[bad[i]],sigs[bad[i]]
		print,flist[bad[i]],skys[bad[i]],sigs[bad[i]]
		badf = repstr(flist[bad[i]],'int','*')
		spawn,'mv '+badf+' bad/'
	endfor
endif
;
; now get rid of baddies
good = where(finite(skys) eq 1 and finite(sigs) eq 1 and $
	sigs gt 0. and sigs le sigth and skys le skyth, ngood)
if ngood le 0 then begin
	print,'WISE_FIND_BAD: Error - all files bad'
	free_lun,ll
	return
endif
skys=skys[good]
sigs=sigs[good]
flist=flist[good]
;
; get stats
ims,skys,skymn,skysg,wgtsky,siglim=siglim
ims,sigs,sigmn,sigsg,wgtsig,siglim=siglim
print,'Skys; Mean, Sigma: ',skymn,skysg
print,'Sigs; Mean, Sigma: ',sigmn,sigsg
bad = where(wgtsky le 0 or wgtsig le 0, nbad)
if nbad gt 0 then begin
	print,'Files with outlier skys/sigs'
	print,'bad image                        SKY          SIG'
	for i=0,nbad-1 do begin
		;
		; make sure we have a place to put the baddies
		if file_test('bad',/directory) ne 1 then $
			file_mkdir,'bad',/noexpand_path
		printf,ll,flist[bad[i]],skys[bad[i]],sigs[bad[i]]
		print,flist[bad[i]],skys[bad[i]],sigs[bad[i]]
		badf = repstr(flist[bad[i]],'int','*')
		spawn,'mv '+badf+' bad/'
	endfor
endif
free_lun,ll
;
if keyword_set(display) then begin
	;
	; plot histograms
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	!p.multi=[0,2,1]
	plothist,skys,bin=50,xtitle='Sky Level',ytitle='N'
	plothist,sigs,xtitle='Sky Sigma',ytitle='N'
	!p.multi=0
endif
;
end
