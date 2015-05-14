pro gx_profile,gal,rn,mn,mne,rf,mf,mfe,rfmn,mfmn,mfmne,q,pa,ra,dec,silent=silent
;
; get galex NGA profile
;
; check inputs
if n_params(0) lt 1 then begin
	print,'GX_PROFILE - Usage: gx_profile,galaxy,rNUV,magNUV,merrNUV,rFUV,magFUV,merrFUV,rFUV-NUV,magFUV-NUV,merrFUV-NUV,axalrat,posang,ra_deg,dec_deg'
	return
endif
;
; setups
q=-1.
pa=-999.
ra=-9.
dec=-99.
rn=[-1.]
mn=[-1.]
mne=[-1.]
rf=[-1.]
mf=[-1.]
mfe=[-1.]
rfmn=[-1.]
mfmn=[-1.]
mfmne=[-1.]
;
; get list of data files
flist=file_search(!NGA_DATA+gal+'-prof_*.dat',count=nf)
if nf le 0 then begin
	if not keyword_set(silent) then $
		print,'No files found for ',gal
	return
endif
;
; Do we have NUV?
n=where(strpos(flist,'NUV') ge 0, nn)
if nn eq 1 then begin
	readcol,flist(n(0)),a,d,q,pa,rn,mn,mne,format='d,d,f,f,f,f,f',/silent
	q = (1.-q(0))
	pa = pa(0)
	ra=a(0)
	dec=d(0)
	s=sort(rn)
	rn=rn(s)
	mn=mn(s)
	mne=mne(s)
endif else begin
	if not keyword_set(silent) then $
		print,'No NUV profile for ',gal
	rn=[-1.]
	mn=[-1.]
	mne=[-1.]
endelse
;
; Do we have FUV?
n=where(strpos(flist,'FUV') ge 0, nn)
if nn eq 1 then begin
	readcol,flist(n(0)),a,d,q,pa,rf,mf,mfe,format='d,d,f,f,f,f,f',/silent
	q = (1.-q(0))
	pa = pa(0)
	ra=a(0)
	dec=d(0)
	s=sort(rf)
	rf=rf(s)
	mf=mf(s)
	mfe=mfe(s)
endif else begin
	if not keyword_set(silent) then $
		print,'No FUV profile for ',gal
	rf=[-1.]
	mf=[-1.]
	mfe=[-1.]
endelse
;
; Do we have FUV-NUV?
n=where(strpos(flist,'F-N') ge 0, nn)
if nn eq 1 then begin
	readcol,flist(n(0)),a,d,q,pa,rfmn,mfmn,mfmne,format='d,d,f,f,f,f,f',/silent
	q = (1.-q(0))
	pa = pa(0)
	ra=a(0)
	dec=d(0)
	s=sort(rfmn)
	rfmn=rfmn(s)
	mfmn=mfmn(s)
	mfmne=mfmne(s)
endif else begin
	if not keyword_set(silent) then $
		print,'No FUV-NUV profile for ',gal
	rfmn=[-1.]
	mfmn=[-1.]
	mfmne=[-1.]
endelse
;
return
end
