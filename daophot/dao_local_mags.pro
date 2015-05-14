pro dao_local_mags,ids,x,y,mags,merrs,chis,shrps,alflist,filts,root,offsets

nfilt = n_elements(filts)
nstars = n_elements(ids)

; loop over filters

for flt = 0, nfilt-1 do begin

    pos = strpos(alflist, filts(flt))	; where are this filter's data
    grab = where(pos eq 0, ngrab)

    if ngrab gt 0 then begin		; get the observations

; force magnitudes to reference frame

	flist = alflist(grab)
	offs  = offsets(grab)

	std = where(offs lt 99., nstd)	; is there a standard set?
	if nstd gt 0 then begin
	     tmp = flist(std(0))	; if yes, grab the filename
	     off = offs(std(0))		;         grab the offset
	     mframe = std(0)		;         master frame index

	endif else begin
		tmp = flist(0)		; if not, just use the first one
		off = 0.0		;         set offset to zero
		mframe = 0		;         master frame index
	endelse

	pref = gettok(tmp,'.')

	get_dao_refs, pref, rids	; find the ref stars ids

	nrids= n_elements(rids)		; how many ref ids?
	rindex= lonarr(nrids)		; where are they located?
	uids = lonarr(nrids)		; which will actually be used?

	p = 0

	for i=0,nrids-1 do begin	; loop over ref ids
	    t=where(ids eq rids(i), count)	; find them
	    if count eq 1 then begin
		uids(p) = rids(i)	; add to list of ref ids used
		rindex(p) = t(0)	; store index into mags, etc
		p = p + 1
	    endif
	endfor
	uids  = uids(0:p-1)
	rindex = rindex(0:p-1)
	print,'Found ref ids: ',p

	refmags = reform(mags(grab(mframe), rindex)) + off

	openw,flun,root+filts(flt)+'.loc',/get_lun
	printf,flun,'DAO_LOCAL_MAGS: '+systime(0)
	printf,flun,'phot ref file: ',pref
	printf,flun,'phot ref off : ',off
	printf,flun,'reference stars:'
	printf,flun,'       #          ID       INDEX'
	for i=0,p-1 do printf,flun,i+1, uids(i), rindex(i)
	printf,flun,' '
	printf,flun,'epochs:'
	printf,flun,'FILE             DELMAG    SIGMA   NSTARS     NREJ'

	fmags = mags(grab,*)
	fmerrs= merrs(grab,*) > 0.001

	for i=0,ngrab-1 do begin

	    tmags = reform(mags(grab(i),*))
	    tmags = tmags(rindex)

	    rgood = where(tmags lt 99. and refmags lt 99., nrgood)
	    if nrgood gt 5 then begin

	    	dmags = tmags(rgood) - refmags(rgood)
	    	ims, dmags, mean, sigma, wgt
		if i eq mframe then $
			nrej = 0 $
		else	nrej = n_elements(dmags) - total(wgt)
		
		good = where(fmags(i,*) lt 99., ngood)
		if ngood gt 0 then $
	    	    fmags(i,good) = fmags(i,good) - mean

	    endif else begin

	    	printf,flun,'Not enough good points: ',nrgood
	    	print,'Not enough good points: ',nrgood
		fmags(i,*) =  mags(grab(i),*)
		fmerrs(i,*)= merrs(grab(i),*)
		mean = 0.
		sigma = 0.

	    endelse

	    printf,flun,alflist(grab(i)),mean,sigma,nrgood,nrej, $
	    	form='(a,f9.3,f9.3,2i9)'
	    print,alflist(grab(i)),mean,sigma,nrgood,nrej, $
	    	form='(a,f9.3,f9.3,2i9)'
	endfor

	printf,flun,' '
	free_lun,flun

; calculate statistics for each star
; lifted nearly wholesale from Peter Stetson's algorithms in daomaster.f

	gchis	= chis(grab,*)
	gshrps	= shrps(grab,*)

	nframes	= intarr(nstars)
	chi	= fltarr(nstars)
	sharp	= fltarr(nstars)
	var	= fltarr(nstars)
	blunder	= fltarr(nstars)

	mm	= reform(fmags(mframe,*))	; master magnitudes
	ss	= reform(fmerrs(mframe,*)); master errors

	for i=0L,nstars-1L do begin

	    iobs	= where(fmags(*,i) lt 99., nobs)
	    nframes(i)	= nobs

	    if nobs gt 0 then begin

		if nobs ge 2 then begin

	    	    m	= reform(fmags(*,i))
	    	    s	= reform(fmerrs(*,i))

	            mm(i)	= exp(0.921034*(25. - fmags(mframe,i)))

		    sumchi 	= 0.
		    sumshp	= 0.
		    sumwt	= 0.
		    sumerr	= 0.

		    for j = 0,nobs-1 do begin

		    	jj	= iobs(j)
			m(jj)	= exp(0.921034 * (25. - m(jj)))
			s(jj)	= s(jj) * (0.921034 * m(jj))^2
			wt	= 1. / s(jj)
			sumerr	= sumerr + wt^2
			sumwt	= sumwt + wt
			sumchi	= sumchi + gchis(jj,i) * wt
			sumshp	= sumshp + gshrps(jj,i) * wt

		    endfor

		    sumerr = sqrt( 1. / sumerr )
		    sumchi = sumchi / sumwt
		    sumshp = sumshp / sumwt

		    repeat begin
		    
			sum = 0.
			sumwt = 0.

			for j = 0, nobs-1 do begin

		    	    jj		= iobs(j)
			    b		= m(jj) - mm(i)
			    wt		= (1./s(jj)) * (4. / (4.+(b^2 / s(jj))))
			    sum 	= sum + b * wt
			    sumwt	= sumwt + wt

			endfor

			b = sum / sumwt
			mm(i) = mm(i) + b
		    	
		    endrep until abs(b) lt 0.00005*mm(i)

		    a = 0.
		    rr = 0.
		    sum = 0.
		    sumwt = 0.

		    for j = 0, nobs-1 do begin

		    	jj	= iobs(j)
			b	= m(jj) - mm(i)
			wt	= 1. / s(jj)
			a	= a + wt * abs(b)

			if b gt 0. then rr = rr + 1.

			sum	= sum + wt * (abs(b) / sqrt(s(jj)))
			sumwt	= sumwt + wt
		    ;	s(jj)	= s(jj) / (0.921034 * m(jj))^2
		    ;	m(jj)	= 25. - 1.0857362 * alog10(m(jj))

		    endfor

		    b = sqrt(nobs/(nobs-1.))
		    a = 1.2533 * a / sumwt	; sigma (1 obs.)
		    rr = rr / nobs
		    a = 1.0857362 * a * b / mm(i)
		    sum = 1.2533 * b * sum / sumwt

		    ss(i) 	= sumerr / (0.921034 * mm(i))^2
		    mm(i)	= 25. - 1.0857362 * alog10(mm(i))
		    chi(i)	= sumchi
		    sharp(i)	= sumshp
		    var(i)	= sum
		    blunder(i)	= rr

		endif else begin
		    mm(i)	= fmags(iobs,i)
		    ss(i)	= fmerrs(iobs,i)
		    chi(i)	= gchis(iobs,i)
		    sharp(i)	= gshrps(iobs,i)
		    var(i)	= 0.
		    blunder(i)	= 1.
		endelse
	    endif else begin
	        mm(i)		= 99.999
		ss(i)		= 9.999
		chi(i)		= 9.999
		sharp(i)	= 99.999
		var(i)		= 0.
		blunder(i)	= 1.
	    endelse

	    if i mod 100L eq 0 then $
	    	print,string(13B),'stars calculated: (',i,'/',nstars,')',$
			format='($,a1,a,i5,a1,i5,a1)'

	endfor

	print,string(13B),'Stars calculated: ',i, '        ',$
		format='($,a1,a,i6,a8)'
	print,' '

; get julian dates for filter

	ndates = n_elements(flist)
	jd = dblarr(ndates)
	for i=0,ndates-1 do jd(i) = jdfname(flist(i))

; sort data
	
	s = sort(jd)

	jd = jd(s)
	fmags = fmags(s,*)
	fmerrs = fmerrs(s,*)

; write out data

	ofile = root + filts(flt) + '.dat'
	openw,olun,ofile,/get_lun

	writeu,olun,ndates,nstars
	writeu,olun,jd
	writeu,olun,ids,x,y,mm,ss,nframes,chi,sharp,var,blunder,fmags,fmerrs

	free_lun,olun

	print,'Wrote file: ',ofile

    endif else $
	print,'No data for filter: ',filts(flt)


endfor	; end loop over filters (flt)
;
return
end
