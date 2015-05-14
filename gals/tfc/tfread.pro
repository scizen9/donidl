; $Id: tfread.pro,v 1.25 2014/02/10 19:08:00 neill Exp $
;	tfread - read TF calibration data
pro tfread,tail,data,clsts,clids,vcmbs,vcmbe,bias, $
	w1phot=w1phot,w2phot=w2phot,iphot=iphot, $
	color=color,offsets=offsets,neilloffs=neilloffs,tullyoffs=tullyoffs, $
	erradjust=erradjust,magfile=magfile,extrap=extrap,disk=disk
	;
	; get started
	pre = 'TFREAD'
	version = repstr('$Revision: 1.25 $ $Date: 2014/02/10 19:08:00 $','$','')
	;
	; cluster names:
	clsts = ['Virgo','UMa','Fornax','Antlia','Centaurus','Pegasus','Hydra',$
		 'Pisces','Cancer','Abell 400','Abell 1367','Coma', $
		 'Abell 2634/66']
	;
	; cluster ids:
	clids = [0001, 0102, 0843, 0488, 0441, 2932, 0487, $
		 2044, 2331, 2092, 2453, 2493, $
		 2935] 
	vcmbs = [1495., 1079., 1358., 3198., 3823., 3062., 4088., $
		 4759., 5059., 7228., 6969., 7370., $
		 8938.]
	vcmbe = [  37.,   14.,   45.,   74.,   82.,   78.,   72., $
		   39.,   82.,   97.,   93.,   76., $
		  164.]
	;vcmbs = [1410., 1101., 1484., 3119., 3679., 3518., 4121., $
	;	 4779., 4940., 7108., 6923., 7194., $
	;	 8381.]
	bias = [0.000, 0.000, 0.001, 0.052, 0.003, 0.002, 0.011, $
		0.029, 0.017, 0.145, 0.098, 0.068, $
		0.085]
	if keyword_set(iphot) then $
		bias = [0.000, 0.000, 0.000, 0.040, 0.010, 0.000, 0.030, $
			0.010, 0.020, 0.070, 0.050, 0.050, $
			0.040] * 0.80 	; correction from TC2012 values
	;
	; AB offset for I-band
	aboffi = 0.342
	;
	; color correction coeff's
	if keyword_set(neilloffs) then begin
		ccw1sl = -0.583
		ccw1zp = -0.475
	endif else begin
		ccw1sl = -0.650
		ccw1zp = -0.514
	endelse
	;ccw2sl = -0.679
	;ccw2zp = -0.918
	ccw2sl = -0.669
	ccw2zp = -0.899
	;
	; color correction for magfile mags
	if keyword_set(magfile) then begin
		ccw1sl = -0.647
		ccw1zp = -0.512
		if keyword_set(extrap) then begin
			ccw1sl = -0.669
			ccw1zp = -0.527
		endif
		if keyword_set(disk) then begin
			ccw1sl = -0.844
			ccw1zp = -0.651
		endif
	endif
	;
	; data structure, one instance per galaxy
	ngal = 500
	data = replicate({ $
		pgc:0l, $	; PGC number
		axr:0., $	; Axial ratio
		inc:0., $	; Inclination (degrees)
		ni:0, $		; number of I-band obs?
		mg:0., $	; corrected magnitude
		mge:0., $	; error
		mgo:0., $	; observed magnitude
		off:0., $	; offset between observed and corrected mag
		delu:-99., $	; delta with universal fit
		col:-99., $	; color of galaxy (i-W1 or W2)
		colerr:-99., $	; error in the color
		wcol:-99., $	; WISE color W1 - W2
		wcolerr:-99., $	; error in WISE color
		cc:0., $	; color correction applied to mg
		lclw:0., $	; log corrected HI line width
		lclwe:0., $	; log corrected HI line width error
		clid:0l $	; cluster ID number
		}, ngal)
	;
	; get photometry type
	ptype = 'W1'	; default
	if keyword_set(w2phot) then $
		ptype = 'W2'
	if keyword_set(iphot) then $
		ptype = 'I-band'
	print,pre,': ',version
	print,pre,': reading '+ptype+' photometry.'
	;
	; read offsets if requested
	if keyword_set(offsets) then begin
		print,pre,': using GLGA data, offsets file = ',offsets
		common glgadb_info
		readcol,offsets,opgc,oid,offs1,offs2,format='l,a,f,f',/silent
		if keyword_set(neilloffs) then $
			offs = offs2 $
		else	offs = offs1
	endif
	;
	; read magfile if requested (should already be offset adjusted)
	if keyword_set(magfile) then begin
		print,pre,': magnitudes file = ',magfile
		readcol,magfile,fpgc,fid,foff,fmx,fmxe,fmd,fmde,fmas,fmase, $
			format='l,a,f,f,f,f,f,f'
		fmag = fmas
		fmagerr = fmase
		if keyword_set(extrap) then begin
			fmag = fmx
			fmagerr = fmxe
		endif
		if keyword_set(disk) then begin
			fmag = fmd
			fmagerr = fmde
		endif
	endif
	;
	; set up a pointer into the data
	p=0
	;
	; loop over cluster ids
	for i=0,n_elements(clids)-1 do begin
		file = string(clids[i],"(i04)")+'_'+tail
		if tail eq 'w' then $
			readcol,file,pgc,axr,inc,ni,img,sp1,sp2,sp3,sp4,w1, $
				lw,lwi,lclw,sgl,sgb,gid,lclwerr, $
				comment='#', /silent, $
				form='l,f,f,i,f,f,f,f,f,f,f,f,f,f,f,i,f' $
		else	readcol,file,pgc,axr,inc,ni,img,sp1,sp2,sp3,sp4,w1, $
				lw,lwi,lclw,sgl,sgb,gid,lclwerr,imge, $
				comment='#', /silent, $
				form='l,f,f,i,f,f,f,f,f,f,f,f,f,f,f,i,f,f'
		;
		; get only those with good I-band mags
		g = where(img ne 0.)
		;
		; convert I-band mags from Vega to AB
		img[g] = img[g] + aboffi
		for j=0,n_elements(pgc)-1 do begin
			;
			; get offset if requested
			nl = 0
			if keyword_set(offsets) then begin
				t=where(opgc eq pgc[j], nt)
				if nt ge 1 then begin
					l = glfind(oid[t[0]],count=nl)
				endif
			endif
			data[p].pgc	= pgc[j]
			data[p].axr	= axr[j]
			data[p].inc	= inc[j]
			data[p].ni	= ni[j]
			data[p].mg	= w1[j]
			data[p].mge	= 0.08
			if nl eq 1 then begin
				off = max(offs[t])
				data[p].mg = glgadat[l[0]].w1_asym_mag + off
				data[p].mgo = glgadat[l[0]].w1_asym_mag
				data[p].off = off
			endif
			;
			; use magfile if requested
			if keyword_set(magfile) then begin
				t = where(fpgc eq pgc[j], nt)
				if nt eq 1 then begin
					data[p].mg = fmag[t[0]]
					data[p].mge = fmagerr[t[0]]
				endif else if nt gt 1 then begin
					m = where(fmag[t] eq max(fmag[t]))
					data[p].mg = fmag[t[m]]
					data[p].mge = fmagerr[t[m]]
				endif
			endif
			;
			; get W1 mag for color when /iphot set
			w1r = data[p].mg
			w1re= data[p].mge
			if keyword_set(w2phot) then begin
				if nl eq 1 then begin
					off = max(offs[t])
					data[p].mg = glgadat[l[0]].w2_asym_mag $
						+ off
					data[p].mgo = glgadat[l[0]].w2_asym_mag
					data[p].off = off
					;
					; color in Vega system
					data[p].wcol = (w1r - data[p].mg)+0.64
					data[p].wcolerr = sqrt(w1re^2 + $
						glgadat[l[0]].w2_asym_magerr^2)
				endif else begin
					;
					; not available in data file yet
					data[p].mg = -9.99
				endelse
				data[p].mge = 0.08
			endif
			if keyword_set(iphot) then begin
				data[p].mg = img[j]
				if tail eq 'w' then $
					data[p].mge	= 0.1 $
				else	data[p].mge	= imge[j]
				;
				; use W1 only for color when /iphot set
				if img[j] ne 0. then begin
					data[p].col	=img[j]-w1r
					data[p].colerr	=sqrt(imge[j]^2+w1re^2)
				endif
			endif else begin
				if img[j] ne 0. then begin
					data[p].col	=img[j]-data[p].mg
					data[p].colerr	=sqrt(imge[j]^2 + data[p].mge^2)
				endif
			endelse
			data[p].lclw	= lclw[j]
			data[p].lclwe	= lclwerr[j]
			data[p].clid	= clids[i]
			;
			; account for photometric errors
			if keyword_set(iphot) and $
			   keyword_set(erradjust) then begin
				ilwe = data[p].mge / 8.97
				data[p].lclwe = sqrt( data[p].lclwe^2 + ilwe^2 )
			endif
			;
			; color correction?
			if not keyword_set(iphot) and $
			       keyword_set(color) and $
			       data[p].col gt -90. then begin
				;
				; W1 Color correction
				data[p].cc = ccw1zp + ccw1sl * data[p].col
				;
				; W2 Color correction
				if keyword_set(w2phot) then $
				    data[p].cc = ccw2zp + ccw2sl * data[p].col
				;
				; apply correction
				data[p].mg = data[p].mg - data[p].cc
			endif
			;
			; increment pointer
			p += 1
		endfor
	endfor
	;
	; report
	print,pre,': number of TF calibrator galaxies read = ',p
	;
	; now read zero point galaxies
	readcol,'zp37_w',pgc,axr,inc,ni,img,sp1,w1,lw,lwi,lclw, $
		sgl,sgb,dmod,v1,amgi,imge,lclwerr,amw1,comment='#', $
		form='l,f,f,i,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f',/silent
	;
	; convert I-band mags from Vega to AB
	g = where(amgi ne 0.)
	amgi[g] = amgi[g] + aboffi
	;
	; loop over zero point galaxies
	for j=0,n_elements(dmod)-1 do begin
		;
		; get offset if requested
		nl = 0
		if keyword_set(offsets) then begin
			t=where(opgc eq pgc[j], nt)
			if nt ge 1 then begin
				l = glfind(oid[t[0]],count=nl)
			endif
		endif
		data[p].pgc	= pgc[j]
		data[p].axr	= axr[j]
		data[p].inc	= inc[j]
		data[p].ni	= ni[j]
		data[p].mg	= amw1[j]
		data[p].mge	= 0.08
		if nl eq 1 then begin
			off = min(offs[t])
			data[p].mg = glgadat[l[0]].w1_asym_mag + off
			data[p].mgo = glgadat[l[0]].w1_asym_mag
			data[p].off = off
		endif
		;
		; use magfile if requested
		if keyword_set(magfile) then begin
			t = where(fpgc eq pgc[j], nt)
			if nt eq 1 then begin
				data[p].mg = fmag[t[0]]
				data[p].mge = fmagerr[t[0]]
			endif else if nt gt 1 then begin
				m = where(fmag[t] eq min(fmag[t]))
				data[p].mg = fmag[t[m]]
				data[p].mge = fmagerr[t[m]]
			endif
		endif
		;
		; get W1 mag for color when /iphot set
		w1r = data[p].mg
		w1re= data[p].mge
		if keyword_set(w2phot) then begin
			if nl eq 1 then begin
				off = min(offs[t])
				data[p].mg = glgadat[l[0]].w2_asym_mag + off
				data[p].mgo = glgadat[l[0]].w2_asym_mag
				data[p].off = off
				;
				; color in Vega system
				data[p].wcol = (w1r - data[p].mg)+0.64
				data[p].wcolerr = sqrt(w1re^2 + $
					glgadat[l[0]].w2_asym_magerr^2)
			endif else begin
				;
				; not available in data file yet
				data[p].mg = -9.99
			endelse
			data[p].mge	= 0.08
		endif
		if keyword_set(iphot) then begin
			data[p].mg	= amgi[j]
			data[p].mge	= imge[j]
			;
			; use W1 only for color when /iphot set
			if amgi[j] ne 0. then begin
				data[p].col	= amgi[j]-w1r
				data[p].colerr	= sqrt(imge[j]^2 + w1re^2)
			endif
		endif else begin
			if amgi[j] ne 0. then begin
				data[p].col	= amgi[j]-data[p].mg
				data[p].colerr	= sqrt(imge[j]^2 + data[p].mge^2)
			endif
		endelse
		data[p].lclw	= lclw[j]
		data[p].lclwe	= lclwerr[j]
		data[p].clid	= 9999
		;
		; account for photometric errors
		if keyword_set(iphot) and $
		   keyword_set(erradjust) then begin
			ilwe = data[p].mge / 8.97
			data[p].lclwe = sqrt( data[p].lclwe^2 + ilwe^2 )
		endif
		;
		; color correction?
		if not keyword_set(iphot) and $
		       keyword_set(color) and $
		       data[p].col gt -90. then begin
			;
			; W1 Color correction
			data[p].cc = ccw1zp + ccw1sl * data[p].col
			;
			; W2 Color correction
			if keyword_set(w2phot) then $
			    data[p].cc = ccw2zp + ccw2sl * data[p].col
			;
			; apply correction
			data[p].mg = data[p].mg - data[p].cc
		endif
		;
		; increment pointer
		p += 1
	endfor
	;
	; report
	print,pre,': number of Zero Point calibrator galaxies read = ', $
		n_elements(dmod)
	;
	; clean baddies
	good = where(data.mg ne 0., ngood)
	if ngood gt 0 then begin
		data = data[good]
	endif else begin
		data = -1
	endelse
	;
	; report
	print,pre,': number of good galaxies = ',ngood
	if keyword_set(iphot) then begin
		if keyword_set(erradjust) then $
			print,pre,': adjusting line-width errors using I-band errors.' $
		else	print,pre,': NOT adjusting line-width errors.'
	endif else begin
		if keyword_set(color) then $
			print,pre,': applying a color correction to WISE photometry.' $
		else	print,pre,': NOT Applying a color correction.'
	endelse
;
	return
end
