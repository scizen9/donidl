; $Id: tf_curve.pro,v 1.6 2014/05/15 17:38:56 neill Exp $
pro tf_curve,suf,univ,unizp,unilin,allind,allfix,data,clsts,clids, $
	xdat, xerr, ydat, yerr, tdat, dataout=dataout, $
	w1phot=w1phot, w2phot=w2phot
;+
; tf_curve - Calibrate the Tully-Fisher relation with curvature
;-
	;
	; get started
	pre = 'TF_CURVE'
	version = repstr('$Revision: 1.6 $ $Date: 2014/05/15 17:38:56 $','$','')
	;
	; check inputs
	if n_params(0) lt 1 then $
		suf = 'w'
	;
	; read in the data
	tf_rd,suf,data,clsts,clids,vcmbs,vcmbe,bias,w1phot=w1phot,w2phot=w2phot
	;
	; get photometry type
	ptype = 'W1'	; default
	mtype = 'Asym'	; default
	biasc = 0.004
	biase = 2.500
	biasf = 1.000
	if keyword_set(w2phot) then $
		ptype = 'W2'
	ptype = ptype + '_' + mtype
	print,''
	print,pre,': ',version
	print,'Calibrating TF with '+ptype+' photometry.'
	;
	; output resulting fit data
	if keyword_set(dataout) then begin
		;
		; fit file
		ffile = 'tfcfit_'+ptype+'.dat'
		filestamp,ffile,/arch
		openw,fl,ffile,/get_lun
		printf,fl,'# '+pre+': '+version
		printf,fl,'# Run on '+systime(0)
		printf,fl,'# Photometry type: '+ptype
		printf,fl,'# Fit output (this) filename: '+ffile
		printf,fl,'# Asymptotic magnitudes'
		;
		; header
		printf,fl,'# Sample        Ngal  ZP(mag)   ZPerr   Slope    Slerr   Curve    Cuerr  ch^2/dof   RMS    type     Vcmb(kms)     VcmbErr    bias'
		;
		; data file
		dfile = 'tfcdat_'+ptype+'.dat'
		filestamp,dfile,/arch
		openw,dl,dfile,/get_lun
		printf,dl,'# '+pre+': '+version
		printf,dl,'# Run on '+systime(0)
		printf,dl,'# Photometry type: '+ptype
		printf,dl,'# Galaxy data output (this) filename: '+dfile
		printf,dl,'# Asymptotic magnitudes'
		;
		; header
		printf,dl,'# pgc    axr    inc   cmg       mge     omg       omge     bika     delu     ImgAB    I-cmg    err     W1-W2     err      cc     Wmx  Wmxi  lWmxi lerr   TFabmg    Dmod      bias   clid'
	endif	; keyword_set(dataout) 
	;
	; number of clusters
	ncl = n_elements(clids)
	;
	; define a struct to keep fit results in
	fitres = { $
		zp:0., $	; zero point
		sl:0., $	; slope
		cv:0., $	; curvature term
		zp_sig:0., $	; zero point sigma
		sl_sig:0., $	; slope sigma
		cv_sig:0., $	; curvature sigma
		izp:0., $	; inverse zero point
		isl:0., $	; inverse slope
		icv:0., $	; inverse curvature
		izp_sig:0., $	; inverse zero point sigma
		isl_sig:0., $	; inverse slope sigma
		icv_sig:0., $	; inverse curvature sigma
		np:0, $		; number of points fit
		nit:0, $	; number of iterations
		redchi2:0., $	; Reduced Chi^2 of fit
		rms:0., $	; RMS scatter in magnitudes
		cc:0, $		; Color corrected? 0 - no, 1 - yes
		type:'', $	; type of fit
		vcmb:0., $	; CMB velocity
		vcmbe:0., $	; CMB velocity error
		bias:0.	$	; Bias correction
	}
	;
	; all clusters fit individually
	allind = replicate(fitres, ncl)	; free individual fits
	allfix = replicate(fitres, ncl)	; fixed universal slope
	;
	; populate with data
	allind.vcmb = vcmbs
	allind.vcmbe= vcmbe
	allind.bias = bias
	allfix.vcmb = vcmbs
	allfix.vcmbe= vcmbe
	allfix.bias = bias
	;
	; universal linear fit
	unilin = fitres
	;
	; universal slope fit
	univ = fitres
	;
	; universal zero point fit
	unizp = fitres
	;
	; cluster zero points
	zps = fltarr(ncl)
	;
	; Fit individual clusters first
	print,''
	print,'Inidividual fits to each cluster:'
	for i=0,ncl-1 do begin
		;
		; get appropriate galaxies
		c = where(data.clid eq clids[i], nc)
		;
		; corrected line widths, errors
		lclw = data[c].lclw
		lclwe= data[c].lclwe
		;
		; photometry
		mg   = data[c].mg
		mge  = data[c].mge
		;
		; fit
		res = mpfitfun('linfunc',mg,lclw,lclwe,[0.,1.], $
			bestnorm=chisq, perror=sigma, /quiet)
		;
		; store
		allind[i].np		= nc
		allind[i].type		= 'free'
		allind[i].zp		= res[0]
		allind[i].sl		= res[1]
		allind[i].zp_sig	= sigma[0]
		allind[i].sl_sig	= sigma[1]
		allind[i].izp		= (2.5-res[0])/res[1]
		allind[i].isl		= 1./res[1]
		;
		; rms
		yfit = allind[i].izp + allind[i].isl * (lclw - 2.5)
		;allind[i].rms		= sqrt(total((mg-yfit)^2)/(nc-2))
		allind[i].rms		= stddev(mg-yfit)
		;
		; uncertainties
		;allind[i].izp_sig	= sqrt( (sigma[0]/res[1])^2 + $
		;		sigma[1]^2 * ( (res[0]-2.5)/res[1]^2 )^2 )
		allind[i].izp_sig	= allind[i].rms/sqrt(nc)
		allind[i].isl_sig	= sigma[1] / res[1]^2
		allind[i].redchi2	= chisq/(nc-2)
		;
		; report
		clstr = repstr(clsts[i],' ','_')
		print,clsts[i],nc,allind[i].izp,' +-',allind[i].izp_sig, $
			allind[i].isl,' +-',allind[i].isl_sig, $
			allind[i].icv,' +-',allind[i].icv_sig, $
			allind[i].redchi2,allind[i].rms,allind[i].type, $
			allind[i].vcmb,' +- ',allind[i].vcmbe,allind[i].bias, $
		format='(a-15,i5,3(f8.3,a,f6.3),2f9.3,2x,a-8,f9.3,a,2f9.3)'
		if keyword_set(dataout) then $
		printf,fl,clstr,nc,allind[i].izp,' +-',allind[i].izp_sig, $
			allind[i].isl,' +-',allind[i].isl_sig, $
			allind[i].icv,' +-',allind[i].icv_sig, $
			allind[i].redchi2,allind[i].rms,allind[i].type, $
			allind[i].vcmb,' +- ',allind[i].vcmbe,allind[i].bias, $
		format='(a-15,i5,3(f8.3,a,f6.3),2f9.3,2x,a-8,f9.3,a,2f9.3)'
	endfor
	;
	; now we iterate to find universal slope
	;
	; this struct tells the fitter to fix the slope and fit the
	; zero points
	pi = replicate({fixed:0, limited:[0,0], limits:[0.D,0.D]},2)
	pi[1].fixed = 1
	;
	; initial universal slope based on Virgo (cluster 0)
	univ.zp = allind[0].zp
	univ.sl = allind[0].sl
	;
	; highest number clusters to use in each iteration
	clim = [2,5,8,12]
	;
	; loop over highest numbers
	print,''
	print,'Iterate to derive '+ptype+' Univsersal Slope:'
	for j=0,n_elements(clim)-1 do begin
		;
		; report clusters fitted
		for i=0,clim[j] do $
			print,clsts[i]+' ', format='($,a)'
		print,':'
		print,'It#   Slope     DeltaSlope'
		;
		; convergence criterion
		slope_delta = 1.e9
		slope_epsilon = 0.00001
		;
		; reset number of iterations
		univ.nit = 0
		;
		; keep going until we converge
		while slope_delta gt slope_epsilon do begin
			;
			; ensemble variables
			xdat = [0.]
			xerr = [0.]
			ydat = [0.]
			yerr = [0.]
			tdat = [0.]
			;
			; accumulate ensemble
			for i=0,clim[j] do begin
				;
				; get appropriate galaxies
				c = where(data.clid eq clids[i], nc)
				;
				; corrected line widths, errors
				lclw = data[c].lclw
				lclwe= data[c].lclwe
				;
				; photometry
				mg   = data[c].mg
				mge  = data[c].mge
				;
				; type
				typ  = data[c].tyn
				;
				; fit with fixed slope to get zero points
				res = mpfitfun('linfunc',mg,lclw,lclwe,[0.,univ.sl], $
					parinfo=pi, /quiet)
				;
				; get zero point
				zps[i] = (2.5-res[0])/res[1]
				;
				; accumulate zero-point corrected photometry
				xdat = [xdat, mg+(zps[0] - zps[i])]
				xerr = [xerr, mge]
				ydat = [ydat, lclw]
				yerr = [yerr, lclwe]
				tdat = [tdat, typ]
			endfor
			;
			; fit ensemble
			xdat = xdat[1:*]
			xerr = xerr[1:*]
			ydat = ydat[1:*]
			yerr = yerr[1:*]
			tdat = tdat[1:*]
			;
			; fit slope and zeropoint, but start with
			; last slope guess
			res = mpfitfun('linfunc',xdat,ydat,yerr, [0.,univ.sl], $
		    		perror=sigma, bestnorm=chisq, /quiet)
			;
			; calculate convergence criterion
	    		slope_delta = abs(univ.sl - res[1])
			;
			; update universal values
			univ.np		= n_elements(xdat)
			univ.type	= 'free'
			univ.nit	= univ.nit + 1
			univ.zp		= res[0]
			univ.sl		= res[1]
			univ.zp_sig	= sigma[0]
			univ.sl_sig	= sigma[1]
			univ.izp	= (2.5-res[0])/res[1]
			univ.isl	= 1./res[1]
			;
			; rms
			yfit = univ.izp + univ.isl * (ydat - 2.5)
			univ.rms	= stddev(xdat-yfit)
			;univ.rms	= sqrt(total((xdat-yfit)^2)/(univ.np-2))
			;
			; uncertainties
			;univ.izp_sig	= sqrt( (sigma[0]/res[1])^2 + $
			;	sigma[1]^2 * ( (res[0]-2.5)/res[1]^2 )^2 )
			univ.izp_sig	= univ.rms/sqrt(univ.np)
			univ.isl_sig	= sigma[1] / res[1]^2
			univ.redchi2	= chisq/(n_elements(xdat)-2)
	    		print,univ.nit,univ.sl,slope_delta, $
				format='(i3,f12.6,f9.6)'
		endwhile
	endfor	; loop over limits (j)
	;
	; store in universal linear struct
	unilin = univ
	;
	; report
	print,ptype+' Universal Slope',univ.np,univ.izp,' +-',univ.izp_sig, $
		univ.isl,' +-',univ.isl_sig, $
		univ.icv,' +-',univ.icv_sig, $
		univ.redchi2,univ.rms,univ.type, $
		format='(a-22,i5,3(f8.3,a,f6.3),2f9.3,2x,a)'
	if keyword_set(dataout) then $
	printf,fl,ptype+'_Univ',univ.np,univ.izp,' +-',univ.izp_sig,$
		univ.isl,' +-',univ.isl_sig,univ.icv,' +-',univ.icv_sig, $
		univ.redchi2,univ.rms, $
		univ.type,univ.vcmb,' +- ',univ.vcmbe,univ.bias,$
		format='(a-15,i5,3(f8.3,a,f6.3),2f9.3,2x,a-8,f9.3,a,2f9.3)'
	;
	; project lw errs onto relation to get mag errs
	yhi = ydat + yerr
	xfit = univ.izp + univ.isl * (ydat - 2.5)
	xhi  = univ.izp + univ.isl * (yhi  - 2.5)
	merr = abs(xhi - xfit)
	;
	; fit curved direct relation
	res = mpfitfun('curfunc',(ydat-2.5),xdat,merr, [0.,univ.sl,-5.], $
		perror=sigma, bestnorm=chisq, /quiet)
	univ.zp = res[0]
	univ.sl = res[1]
	univ.cv = res[2]
	univ.zp_sig = sigma[0]
	univ.sl_sig = sigma[1]
	univ.cv_sig = sigma[2]
	univ.izp = res[0]
	univ.isl = res[1]
	univ.icv = res[2]
	univ.izp_sig = sigma[0]
	univ.isl_sig = sigma[1]
	univ.icv_sig = sigma[2]
	yfit = univ.zp + univ.sl * (ydat-2.5) + univ.cv * (ydat-2.5)^2
	univ.rms = stddev(xdat - yfit)
	univ.redchi2	= chisq/(n_elements(xdat)-2)
	;
	; report
	print,ptype+' Universal Curve',univ.np,univ.zp,' +-',univ.zp_sig, $
		univ.sl,' +-',univ.sl_sig, $
		univ.cv,' +-',univ.cv_sig, $
		univ.redchi2,univ.rms,univ.type, $
		format='(a-22,i5,3(f8.3,a,f6.3),2f9.3,2x,a)'
	if keyword_set(dataout) then $
	printf,fl,ptype+'_Ucur',univ.np,univ.zp,' +-',univ.zp_sig,$
		univ.sl,' +-',univ.sl_sig,univ.cv,' +-',univ.cv_sig, $
		univ.redchi2,univ.rms, $
		univ.type,univ.vcmb,' +- ',univ.vcmbe,univ.bias,$
		format='(a-15,i5,3(f8.3,a,f6.3),2f9.3,2x,a-8,f9.3,a,2f9.3)'
	;
	; determine magnitude offsets for each galaxy relative to univ. slope
	;
	; check each cluster
	for i=0,ncl-1 do begin
		;
		; get appropriate galaxies
		c = where(data.clid eq clids[i], nc)
		;
		; loop over each galaxy
		for j=0,nc-1 do begin
			;
			; corrected line widths
			lclw = (data[c[j]].lclw - 2.5)
			;
			; predicted magnitude using universal values
			pmag = univ.zp + univ.sl * lclw + univ.cv * lclw^2
			;
			; delta from universal
			mg = data[c[j]].mg+(zps[0] - zps[i])
			data[c[j]].delu = mg - pmag
		endfor
	endfor
	;
	; now fit zero point sample
	print,''
	print,'Fit zero-point galaxies using '+ptype+' Universal Curve:'
	;
	; get appropriate galaxies
	c = where(data.clid gt max(clids), nc)
	;
	; corrected line widths, errors
	lclw = (data[c].lclw - 2.5)
	lclwe= data[c].lclwe
	;
	; photometry
	mg   = data[c].mg
	mge  = data[c].mge
	;
	; project lw errs onto relation to get mag errs
	yhi = lclw + lclwe
	xfit = univ.zp + univ.sl * lclw + univ.cv * lclw^2
	xhi = univ.izp + univ.isl * yhi + univ.cv * yhi^2
	merr = abs(xhi - xfit)
	;
	; fit
	pi = replicate({fixed:0, limited:[0,0], limits:[0.D,0.D]},3)
	pi[1].fixed = 1
	pi[2].fixed = 1
	res = mpfitfun('curfunc',lclw,mg,merr,[0.,univ.sl,univ.cv], $
		bestnorm=chisq, parinfo=pi, perror=sigma, /quiet)
	;
	; store
	unizp.np	= nc
	unizp.type	= 'fixed'
	unizp.zp	= res[0]
	unizp.sl	= res[1]
	unizp.cv	= res[2]
	unizp.zp_sig	= sigma[0]
	unizp.sl_sig	= univ.sl_sig
	unizp.cv_sig	= univ.cv_sig
	unizp.izp	= res[0]
	unizp.isl	= res[1]
	unizp.icv	= res[2]
	unizp.izp_sig	= sigma[0]
	unizp.isl_sig	= univ.sl_sig
	unizp.icv_sig	= univ.cv_sig
	;
	; rms
	yfit = unizp.zp + unizp.sl * lclw + unizp.cv * lclw^2
	unizp.rms	= stddev(mg-yfit)
	;
	; uncertainties
	unizp.redchi2	= chisq/(nc-2)
	;
	; TF abs mag
	data[c].abmag = unizp.zp + unizp.sl * lclw + unizp.cv * lclw^2
	;
	; offsets
	;
	; predicted magnitude using universal values
	pmag = univ.zp + univ.sl * lclw + univ.cv * lclw^2
	data[c].delu = mg - pmag
	;
	; report
	print,ptype+' Zero Point Gals',nc,unizp.zp,' +-',unizp.zp_sig, $
		unizp.sl,' +-',unizp.sl_sig, $
		unizp.cv,' +-',unizp.cv_sig, $
		unizp.redchi2,unizp.rms,unizp.type, $
		format='(a-22,i5,3(f8.3,a,f6.3),2f9.3,2x,a)'
	if keyword_set(dataout) then $
	printf,fl,ptype+'_ZP',nc,unizp.zp,' +-',unizp.zp_sig, $
		unizp.sl,' +-',unizp.sl_sig, unizp.cv,' +-',unizp.cv_sig, $
		unizp.redchi2,unizp.rms, $
		unizp.type,unizp.vcmb,' +- ',unizp.vcmbe,unizp.bias, $
		format='(a-15,i5,3(f8.3,a,f6.3),2f9.3,2x,a-8,f9.3,a,2f9.3)'
	;
	; Fit individual clusters using universal slope
	print,''
	print,'Inidividual fits to each cluster using '+ptype+' Universal Curve:'
	for i=0,ncl-1 do begin
		;
		; get appropriate galaxies
		c = where(data.clid eq clids[i], nc)
		;
		; corrected line widths, errors
		lclw = (data[c].lclw - 2.5)
		lclwe= data[c].lclwe
		;
		; photometry
		mg   = data[c].mg
		mge  = data[c].mge
		;
		; project lw errs onto relation to get mag errs
		yhi = lclw + lclwe
		xfit = univ.zp + univ.sl * lclw + univ.cv * lclw^2
		xhi = univ.izp + univ.isl * yhi + univ.cv * yhi^2
		merr = abs(xhi - xfit)
		;
		; TF abs mag
		data[c].abmag = unizp.zp + unizp.sl * lclw + unizp.cv * lclw^2
		;
		; distance modulus
		data[c].mu = data[c].mg - data[c].abmag
		;
		; bias
		data[c].bias = biasc * ((data[c].mu>31.) - 31.)^biase * biasf
		;
		; fit
		res = mpfitfun('curfunc',lclw,mg,merr,[0.,univ.sl,univ.cv], $
			bestnorm=chisq, parinfo=pi, perror=sigma, /quiet)
		;
		; store
		allfix[i].np		= nc
		allfix[i].type		= 'fixed'
		allfix[i].zp		= res[0]
		allfix[i].sl		= res[1]
		allfix[i].cv		= res[2]
		allfix[i].zp_sig	= sigma[0]
		allfix[i].sl_sig	= univ.sl_sig
		allfix[i].cv_sig	= univ.cv_sig
		allfix[i].izp		= res[0]
		allfix[i].isl		= res[1]
		allfix[i].icv		= res[2]
		allfix[i].izp_sig	= sigma[0]
		allfix[i].isl_sig	= univ.sl_sig
		allfix[i].icv_sig	= univ.cv_sig
		;
		; rms
		yfit = allfix[i].zp + allfix[i].sl * lclw + allfix[i].cv * lclw^2
		allfix[i].rms		= stddev(mg-yfit)
		;
		; uncertainties
		allfix[i].redchi2	= chisq/(nc-2)
		;
		; report
		clstr = repstr(clsts[i],' ','_')
		print,clsts[i],nc,allfix[i].zp,' +-',allfix[i].zp_sig, $
			allfix[i].sl,' +-',allfix[i].sl_sig, $
			allfix[i].cv,' +-',allfix[i].cv_sig, $
			allfix[i].redchi2,allfix[i].rms,allfix[i].type, $
			allfix[i].vcmb,' +- ',allfix[i].vcmbe,allfix[i].bias, $
		format='(a-15,i5,3(f8.3,a,f6.3),2f9.3,2x,a-8,f9.3,a,2f9.3)'
		if keyword_set(dataout) then $
		printf,fl,clstr,nc,allfix[i].zp,' +-',allfix[i].zp_sig, $
			allfix[i].sl,' +-',allfix[i].sl_sig, $
			allfix[i].cv,' +-',allfix[i].cv_sig, $
			allfix[i].redchi2,allfix[i].rms,allfix[i].type, $
			allfix[i].vcmb,' +- ',allfix[i].vcmbe,allfix[i].bias, $
		format='(a-15,i5,3(f8.3,a,f6.3),2f9.3,2x,a-8,f9.3,a,2f9.3)'
	endfor
	print,''
	if keyword_set(dataout) then free_lun,fl
	;
	; now print galaxy data
	if keyword_set(dataout) then begin
		for i=0,n_elements(data)-1 do begin
			printf,dl,data[i].pgc,data[i].axr,data[i].inc, $
				data[i].mg,data[i].mge, $
				data[i].mgo,data[i].mgoe, $
				data[i].off,data[i].delu, $
				data[i].img,data[i].col, $
				data[i].colerr,data[i].wcol,data[i].wcolerr, $
				data[i].cc,data[i].lw,data[i].lwi, $
				data[i].lclw,data[i].lclwe, $
				data[i].abmag,data[i].mu,data[i].bias, $
				data[i].clid, $
				format='(i7,2f6.2,12f9.3,2f6.0,2f6.3,3f9.3,2x,i04)'
		endfor	; for i=0,n_elements(data)-1
		free_lun,dl
	endif	; keyword_set(dataout)
	;
	return
end
