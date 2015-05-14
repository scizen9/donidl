; $Id: tf_plot.pro,v 1.29 2014/05/26 18:07:33 neill Exp $
pro tf_plot,suf,color=color,w1phot=w1phot, w2phot=w2phot, iphot=iphot, $
	offsets=offsets,neilloffs=neilloffs,tullyoffs=tullyoffs, $
	ps=ps,dataout=dataout,erradjust=erradjust, $
	magfile=magfile, extrap=extrap, disk=disk, titleoff=titleoff
;+
; tf_plot - generate plots of Tully-Fisher relation
;-
	;
	; check inputs
	if n_params(0) lt 1 then $
		suf = 'w'
	;
	; set up plot
	th = 5
	si = 1.5
	a=[findgen(40)*(!pi*2/40.),0.]
	usersym,cos(a),sin(a),/fill,thick=th
	;
	; labels and suffixes
	;
	; color correction for WISE mags?
	if keyword_set(color) then begin
		clab = ''
		pstail = '_w1_cc'
		ylab = 'C!DW1!N [AB]'
		ymlab = 'M!DC!LW1!N [AB]'
		if keyword_set(w2phot) then begin
			pstail = '_w2_cc'
			ylab = 'C!DW2!N [AB]'
			ymlab = 'M!DC!LW2!N [AB]'
		endif
	;
	; no color correction for WISE mags
	endif else begin
		clab = ''
		pstail = '_w1'
		ylab = 'W1!S!Ub,i,k,a!N!R!DT!N      [AB]'
		ymlab = 'M!S!Ub,i,k,a!N!R!DW1!N    [AB]'
		if keyword_set(w2phot) then begin
			pstail = '_w2'
			ylab = 'W2!S!Ub,i,k,a!N!R!DT!N      [AB]'
			ymlab = 'M!S!Ub,i,k,a!N!R!DW2!N    [AB]'
		endif
	endelse
	;
	; I-band has no color correction
	if keyword_set(iphot) then begin
		clab = ''
		pstail = '_i'
		ylab = 'I!S!Ub,i,k!N!R!DT!N    [Vega]'
		ymlab = 'M!S!Ub,i,k!N!R!DI!N     [Vega]'
	endif
	;
	; Magfile mags?
	if keyword_set(magfile) then begin
		clab = ' ' + magfile
		if keyword_set(extrap) then begin
			clab = clab + ' Extrap'
		endif else if keyword_set(disk) then begin
			clab = clab + ' Disk only'
		endif else clab = clab + ' Asym'
	endif
	xlab = 'Log W!S!Ui!N!R!Dmx!N'
	q=''
	;
	; Have we adjusted errors?
	if keyword_set(erradjust) then begin
		pstail = '_i_ae'
	endif
	;
	; do calibration
	tf_cal,suf,univ,unizp,allind,allfix,data,clsts,clids, $
		xdat, xerr, ydat, yerr, dataout=dataout, $
		offsets=offsets, neilloffs=neilloffs, tullyoffs=tullyoffs, $
		erradjust=erradjust, magfile=magfile, $
		extrap=extrap, disk=disk, $
		color=color, w1phot=w1phot, w2phot=w2phot, iphot=iphot
	nf = n_elements(clids)
	;
	; get offsets
	offs = allfix[0].izp - allfix.izp
	;
	; get plot ranges
	xrng = get_plotlims(ydat,yerr)
	yrng = get_plotlims(xdat,xerr,/magnitudes)
	; ===============================================================
	; make final ensemble fit
	; ===============================================================
	if keyword_set(ps) then begin
		psfile,'tfcal_ens' + pstail
		!p.font=0
	endif
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	;
	; plot
	if keyword_set(titleoff) then $
		tlab = '' $
	else	tlab = clab
	plot,ydat,xdat,title=tlab,charthi=th,charsi=si,thick=th, $
		xran=xrng,xsty=1,xthick=th,xtitle=xlab, $
		yran=yrng,ysty=1,ythick=th,ytitle=ylab, $
		/nodata, xticks=2,xtickinterval=0.5
	oploterror,ydat,xdat,yerr,xerr,psym=8
	;
	; plot fit
	yy = [0., 20.]
	xx = univ.zp + yy * univ.sl
	oplot,xx,yy,linesty=2,thick=th
	leg = ['zp,slope: '+string(univ.izp,textoidl('\pm'),univ.izp_sig, $
		univ.isl,textoidl('\pm'),univ.isl_sig, $
		form='(f6.2,1x,a,1x,f4.2,2x,f6.2,1x,a,1x,f4.2)'), $
		textoidl('\chi^2_{\nu}')+',RMS: '+string(univ.redchi2,univ.rms, $
		form='(f4.2,1x,f4.2)')]
	legend,leg,charsi=si,charthi=th,box=0,spac=2.5
	; ===============================================================
	; make all figure
	; ===============================================================
	if keyword_set(ps) then begin
		psfile,'tfcal_all' + pstail
		!p.font=0
	endif else read,'next: ',q
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	ci = [cgcolor('Blue'), cgcolor('Red'), cgcolor('Green'), $
	      cgcolor('Cyan'), cgcolor('Magenta'), cgcolor('Steel Blue'), $
	      cgcolor('PUR3'), cgcolor('TG1'), cgcolor('RED6'), $
	      cgcolor('Sky Blue'), cgcolor('Forest Green'), cgcolor('Orange'), $
	      cgcolor('Plum')]
	sr = sort(abs(offs))
	offs = offs[sr]
	ci = ci[sr]
	clsts = clsts[sr]
	clids = clids[sr]
	allind = allind[sr]
	allfix = allfix[sr]
	cli = where(data.clid eq clids[0], ngals)
	if keyword_set(color) then $
		cli = where(data.clid eq clids[0] and data.col gt -90, ngals)
	if keyword_set(titleoff) then $
		tlab = '' $
	else	tlab = clab
	plot,data[cli].lclw,data[cli].mg,charthi=th,charsi=si,thick=th, $
		xran=xrng,xsty=1,xthick=th,xtitle=xlab, $
		yran=yrng,ysty=1,ythick=th,ytitle=ylab, $
		/nodata, title=tlab, xticks=2,xtickinterval=0.5
	vsym,24,/fill
	oplot,data[cli].lclw,data[cli].mg,psym=8,color=ci[0],symsi=1.2
	oplot,xx,yy,thick=th
	xdel = !x.crange[1] - !x.crange[0]
	ydel = !y.crange[0] - !y.crange[1]
	x0 = !x.crange[0] + xdel*0.05
	y0 = !y.crange[1] + ydel*0.05
	dy = ydel*0.04
	xyouts,x0,y0,strn(ngals) + ' ' + clsts[0],color=ci[0]
	for i=1,nf-1 do begin
		if i le 2 then $
			vsym,i+2,/fill $
		else	vsym,6,/skel
		cli = where(data.clid eq clids[i], ngals)
		if keyword_set(color) then $
			cli = where(data.clid eq clids[i] and $
				    data.col gt -90, ngals)
		oplot,data[cli].lclw,data[cli].mg+offs[i],psym=8,color=ci[i],symsi=1.2
		;
		y0 = y0 + dy
		xyouts,x0,y0,strn(ngals) + ' ' + clsts[i] + ' + ' + $
			string(abs(offs[i]),form='(f4.2)'), color=ci[i]
	endfor
	isl = unizp.isl
	isle = unizp.isl_sig
	legend,['Slope = '+string(isl,'(f5.2)')+textoidl(' \pm ')+ $
		string(isle,'(f4.2)')],/bottom,/right,box=0,charsi=si, $
		charthi=th
	if keyword_set(ps) then psclose
	; ===============================================================
	; make Virgo figure
	; ===============================================================
	if keyword_set(ps) then begin
		psfile,'tfcal_virgo' + pstail
		!p.font=0
	endif else read,'next: ',q
	;
	; get magnitude range
	cli = where(data.clid gt 0 and data.clid lt 9000, ngals)
	yrng = get_plotlims(data[cli].mg,data[cli].mge,/magnitudes)
	cli = where(data.clid eq clids[0], ngals)
	if keyword_set(color) then $
		cli = where(data.clid eq clids[0] and data.col gt -90, ngals)
	vsym,24,/fill
	if keyword_set(titleoff) then $
		tlab = '' $
	else	tlab = clab
	plot,data[cli].lclw,data[cli].mg,charthi=th,charsi=si,thick=th, $
		xran=xrng,xsty=1,xthick=th,xtitle=xlab, $
		yran=yrng,ysty=1,ythick=th,ytitle=ylab, $
		/nodata, title=tlab, xticks=2,xtickinterval=0.5
	oploterror,data[cli].lclw,data[cli].mg,data[cli].lclwe,data[cli].mge, $
		psym=8
	xi = allind[0].zp + yy * allind[0].sl
	oplot,xx,yy,thick=th
	oplot,xi,yy,thick=th,linesty=2
	legend,strn(ngals)+' '+clsts[0],charsi=si,charthi=th,box=0
	if keyword_set(ps) then psclose
	; ===============================================================
	; make others figure
	; ===============================================================
	if keyword_set(ps) then begin
		psfile,'tfcal_others' + pstail
		!p.font=0
	endif else read,'next: ',q
	row = 0
	erase
	for i=1,nf-1 do begin
		colm = (i-1) mod 4
		if colm eq 0 then $
			yylab = ylab $
		else	yylab = ''
		if row eq 2 then $
			xxlab = xlab $
		else	xxlab = ''
		plot,data.lclw,data.mg,charthi=th,charsi=si,thick=th, $
			xran=xrng,xsty=1,xthick=th,xtitle=xxlab, $
			yran=yrng,ysty=1,ythick=th,ytitle=yylab, $
			/nodata, _extra=gang_plot_pos(4,3,colm,row), $
			xticks=2,xtickinterval=0.5
		cli = where(data.clid eq clids[i], ngals)
		if keyword_set(color) then $
			cli = where(data.clid eq clids[i] and $
				    data.col gt -90, ngals)
		oploterror,data[cli].lclw,data[cli].mg, $
			data[cli].lclwe,data[cli].mge,psym=8
		oplot,xx,yy-offs[i],thick=th
		xi = allind[i].zp + yy * allind[i].sl
		oplot,xi,yy,thick=th,linesty=2
		xp0 = !x.crange[0] - 0.01 * (!x.crange[1] - !x.crange[0])
		yp0 = !y.crange[1] + 0.05 * (!y.crange[0] - !y.crange[1])
		legend,strn(ngals)+' '+clsts[i],charthi=th,box=0, $
			pos=[xp0,yp0]
		if colm eq 3 then row += 1
	endfor
	if keyword_set(ps) then psclose
	!p.multi=0
	; ===============================================================
	; make zeropoint figure
	; ===============================================================
	if keyword_set(ps) then begin
		psfile,'tfcal_zp' + pstail
		!p.font=0
	endif else read,'next: ',q
	;
	; get final zero point
	zp = allfix[0].izp - unizp.izp
	izp = unizp.izp
	izpe = unizp.izp_sig
	;
	; get new y plot range
	yrng = get_plotlims(xdat-zp,xerr,/magnitudes)
	;
	; Start with Zero Point calibrators
	cli = where(data.clid gt max(clids), ngals)
	if keyword_set(color) then $
		cli = where(data.clid gt max(clids) and data.col gt -90, ngals)
	if keyword_set(titleoff) then $
		tlab = '' $
	else	tlab = clab
	plot,data[cli].lclw,data[cli].mg,charthi=th,charsi=si,thick=th, $
		xran=xrng,xsty=1,xthick=th,xtitle=xlab, $
		yran=yrng,ysty=1,ythick=th,ytitle=ymlab, $
		/nodata, title=tlab, xticks=2,xtickinterval=0.5
	oplot,[0.,2.5],[izp,izp],thick=th,color=colordex('red')
	oplot,[2.5,2.5],[izp,0],thick=th,color=colordex('red')
	vsym,24,/fill
	oplot,data[cli].lclw,data[cli].mg,psym=8,symsi=1.5,thick=th
	oploterror,data[cli].lclw,data[cli].mg, $
			data[cli].lclwe,data[cli].mge,psym=8,symsi=1.5
	oplot,xx,yy-zp,thick=th
	legend,[strn(ngals)+' Zeropoint Galaxies', $
	        'ZP = '+string(izp,'(f6.2)')+textoidl(' \pm ')+ $
		string(izpe,'(f4.2)')],charsi=si,charthi=th,box=0,spac=2.5
	if keyword_set(ps) then psclose
	; ===============================================================
	; make zeropoint all figure
	; ===============================================================
	if keyword_set(ps) then begin
		psfile,'tfcal_zpall' + pstail
		!p.font=0
	endif else read,'next: ',q
	;
	; get final zero point
	zp = allfix[0].izp - unizp.izp
	isl = unizp.isl
	isle = unizp.isl_sig
	;
	; get new y plot range
	yrng = get_plotlims(xdat-zp,xerr,/magnitudes)
	;
	; Start with Zero Point calibrators
	cli = where(data.clid gt max(clids), ngals)
	if keyword_set(color) then $
		cli = where(data.clid gt max(clids) and data.col gt -90, ngals)
	if keyword_set(titleoff) then $
		tlab = '' $
	else	tlab = clab
	plot,data[cli].lclw,data[cli].mg,charthi=th,charsi=si,thick=th, $
		xran=xrng,xsty=1,xthick=th,xtitle=xlab, $
		yran=yrng,ysty=1,ythick=th,ytitle=ymlab, $
		/nodata, title=tlab, xticks=2,xtickinterval=0.5
	vsym,24
	oplot,data[cli].lclw,data[cli].mg,psym=8,symsi=1.5,thick=th
	oplot,xx,yy-zp,thick=th
	xdel = !x.crange[1] - !x.crange[0]
	ydel = !y.crange[0] - !y.crange[1]
	x0 = !x.crange[0] + xdel*0.05
	y0 = !y.crange[1] + ydel*0.05
	dy = ydel*0.04
	xyouts,x0,y0,strn(ngals) + ' Zero Point Calibrators'
	for i=0,nf-1 do begin
		if i le 2 then $
			vsym,i+2,/fill $
		else	vsym,6,/skel
		if i eq 0 then vsym,24,/fill
		cli = where(data.clid eq clids[i], ngals)
		if keyword_set(color) then $
			cli = where(data.clid eq clids[i] and $
				    data.col gt -90, ngals)
		oplot,data[cli].lclw,data[cli].mg+offs[i]-zp,psym=8,color=ci[i],symsi=1.2
		;
		y0 = y0 + dy
		xyouts,x0,y0,strn(ngals) + ' ' + clsts[i], color=ci[i]
	endfor
	legend,['Slope = '+string(isl,'(f5.2)')+textoidl(' \pm ')+ $
		string(isle,'(f4.2)'), $
		'ZP = '+string(izp,'(f6.2)')+textoidl(' \pm ') + $
		string(izpe,'(f4.2)')],/bottom,/right,box=0,charsi=si, $
		charthi=th,spac=2.5
	if keyword_set(ps) then psclose
	; ===============================================================
	; make color term figure
	; ===============================================================
	if not keyword_set(color) then begin
		if keyword_set(ps) then begin
			psfile,'tfcal_color' + pstail
			!p.font=0
		endif else read,'next: ',q
		iplab = 'I!S!Ub,i,k!N!R!DT!N'
		xlab = iplab + '     - W1!S!Ub,i,k,a!N!R!DT!N      [AB]'
		ylab = 'W1!S!Ub,i,k,a!N!R!DT!N     - Mean Correlation'
		if keyword_set(w2phot) then begin
			xlab = iplab + '     - W2!S!Ub,i,k,a!N!R!DT!N      [AB]'
			ylab = 'W2!S!Ub,i,k,a!N!R!DT!N     - Mean Correlation'
		endif
		if keyword_set(iphot) then begin
			ylab = iplab + '     - Mean Correlation' 
		endif
		;
		; get good data
		g = where(data.col gt -90.,ng)
		col = data[g].col
		del = data[g].delu
		err = data[g].colerr
		lclw = data[g].lclw
		lerr = data[g].lclwe
		;
		; fit data
		res = mpfitfun('linfunc',col,del,err,[0.,0.],perror=sig, $
			weights=fltarr(ng)+1.,/quiet)
		lab = ['slope: '+string(res[1],"(f6.3)") + textoidl(' \pm ')+ $
			string(sig[1],"(f5.3)"), $
			'zero point: '+string(res[0],"(f6.3)")+textoidl(' \pm ')+ $
			string(sig[0],"(f5.3)")]
		;
		; log results
		cofile = 'tfcal_color' + pstail + '.dat'
		filestamp,cofile,/arch
		openw,colun,cofile,/get_lun
		printf,colun,'# TF_PLOT - run on '+systime(0)
		printf,colun,'# zp, sl, zperr, slerr'
		printf,colun,res,sig,format='(4f9.3)'
		free_lun,colun
		;
		; get new y plot range
		xrng = get_plotlims(col,err)
		yrng = get_plotlims(del,err,pad=0.3,/magnitudes)
		;
		; plot color offset
		vsym,24
		if keyword_set(titleoff) then $
			tlab = '' $
		else	tlab = clab
		plot,col,del,charthi=th,charsi=si*1.5,thick=th,psym=1,title=tlab, $
			xran=xrng,xsty=1,xthick=th,xtitle=xlab, $
			yran=yrng,ysty=1,ythick=th,ytitle=ylab
		;oploterr,col,del,err,3
		oplot,!x.crange,res[0]+!x.crange*res[1],linesty=0
		off = res[0]/res[1]
		yfit = (res[1]-sig[1])*(!x.crange + off)
		oplot,!x.crange,yfit,linesty=1
		yfit = (res[1]+sig[1])*(!x.crange + off)
		oplot,!x.crange,yfit,linesty=1
		legend,lab,charsi=2.0,charthi=3,spac=2.5,box=0
		;
		if keyword_set(ps) then psclose
	; ===============================================================
	; make color correlation figure
	; ===============================================================
		if keyword_set(ps) then begin
			psfile,'tfcal_ccorr' + pstail
			!p.font=0
		endif else read,'next: ',q
		res = mpfitfun('linfunc',col,lclw,lerr,[0.,0.],perror=sig, $
			weights=fltarr(ng)+1.,/quiet)
		lab = ['slope: '+string(res[1],"(f6.3)") + textoidl(' \pm')+ $
			string(sig[1],"(f5.3)"), $
			'zero point: '+string(res[0],"(f6.3)")+textoidl(' \pm')+ $
			string(sig[0],"(f5.3)")]
		;
		; plot color correlation
		yrng = get_plotlims(lclw,lerr,pad=0.3)
		ylab = 'Log W!S!Ui!N!R!Dmx!N'
		vsym,24
		plot,col,lclw,charthi=th,charsi=si,thick=th,psym=1,title=tlab, $
			xran=xrng,xsty=1,xthick=th,xtitle=xlab, $
			yran=yrng,ysty=1,ythick=th,ytitle=ylab
		;oploterr,col,lclw,lerr,3
		;oplot,!x.crange,res[0]+!x.crange*res[1],linesty=0
		;legend,lab,charsi=1.2,charthi=3
		if keyword_set(ps) then psclose
	endif
	; ===============================================================
	; make WISE color figure
	; ===============================================================
	if keyword_set(w2phot) then begin
		if keyword_set(ps) then begin
			psfile,'tfcal_wcol' + pstail
			!p.font=0
		endif else read,'next: ',q
		xlab = 'W1!S!Ub,i,k,a!N!R!DT!N     - W2!S!Ub,i,k,a!N!R!DT!N     [Vega]'
		ylab = 'W2!S!Ub,i,k,a!N!R!DT!N     - Mean Correlation'
		if keyword_set(color) then begin
			xlab = 'C!DW1!N - C!DW2!N [Vega]'
			ylab = 'W2!S!Ub,i,k,a,c!N!R!DT!N        - Mean Correlation'
		endif
		;
		; get good data
		g = where(data.wcol gt -90. and data.delu gt -90.,ng)
		col = data[g].wcol
		del = data[g].delu
		err = data[g].wcolerr
		sam = data[g]
		;
		; get extreme galaxies
		t=where(col eq min(col))
		print,'Bluest  (W1-W2,PGC,clid): ', $
			col[t],sam[t].pgc,sam[t].clid,format='(a,f9.3,2i7)'
		t=where(col eq max(col))
		print,'Reddest (W1-W2,PGC,clid): ', $
			col[t],sam[t].pgc,sam[t].clid,format='(a,f9.3,2i7)'
		;
		; get new y plot range
		xrng = get_plotlims(col,err)
		yrng = get_plotlims(del,err,pad=0.3,/magnitudes)
		;
		; plot color offset
		plot,col,del,charthi=th,charsi=si,thick=th,psym=1,title=tlab, $
			xran=xrng,xsty=1,xthick=th,xtitle=xlab, $
			yran=yrng,ysty=1,ythick=th,ytitle=ylab
		;
		if keyword_set(ps) then psclose
	endif
	print,''
	;
	return
end
