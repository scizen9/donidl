; $Id: tf_plcurve.pro,v 1.8 2014/05/19 18:30:05 neill Exp $
pro tf_plcurve,suf,w1phot=w1phot, w2phot=w2phot, ps=ps, dataout=dataout, $
	titleoff=titleoff
;+
; tf_plot - generate plots of Tully-Fisher relation
;-
	;
	; check inputs
	if n_params(0) lt 1 then $
		suf = 'wd'
	;
	; set up plot
	th = 5
	si = 1.5
	a=[findgen(40)*(!pi*2/40.),0.]
	usersym,cos(a),sin(a),/fill,thick=th
	;
	; labels and suffixes
	;
	; no color correction for WISE mags
	clab = ''
	pstail = '_w1'
	ylab = 'W1!S!Ub,i,k,a!N!R!DT!N      [AB]'
	ymlab = 'M!S!Ub,i,k,a!N!R!DW1!N    [AB]'
	if keyword_set(w2phot) then begin
		pstail = '_w2'
		ylab = 'W2!S!Ub,i,k,a!N!R!DT!N      [AB]'
		ymlab = 'M!S!Ub,i,k,a!N!R!DW2!N    [AB]'
	endif
	xlab = 'Log W!S!Ui!N!R!Dmx!N'
	q=''
	;
	; do calibration
	tf_curve,suf,univ,unizp,unilin,allind,allfix,data,clsts,clids, $
		xdat, xerr, ydat, yerr, tdat, dataout=dataout, $
		w1phot=w1phot, w2phot=w2phot
	nf = n_elements(clids)
	;
	; get offsets
	offs = allfix[0].izp - allfix.izp
	;
	; get plot ranges
	xrng = get_plotlims(ydat,yerr)
	yrng = get_plotlims(xdat,xerr,/magnitudes)
	; ===============================================================
	; make final ensemble plot
	; ===============================================================
	if keyword_set(ps) then begin
		psfile,'tfcur_ens' + pstail
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
	xx = findgen(150)/100. + 1.9
	yy = univ.zp + (xx-2.5) * univ.sl + univ.cv * (xx-2.5)^2
	yyl= unilin.izp + (xx-2.5) * unilin.isl
	oplot,xx,yyl,thick=th*2.,linesty=5,color=colordex('red')
	oplot,xx,yy,thick=th*2.,color=colordex('green')
	leg = ['zp,slope,cv: '+string(univ.zp,textoidl(' \pm '),univ.zp_sig, $
		univ.sl,textoidl(' \pm '),univ.sl_sig, $
		univ.cv,textoidl(' \pm '),univ.cv_sig, $
		form='(f6.2,a,f4.2,2x,f6.2,a,f4.2,2x,f6.2,a,f4.2)'), $
		textoidl('\chi^2_{\nu}')+',RMS: '+string(univ.redchi2,univ.rms, $
		form='(f5.2,1x,f4.2)')]
	xp0 = !x.crange[0] - 0.01 * (!x.crange[1] - !x.crange[0])
	yp0 = !y.crange[1] + 0.01 * (!y.crange[0] - !y.crange[1])
	legend,leg,charsi=si,charthi=th,box=0,spac=2.5,pos=[xp0,yp0]
	; ===============================================================
	; make final ensemble plot showing types
	; ===============================================================
	if keyword_set(ps) then begin
		psfile,'tfcur_typ' + pstail
		!p.font=0
	endif
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	ci = [cgcolor('Blue'), cgcolor('Red'), cgcolor('Green'), $
	      cgcolor('Cyan'), cgcolor('Magenta'), cgcolor('Steel Blue'), $
	      cgcolor('PUR3'), cgcolor('TG1'), cgcolor('RED6'), $
	      cgcolor('Sky Blue'), cgcolor('Forest Green'), cgcolor('Orange'), $
	      cgcolor('Plum')]
        gtypes = ['S0/a','Sa','Sab','Sb','Sbc','Sc','Scd','Sd','Sdm','Sm','Im']
	;
	; plot
	if keyword_set(titleoff) then $
		tlab = '' $
	else	tlab = clab
	plot,ydat,xdat,title=tlab,charthi=th,charsi=si,thick=th, $
		xran=xrng,xsty=1,xthick=th,xtitle=xlab, $
		yran=yrng,ysty=1,ythick=th,ytitle=ylab, $
		/nodata, xticks=2,xtickinterval=0.5
	oploterror,ydat,xdat,yerr,xerr,psym=3
	xdel = !x.crange[1] - !x.crange[0]
	ydel = !y.crange[0] - !y.crange[1]
	x0 = !x.crange[0] + xdel*0.05
	y0 = !y.crange[1] + ydel*0.05
	dy = ydel*0.04
	for i=0,10 do begin
		if i eq 0 then $
			tt=where(tdat le float(i), ntt) $
		else	tt=where(tdat le float(i) and tdat gt float(i-1), ntt)
		if ntt gt 0 then $
			oplot,ydat[tt],xdat[tt],psym=8,color=ci[i]
		oplot,[x0,x0],[y0,y0],psym=8,color=ci[i]
		xyouts,x0+.01,y0+0.1,gtypes[i] + ' ' + strn(ntt),color=ci[i]
		y0 = y0 + dy
	endfor
	;
	; plot fit
	xx = findgen(150)/100. + 1.9
	yy = univ.zp + (xx-2.5) * univ.sl + univ.cv * (xx-2.5)^2
	oplot,xx,yy,thick=th
	; ===============================================================
	; make all figure
	; ===============================================================
	if keyword_set(ps) then begin
		psfile,'tfcur_all' + pstail
		!p.font=0
	endif else read,'next: ',q
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
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
	isl = unizp.sl
	isle = unizp.sl_sig
	icv = unizp.cv
	icve = unizp.cv_sig
	legend,['Slope = '+string(isl,'(f6.2)')+textoidl(' \pm ')+ $
		string(isle,'(f4.2)'), $
		'Curve = '+string(icv,'(f5.2)')+textoidl(' \pm ')+ $
		string(icve,'(f4.2)')],/bottom,/right,box=0,charsi=si, $
		charthi=th,spac=2.5
	if keyword_set(ps) then psclose
	; ===============================================================
	; make Virgo figure
	; ===============================================================
	if keyword_set(ps) then begin
		psfile,'tfcur_virgo' + pstail
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
	oplot,xx,yy,thick=th
	yi = allind[0].izp + (!x.crange - 2.5) * allind[0].isl
	oplot,!x.crange,yi,thick=th,linesty=2
	legend,strn(ngals)+' '+clsts[0],charsi=si,charthi=th,box=0
	if keyword_set(ps) then psclose
	; ===============================================================
	; make others figure
	; ===============================================================
	if keyword_set(ps) then begin
		psfile,'tfcur_others' + pstail
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
		yi = allind[i].izp + (!x.crange-2.5) * allind[i].isl
		oplot,!x.crange,yi,thick=th,linesty=2
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
		psfile,'tfcur_zp' + pstail
		!p.font=0
	endif else read,'next: ',q
	;
	; get final zero point
	zp = allfix[0].zp - unizp.zp
	izp = unizp.zp
	izpe = unizp.zp_sig
	yy = unizp.zp + (xx-2.5) * unizp.sl + unizp.cv * (xx-2.5)^2
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
	oplot,xx,yy,thick=th
	legend,[strn(ngals)+' Zeropoint Galaxies', $
	        'ZP = '+string(izp,'(f6.2)')+textoidl(' \pm ')+ $
		string(izpe,'(f4.2)')],charsi=si,charthi=th,box=0,spac=2.5
	if keyword_set(ps) then psclose
	; ===============================================================
	; make zeropoint all figure
	; ===============================================================
	if keyword_set(ps) then begin
		psfile,'tfcur_zpall' + pstail
		!p.font=0
	endif else read,'next: ',q
	;
	; get final zero point
	zp = allfix[0].zp - unizp.zp
	isl = unizp.sl
	isle = unizp.sl_sig
	icv = unizp.cv
	icve = unizp.cv_sig
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
	oplot,xx,yy,thick=th
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
	legend,['ZP = '+string(izp,'(f6.2)')+textoidl(' \pm ') + $
		string(izpe,'(f4.2)'), $
		'Slope = '+string(isl,'(f6.2)')+textoidl(' \pm ')+ $
		string(isle,'(f4.2)'), $
		'Curve = '+string(icv,'(f5.2)')+textoidl(' \pm ')+ $
		string(icve,'(f4.2)')],/bottom,/right,box=0,charsi=si, $
		charthi=th,spac=2.5
	if keyword_set(ps) then psclose
	print,''
	;
	return
end
