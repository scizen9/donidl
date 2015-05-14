;+
; NAME: mdb_sed
;
; 	Query MySQL database and plot an SED
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2005-10-12 03:12:09 by Marshall Perrin 
;-

PRO mdb_sed,name0,_extra=_extra,print=print,sigma=sigma,$
		overplot=overplot,multiplot=multiplot,properties=properties,$
		alternatenames=alternatenames,$
		kurucz=kurucz,observations=obs,$
		companions=companions,ps=ps,slope=slope

	if ~(keyword_set(sigma)) then sigma=2

	if ~(keyword_set(name0)) then message,"You have to enter an object name!"

;--------------- Get the data ----------------------

	common mysql, SQLhandle
	mysqlcheck,SQLhandle

	name = mdb_nameresolve(name0)
		 

	q = "SELECT filters.IsophotalWavelength,filters.Zeropoint_Jy,photometry.magnitude,photometry.mag_error, "+$
		" photometry.flux, photometry.flux_error, photometry.reference from filters,photometry where filters.band=photometry.band and "+$
		" photometry.name="+quote(name)+" order by filters.isophotalwavelength;"

	print,q

	mysqlquery,SQLhandle,q,wavelen,zp,mag,mag_error,flux,flux_error,src,format="(F,F,F,F,F,F,A)"

	fe0= flux_error
	noflux = where(~finite(flux),nofluxct)
	if nofluxct gt 0 then flux[noflux] = zp[noflux]*10.d^(-0.4*mag[noflux])
	noflux = where(~finite(flux_error),nofluxct)
	if nofluxct gt 0 then flux_error[noflux] = flux[noflux] * alog(10)/2.5 * mag_error[noflux]


	@constants.include
	nu = clight/(wavelen*micron)
	if ~(keyword_set(multiplot)) then begin
		title="SED for "+name
		ytitle=textoidl("\nu f_\nu [erg s^-1 cm^-2]")
	    xtitle=textoidl("Wavelength [\mum]")
	endif else begin
		; nothing now
	endelse

;--------------- Plot the data ----------------------

	if ~(keyword_set(overplot)) then begin
		nodata = (total(finite(flux*Jansky*nu)) eq 0) 
		if ~(keyword_set(nodata)) then $
			plot,wavelen,flux*Jansky*nu,$
		title=title,xtitle=xtitle,ytitle=ytitle,$
		/xlog,/ylog,psym=-3,$
		/xs,/ys,_extra=_extra,nodata=nodata
	endif

	if keyword_set(multiplot) then begin
		dx = !x.crange[0]+0.2*(!x.crange[1]-!x.crange[0])
		dy = !y.crange[0]+0.8*(!y.crange[1]-!y.crange[0])
		px = 10.^(dx)
		py = 10.^(dy)
		xyouts,px,py,name
	endif

	error = flux_error*Jansky*nu*sigma
	if total(finite(error)) gt 0 then $
	oploterror,wavelen,flux*Jansky*nu, flux_error*Jansky*nu*sigma,/nohat,$
		_extra=_extra

	; specially mark my data points
	wmine = where(strmatch(src,"My * data*"),mycnt,comp=notmine,ncomp=notminect)
	if notminect ge 1 then oplot,wavelen[notmine],(flux*Jansky*nu)[notmine],psym=1
	
	if keyword_set(ps) then mycolor=fsc_color('blue') else mycolor=fsc_color('yellow')
	if mycnt gt 0 then begin
		a = findgen(16)*(!pi*2/16.)
		usersym,cos(a),sin(a),/fill
		if keyword_set(!p.symsize) then symsize=!p.symsize else symsize=1.0
		oplot,wavelen[wmine],(flux*Jansky*nu)[wmine],psym=8,symsize=1.3*symsize,$
			color=mycolor
	endif
	
	; specially mark IRAS points
	wiras = where(strmatch(src,"IRAS PSC") or strmatch(src,"Weaver 1992"),irascnt)
	if irascnt gt 0 then begin
		a = findgen(16)*(!pi*2/16.)
		usersym,cos(a),sin(a),/fill
		oplot,wavelen[wiras],(flux*Jansky*nu)[wiras],psym=2,$
			color=fsc_color('red')
	endif
	wspitz = where(strmatch(src,"Spitzer*") ,spitzcnt)
	if spitzcnt gt 0 then begin
		a = findgen(16)*(!pi*2/16.)
		usersym,cos(a),sin(a),/fill
		oplot,wavelen[wspitz],(flux*Jansky*nu)[wspitz],psym=2,$
			color=fsc_color('orange')
	endif


;--------------- Overplot Kurucz model atmosphere? ----------------------

	if keyword_set(kurucz) then begin
		teff=mdb_getprop(name,"Teff",/log,ref=ref_t,/recent,/nostop,/one)
		dist=mdb_getprop(name,"Distance",ref=ref_t,/recent,/nostop,/one)
		radius=mdb_getprop(name,"Rstar",ref=ref_t,/recent,/nostop,/one)

		model = get_kurucz(teff,wave=modelwave)
		if radius eq 0 then begin
			radius=2.5
			message,'USING DEFAULT RADIUS GUESS!',/info
		endif 
		print, "Using radius = "+strc(radius)+" R_sun, dist="+strc(dist)+" pc"
		model *= (radius*Rsun / (dist*parsec))^2 * 4 * !pi
		; model is now in erg /cm^2 /s /Ang
		oplot,modelwave,model*(modelwave*micron/angstrom),$
			color=fsc_color('purple')


	endif
;--------------- Overplot lambda^-4/3 slope ----------------------
	; i.e. Hillenbrand group I objects typical slope
	; start from 2 microns to avoid heavily extincted optical points
	if keyword_set(slope) then begin 
		wk = where(strmatch(src,'2MASS') and wavelen eq 2.159 ,kcnt)
		;wk = where(strmatch(src,'Weaver 1992') and wavelen eq 60.000 ,kcnt)
		;stop
		if kcnt ge 1 then begin 
			startflux = (flux*Jansky*nu)[wk[0]]
			startlambda =  wavelen[wk[0]]
			plot_lambda = startlambda*2.0^(findgen(30)-10)
			; disk
			oplot,plot_lambda,      startflux * (plot_lambda/startlambda)^(-4/3)   ,lines=2,color=fsc_color('dark green')
			; rayleigh-jeans (star?)
			;  it's ^-4, but we multiply by lambda since we plot lambda flambda
			oplot,plot_lambda,      startflux * (plot_lambda/startlambda)^(-3)   ,lines=2,color=fsc_color('navy')
			
			hor,startflux,lines=2,color=fsc_color('maroon')
			
		end
		
	end
		
;--------------- print values? ----------------------


	if keyword_set(print) then begin
		print,"Wavelength          Mags          Flux (Jy)         Source"
		for i=0,n_elements(wavelen)-1 do begin
			print, wavelen[i], mag[i],flux[i],"         ",strsub(src[i],0,29)
		endfor
	endif


	if keyword_set(properties) then begin
			q="select otherproperties.property, otherproperties.value,"+$
			"otherproperties.reference "+$
			" from otherproperties where otherproperties.name = "+quote(name)+" order by otherproperties.property, otherproperties.date;"

		print,""	
		mysqlquery,SQLhandle,q,property,value,references,format='A,A,A,A'
		value = strtrim(value,2)
		print,"--Property--", "--Value--", "--Reference--",format='(A20,"          ",A9,"   ",A-30)'
		for i=0,n_elements(property)-1 do begin
			print,property[i] ,value[i],references[i],format='(A20,"          ",A9,"   ",A-30)'
		endfor
	endif

	if keyword_set(alternatenames) then begin
		q = "select alternatename from alternatenames where name="+quote(name)+";"
		print,q
		mysqlquery,SQLhandle,q,altnames,format='A'
		print, "-- Alternate Names --"
		forprint,altnames,/text
	endif



;--------------- Overplot SEDs for nearby stars? ----------------------

	if keyword_set(companions) then begin
		col = rainbow()
		ncol = n_elements(col)
		q = "select distinct secondary from companions where name="+quote(name)+";"
		mysqlquery,SQLhandle,q,companions,format='A',ngood=count
		if count gt 0 then for i=0L,count-1 do begin
				print,companions[i]
			mdb_sed,companions[i],/over,color=col(i mod ncol),print=print,ps=ps

		endfor


	endif

	if keyword_set(obs) then mdb_listobs,name

end
