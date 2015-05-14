; $Id: pltnovae.pro,v 1.1 2014/02/20 17:28:44 neill Exp $
;
pro pltnovae,mdir,ps=ps,output=output,flash=flash,ten=ten
;+
;	pltnovae - use nova models to generate light curve plots
;-
common filter_info, master_filter
if n_params(0) lt 1 then begin
	print,'Usage: pltnovae, <model_dir>, /ps, /output, /flash, /ten'
	return
endif
;
; get list of model table files
flist = file_search(mdir+'/tbl*',count=nf)
if nf le 0 then begin
	print,'No models found'
	return
endif
;
; check keywords
if keyword_set(flash) then begin
	if keyword_set(ten) then $
		flst = '_t10' $
	else	flst = '_flash'
endif	else	flst = ''
;
; Constants
;
; solar radius (cm)
sr = 6.96e10
;
; ten parsecs (cm)
d = 3.08567758d19
;
; m31 distance modulus
mu = 24.1
;
; waves
arfil = '/Users/neill/catvars/uvflash/swxpc0to12s6_20010101v011_arf.fits'
arf=mrdfits(arfil,1,ahdr)
xea = reverse(arf.specresp)
xwav = reverse( (12.41)/arf.energ_hi )
w=where(xwav gt 60.)
wl = [xwav[0:w[0]],(findgen(6940.)+61.)]
;
; loop over models
for j=0,nf-1 do begin
	;
	rute = flist[j]
	junk = gettok(rute,'/')
	;
	print,'Reading model ',j+1,'/',nf,rute,form='(a,i3,a,i3,2x,a)'
	readcols,flist[j],tim,ltef,lr,ll,skip=1,/silent
	;
	; output file
	if keyword_set(output) then begin
		openw,ol,'output/'+rute+'.txt',/get_lun
		printf,ol,'# MODEL: '+rute
		printf,ol,'# DATE: '+systime(0)
		printf,ol,'#  step       days        T(K)      R(cm)      M(XRT)   M(NUV)    M(V)'
	endif
;
	npts = n_elements(tim)
	xmg = fltarr(npts)
	nmg = fltarr(npts)
	vmg = fltarr(npts)
;
; temp max
	ltefmax = 0.
	for i=0,npts-1 do begin
		r = sr * 10.^lr[i]
		flam = planck(wl,10.^ltef[i])*r^2/d^2
		if ltef[i] gt ltefmax then begin
			sflam = flam
			ltefmax = ltef[i]
		endif
		swift_xrt_mag,flam,wl,xrtmab,/silent,arf=arf
		gx_spec_mag,flam,wl,fuvmab,nuvmab,/silent
		flux = filter_integ(3, wl, flam, 0.0)/master_filter[3].area_lambda
		vmag = flux_to_apparent_mag(flux,ifilter=3,/vegasys) + $
			master_filter[3].ab_offset
		xmg[i] = xrtmab + 24.44
		nmg[i] = nuvmab + 24.44
		vmg[i] = vmag + 24.44
		print,string(13B),i+1,'/',npts,tim[i],10.^ltef[i],r,xrtmab, $
			nuvmab,vmag, $
		   format='($,a1,i5,a,i5,i9,2x,f10.2,2x,g9.3,2x,g9.3,2x,3f9.2)'
	   	if keyword_set(output) then $
		    printf,ol,i,tim[i],10.^ltef[i],r,xrtmab,nuvmab,vmag, $
			format='(i6,2x,f10.2,2x,f10.1,2x,g9.3,2x,3f9.2)'
	endfor
	print,' '
	if keyword_set(output) then free_lun,ol
;
; plot
	if keyword_set(ps) then begin
		psfile,'output/'+rute+flst
	endif
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	!p.multi=[0,1,2]
	if keyword_set(ps) then !p.font=0
	th=3
	si=1.75
	;
	; ranges
	if keyword_set(flash) then begin
		del = 0.05*(max(nmg)-min(nmg))
		nmg0 = nmg[0]
		good = where(nmg lt nmg0-del)
		ttim = tim
		t0 = min(ttim[good])
		delt = (max(ttim[good]) - t0) > 10.
		epst = (delt * 0.1) > 1.
		if keyword_set(ten) then begin
			delt = 10.
			epst = 1.
		endif
		xrng = [t0-epst,t0+delt+epst]
		xxlog = 0
	endif else begin
		xrng = [min(t[where(t gt 0.)]),max(t)]
		xxlog = 1
	endelse
	yrntef = minmax(ltef)
	yrnr = minmax(lr)
	yrng = minmax([ltef,lr])
	yrng = [-4,8]
	plot,tim,nmg,psym=-5,thick=th,charsi=si,charthi=th,title=rute, $
		xtitle='',xthick=th,xrange=xrng,xsty=1,xlog=xxlog, $
		ytitle='MAG (AB, M31)',ythick=th,yrange=[34,16],ysty=1,/nodata
	oplot,tim,vmg,psym=-6,thick=th,color=colordex('B')
	oplot,tim,nmg,psym=-5,thick=th,color=colordex('P')
	oplot,tim,xmg,psym=-4,thick=th,color=colordex('C')
;
	legend,['XRT','NUV','VIS'],psym=[-4,-5,-6],charthi=th,charsi=si*0.5, $
		box=0,color=[colordex('C'), colordex('P'),colordex('B')], $
		thick=[th,th,th], pos=[xrng[0],10.6]
;
	plot,tim,ltef,psym=-4,thick=th,charsi=si,charthi=th, $
		xtitle='Time(days)',xthick=th,xrange=xrng,xsty=1,xlog=xxlog, $
		ytitle='Log',ythick=th,yrange=yrng,/nodata
	oplot,tim,ltef,psym=-4,thick=th,color=colordex('R')
	oplot,tim,lr,psym=-7,thick=th,color=colordex('G')
;
	legend,['Teff(K)','R/Rsun'],psym=[-4,-7],charthi=th,charsi=si*0.5, $
		box=0,color=[colordex('R'),colordex('G')],thick=[th,th], $
		pos=[xrng[0],!y.crange[1]+0.2*(!y.crange[1]-!y.crange[0])]
	q=''
	if keyword_set(ps) then $
		psclose $
	else	read,'Next <cr>: ',q
endfor
;
return
end
