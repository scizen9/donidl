pro gcmd,sam,plterr=plterr,ps=ps,limleg=limleg,pltype=pltype, $
	sntype=sntype,pltstr=pltstr,pltclr=pltclr,only=only, $
	outlier=outlier,outrange=outrange,title=title,apmags=apmags, $
	welltyped=welltyped
;
; GALEX and 2MASS SN host CMD
;
; get sn data
COMMON lowz_sne_info
;
; check keywords
if n_params(0) lt 1 then begin
	print,'gcmd,sam,pltype=pltype,sntype=sntype,title=title,/ps,/plterr,/pltstr,/pltclr,/only,outlier=sntyp,outrange=outrange,limleg=limleg,/welltyped'
	print,'pltype = 1 - Host FUV - K versus Host M(K)'
	print,'       = 2 - Host NUV - K versus Host M(K)'
	print,'       = 3 - Host FUV - NUV versus Host M(K)'
	print,'       = 4 - Host FUV - NUV versus Host NUV - K'
	print,'       = 5 - Site FUV - K versus Host M(K)'
	print,'       = 6 - Site NUV - K versus Host M(K)'
	print,'       = 7 - Site FUV - NUV versus Host M(K)'
	print,'       = 8 - Site FUV - NUV versus Host NUV - K'
	return
endif
;
; get host data
fl=!NGA_DATA+'hostdb.dat'
readcol,fl,host,fuv,fuve,nuv,nuve,bm,bme,km,kme,ty,tye,ebmv,mu,dmpc, $
	form='a,f,f,f,f,f,f,f,f,f,f,f,f,f',/silent
mkm=km-mu-(ebmv*0.367)
ells=where(ty lt -0.5, nells)
dsks=where(ty ge -0.5 and ty le 9.5, ndsks)
irrs=where(ty gt 9.5 and ty le 12.0, nirrs)
unks=where(ty gt 12.0, nunks)
;
; check keywords
if keyword_set(title) then $
	tlab=title $
else	tlab=''
if keyword_set(outlier) then begin
	if keyword_set(outrange) then $
		outr = outrange $
	else	outr = [-999.,-999.]
	outl = outlier
endif else begin
	outl = -1
	outr = [-999.,-999.]
endelse
;
; types to plot
if keyword_set(sntype) then begin
	tyran = [ $
		[0,0], $	; I
		[1,1], $	; Ia
		[2,2], $	; Ib
		[3,3], $	; Ib/c
		[4,4], $	; Ic
		[5,5], $	; IIn
		[6,6], $	; IIb
		[7,7], $	; IIL
		[8,8], $	; IIP
		[9,9] $		; II
		]
	tylabs = [ $
		'I', $
		'Ia', $
		'Ib', $
		'Ib_c', $
		'Ic', $
		'IIn', $
		'IIb', $
		'IIL', $
		'IIP', $
		'II' $
		]
;
	iI = 0
	iIa = 1
	iIb = 2
	iIbc = 3
	iIc = 4
	iIIn = 5
	iIIb = 6
	iIIL = 7
	iIIP = 8
	iII = 9
	clrs=['C','C','X','X','X','B','B','B','B','B']
	psm=[8,8,4,5,6,4,5,6,7,5]
	if sntype lt 0 or sntype gt 9 then $
		sntp = 1 $
	else	sntp = sntype
	psf = 'g'+tylabs(sntp)
	tylabs[3] = 'Ib/c'
endif else begin
	tyran = [ $
		[5,9], $        ; II (all)
		[2,4], $        ; Ib, Ib/c, Ic
		[1,1]]          ; Ia
	tylabs = [ $
		'II', $
		'Ibc', $
		'Ia']
;
; did we request well-typed SNe?
	if keyword_set(welltyped) then begin
		tyran(1,0)=8
		tylabs(0)='IIwt'
	endif
;
	iII = 0
	iIbc = 1
	iIa = 2
	clrs=['B','X','C']
	psm=[6,5,8]
	sntp = -1
	psf = 'gcmd'
endelse
;
; check stretch and color
if keyword_set(pltstr) then $
	psf=psf+'str' $
else if keyword_set(pltclr) then $
	psf=psf+'clr'
if keyword_set(only) then $
	psf=psf+'only'
;
; check pltype
if keyword_set(pltype) then $
	plt = pltype $
else	plt = 1
;
; get sample
if plt gt 4 then begin
	psf=psf+'site'
endif else begin
	psf=psf+'int'
endelse
ng=n_elements(sam)
snsam=sndat(sam)
print,'Total Sample: ',ng,' SNe'
;
; values common to all plots
xrng = [-15, -27.95]
if keyword_set(pltstr) then begin
	s0=0.6
	sinc=0.3
	slab='Str'
	strt = snsam.smpl_str
	soff=0.
endif else begin
	s0=0.3
	sinc=0.3
	slab='B-V+0.06'
	strt = snsam.smpl_clr + 0.6	; offset to avoid negative values
	soff=-0.6
endelse
sn = strtrim(snsam.id,2)
snh = snsam.host
sntn = snsam.tyn
snt = snsam.type
;
; set values particular to plot type
case plt of
	1: begin
		xlb = 'Host M_K Mag'
		ylb = 'Host FUV - K'
		yrng = [-1.99,13.5]
		lx0 = [-100,100]
		ly0 = [8.8,8.8]
		lx1 = 0
		ly1 = 0
		plx = snsam.hK_int_mag - snsam.hmu
		plxe = snsam.hK_int_magerr
		if keyword_set(apmags) then begin
			ply = snsam.fuv_ap_mag - snsam.K_int_mag
			plye =sqrt(snsam.fuv_ap_magerr^2+snsam.K_int_magerr^2)
		endif else begin
			ply = snsam.hfuv_int_mag - snsam.hK_int_mag
			plye=sqrt(snsam.hfuv_int_magerr^2+snsam.hK_int_magerr^2)
		endelse
		nlx0 = mkm(ells)
		nlx1 = mkm(dsks)
		nlx2 = mkm(irrs)
		nlx3 = mkm(unks)
		nly0 = fuv(ells) - km(ells)
		nly1 = fuv(dsks) - km(dsks)
		nly2 = fuv(irrs) - km(irrs)
		nly3 = fuv(unks) - km(unks)
		psf  = psf + 'MKvsFUVmK'
		legpos='top'
		xo=-0.2
		sx0 = -16.0
		sx1 = -16.5
		sy0=-1.
		syinc=-1.0
	end
	2: begin
		xlb = 'Host M_K Mag'
		ylb = 'Host NUV - K'
		yrng = [-1.99,12.5]
		lx0 = 0
		ly0 = 0
		lx1 = 0
		ly1 = 0
		plx = snsam.hK_int_mag - snsam.hmu
		plxe = snsam.hK_int_magerr
		ply = snsam.hnuv_int_mag - snsam.hK_int_mag
		plye = sqrt(snsam.hnuv_int_magerr^2 + snsam.hK_int_magerr^2)
		nlx0 = mkm(ells)
		nlx1 = mkm(dsks)
		nlx2 = mkm(irrs)
		nlx3 = mkm(unks)
		nly0 = nuv(ells) - km(ells)
		nly1 = nuv(dsks) - km(dsks)
		nly2 = nuv(irrs) - km(irrs)
		nly3 = nuv(unks) - km(unks)
		psf  = psf + 'MKvsNUVmK'
		legpos='top'
		xo=-0.2
		sx0 = -16.0
		sx1 = -16.5
		sy0=-1.
		syinc=-1.0
	end
	3: begin
		xlb = 'Host M_K Mag'
		ylb = 'Host FUV - NUV'
		yrng = [-0.5,3.5]
		lx0 = [-100,100]
		ly0 = [0.90,0.90]
		lx1 = 0
		ly1 = 0
		plx = snsam.hK_int_mag - snsam.hmu
		plxe = snsam.hK_int_magerr
		ply = snsam.hfuv_int_mag - snsam.hnuv_int_mag
		plye = sqrt(snsam.hfuv_int_magerr^2 + snsam.hnuv_int_magerr^2)
		nlx0 = mkm(ells)
		nlx1 = mkm(dsks)
		nlx2 = mkm(irrs)
		nlx3 = mkm(unks)
		nly0 = fuv(ells) - nuv(ells)
		nly1 = fuv(dsks) - nuv(dsks)
		nly2 = fuv(irrs) - nuv(irrs)
		nly3 = fuv(unks) - nuv(unks)
		psf  = psf + 'MKvsFUVmNUV'
		legpos='top'
		xo=-0.2
		sx0 = -16.0
		sx1 = -16.5
		sy0=1.1
		syinc=-0.25
	end
	4: begin
		xlb = 'Host NUV - K'
		ylb = 'Host FUV - NUV'
		yrng = [3.5,-0.5]
		xrng = [-1.99,11.0]
		lx0 = [-100,100]
		ly0 = [0.90,0.90]
		lx1 = [5.0,9.5]
		ly1 = [1.7,0.4]
		plx = snsam.hnuv_int_mag - snsam.hK_int_mag
		plxe = sqrt(snsam.hnuv_int_magerr^2 + snsam.hK_int_magerr^2)
		ply = snsam.hfuv_int_mag - snsam.hnuv_int_mag
		plye = sqrt(snsam.hfuv_int_magerr^2 + snsam.hnuv_int_magerr^2)
		nlx0 = nuv(ells) - km(ells)
		nlx1 = nuv(dsks) - km(dsks)
		nlx2 = nuv(irrs) - km(irrs)
		nlx3 = nuv(unks) - km(unks)
		nly0 = fuv(ells) - nuv(ells)
		nly1 = fuv(dsks) - nuv(dsks)
		nly2 = fuv(irrs) - nuv(irrs)
		nly3 = fuv(unks) - nuv(unks)
		psf  = psf + 'NUVmKvsFUVmNUV'
		legpos='bottom'
		xo=0.2
		sx0 = -1.2
		sx1 = -0.7
		sy0=2.7
		syinc=0.255
	end
	5: begin
		xlb = 'Host M_K Mag'
		ylb = 'Site FUV - K'
		yrng = [-1.99,13.5]
		lx0 = [-100,100]
		ly0 = [8.8,8.8]
		lx1 = 0
		ly1 = 0
		plx = snsam.hK_int_mag - snsam.hmu
		plxe = snsam.hK_int_magerr
		ply = snsam.fuv_1kpc_mag - snsam.K_1kpc_mag
		plye = sqrt(snsam.fuv_1kpc_magerr^2 + snsam.K_1kpc_magerr^2)
		nlx0 = mkm(ells)
		nlx1 = mkm(dsks)
		nlx2 = mkm(irrs)
		nlx3 = mkm(unks)
		nly0 = fuv(ells) - km(ells)
		nly1 = fuv(dsks) - km(dsks)
		nly2 = fuv(irrs) - km(irrs)
		nly3 = fuv(unks) - km(unks)
		psf  = psf + 'MKvsFUVmK'
		legpos='top'
		xo=-0.2
		sx0 = -16.0
		sx1 = -16.5
		sy0=-1.
		syinc=-1.0
	end
	6: begin
		xlb = 'Host M_K Mag'
		ylb = 'Site NUV - K'
		yrng = [-1.99,12.5]
		lx0 = 0
		ly0 = 0
		lx1 = 0
		ly1 = 0
		plx = snsam.hK_int_mag - snsam.hmu
		plxe = snsam.hK_int_magerr
		ply = snsam.nuv_1kpc_mag - snsam.K_1kpc_mag
		plye = sqrt(snsam.nuv_1kpc_magerr^2 + snsam.K_1kpc_magerr^2)
		nlx0 = mkm(ells)
		nlx1 = mkm(dsks)
		nlx2 = mkm(irrs)
		nlx3 = mkm(unks)
		nly0 = nuv(ells) - km(ells)
		nly1 = nuv(dsks) - km(dsks)
		nly2 = nuv(irrs) - km(irrs)
		nly3 = nuv(unks) - km(unks)
		psf  = psf + 'MKvsNUVmK'
		legpos='top'
		xo=-0.2
		sx0 = -16.0
		sx1 = -16.5
		sy0=-1.
		syinc=-1.0
	end
	7: begin
		xlb = 'Host M_K Mag'
		ylb = 'Site FUV - NUV'
		yrng = [-0.5,3.5]
		lx0 = [-100,100]
		ly0 = [0.90,0.90]
		lx1 = 0
		ly1 = 0
		plx = snsam.hK_int_mag - snsam.hmu
		plxe = snsam.hK_int_magerr
		ply = snsam.fuv_1kpc_mag - snsam.nuv_1kpc_mag
		plye = sqrt(snsam.fuv_1kpc_magerr^2+snsam.nuv_1kpc_magerr^2)
		nlx0 = mkm(ells)
		nlx1 = mkm(dsks)
		nlx2 = mkm(irrs)
		nlx3 = mkm(unks)
		nly0 = fuv(ells) - nuv(ells)
		nly1 = fuv(dsks) - nuv(dsks)
		nly2 = fuv(irrs) - nuv(irrs)
		nly3 = fuv(unks) - nuv(unks)
		psf  = psf + 'MKvsFUVmNUV'
		legpos='top'
		xo=-0.2
		sx0 = -16.0
		sx1 = -16.5
		sy0=1.1
		syinc=-0.25
	end
	8: begin
		xlb = 'Site NUV - K'
		ylb = 'Site FUV - NUV'
		yrng = [3.5,-0.5]
		xrng = [-1.99,11.0]
		lx0 = [-100,100]
		ly0 = [0.90,0.90]
		lx1 = [5.0,9.5]
		ly1 = [1.7,0.4]
		plx = snsam.nuv_1kpc_mag - snsam.K_1kpc_mag
		plxe = sqrt(snsam.nuv_1kpc_magerr^2 + snsam.K_1kpc_magerr^2)
		ply = snsam.fuv_1kpc_mag - snsam.nuv_1kpc_mag
		plye = sqrt(snsam.fuv_1kpc_magerr^2+snsam.nuv_1kpc_magerr^2)
		nlx0 = nuv(ells) - km(ells)
		nlx1 = nuv(dsks) - km(dsks)
		nlx2 = nuv(irrs) - km(irrs)
		nlx3 = nuv(unks) - km(unks)
		nly0 = fuv(ells) - nuv(ells)
		nly1 = fuv(dsks) - nuv(dsks)
		nly2 = fuv(irrs) - nuv(irrs)
		nly3 = fuv(unks) - nuv(unks)
		psf  = psf + 'NUVmKvsFUVmNUV'
		legpos='bottom'
		xo=0.2
		sx0 = -1.2
		sx1 = -0.7
		sy0=2.7
		syinc=0.255
	end
	else: begin
		print,'GCMD - ERROR: pltype 1 - 8'
		return
	end
endcase
;
; set up plot
font_store=!p.font
if keyword_set(ps) then begin
	psfile,psf
	print,'Plotting to: '+psf+'.ps'
	!p.font=1
	th=4
	si=2.2
	li=1.75
	ss=1.5
	hs=0.75
endif else begin
	print,'Plot: '+psf
	th=2
	si=2.7
	li=2.0
	ss=2.0
	hs=1.0
endelse
xlb = textoidl(xlb)
ylb = textoidl(ylb)
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
a=[findgen(40)*(!pi*2/40.),0.]
usersym,cos(a),sin(a),thick=th,/fill
nm=n_elements(clrs)
ci=lonarr(nm)
for i=0,nm-1 do ci(i) = colordex(clrs(i))
q=''
;
; plot em up
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=xlb, xran=xrng,xsty=1, $
	ytitle=ylb,yran=yrng,ysty=1, $
	title=tlab
;
; NGA galaxies
if n_elements(lx0) gt 1 then $
	oplot,lx0,ly0,color=colordex('orange'),thick=th
if n_elements(lx1) gt 1 then $
	oplot,lx1,ly1,color=colordex('orange'),thick=th,linesty=2
if nells gt 0 then $
	oplot,nlx0,nly0,psym=8,thick=th,color=colordex('red'),symsi=hs
if ndsks gt 0 then $
	oplot,nlx1,nly1,psym=8,thick=th,color=colordex('green'),symsi=hs
if nirrs gt 0 then $
	oplot,nlx2,nly2,psym=8,thick=th,color=colordex('blue'),symsi=hs
if nunks gt 0 then $
	oplot,nlx3,nly3,psym=8,thick=th,color=colordex('black'),symsi=hs
usersym,cos(a),sin(a),thick=th
;
; are we only plotting Ia's?
if keyword_set(pltstr) or keyword_set(pltclr) then begin
	i0=iIa
	i1=iIa
endif else if sntp ge 0 then begin
	i0=sntp
	i1=sntp
endif else begin
	i0=0
	i1=nm-1
endelse
leg = strarr(nm)
;
; loop over sn types
for i=i0,i1 do begin
;
; get good data
	t=where(sntn ge tyran(0,i) and sntn le tyran(1,i), nt)
	if nt gt 0 then begin
;
; get ratio
		if n_elements(ly0) gt 1 then begin
			w=where(ply(t) gt ly0(0), nw)
			fmt='(a-4,a,f6.2,a,i7)'
			print,tylabs(i),'above',ly0(0),': ',nw,form=fmt
			print,tylabs(i),'below',ly0(0),': ',nt-nw,form=fmt
		endif
;
; get outliers
		if i eq outl then begin
			o=where(ply(t) ge outr(0) and ply(t) le outr(1), nout)
			if nout gt 0 then for j=0,nout-1 do begin
				print,sn(t(o(j))),plx(t(o(j))),ply(t(o(j)))
				xyouts,plx(t(o(j)))+xo,ply(t(o(j))), $
				    strmid(sn(t(o(j))),2),charsi=li,charth=th
			endfor
		endif
;
; number of hosts
		hlist=snh(t)
		hlist=hlist(sort(hlist))
		hlist=hlist(uniq(hlist))
		nh=n_elements(hlist)
;
; plot sn Ia hosts
		if keyword_set(pltstr) or keyword_set(pltclr) then begin
;
; plot stretch/color if we have it
			nh = 0
			for j=0,nt-1 do $
				if strt(t(j)) gt -8. then begin
					plots,plx(t(j)),ply(t(j)),psym=8, $
						symsi=strt(t(j))*2.0
					nh = nh + 1
				endif else if not keyword_set(only) then begin
					plots,plx(t(j)),ply(t(j)),psym=1, $
						thick=th,color=ci(i),symsi=ss
					nh = nh + 1
				endif
;
; plot stretch/color legend
			for j=0,3 do begin
				sts=s0+j*sinc
				sy=sy0-j*syinc
				plots,sx0,sy-(0.15*syinc),psym=8,symsi=sts*2.0
				xyouts,sx1,sy,string(sts+soff,form='(f4.1)'), $
					charsi=1.2,charthi=th
			endfor
			xyouts,sx0,sy0-j*syinc,slab,charsi=1.2,charthi=th
;
; plot all sn type hosts
		endif else begin
			oplot,plx(t),ply(t),psym=psm(i),thick=th,color=ci(i), $
				symsi=ss
			if keyword_set(plterr) then begin
				!p.psym=3
				oploterror,plx(t),ply(t),plxe(t),plye(t), $
					errcolor=ci(i)
				!p.psym=0
			endif
		endelse
	endif else nh = 0
	leg(i) = string(tylabs(i),nh,format='(a-4,i4)')+' ('+strn(nt)+')'
endfor
;
; legend and psfile
if keyword_set(pltstr) or keyword_set(pltclr) then begin
	leg = leg(iIa)
	psm = [1]
	ci  = ci(iIa)
	ss  = [ss]
endif else if keyword_set(sntype) then begin
	leg = leg(sntp)
	psm = psm(sntp)
	ci = ci(sntp)
	ss = [ss]
endif else ss = replicate(ss,nm)
if strcmp(legpos,'top') then $
     legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li,symsi=ss $
else legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li,symsi=ss,$
	/bottom
;
; limits legend
if keyword_set(limleg) then $
     legend,textoidl(limleg),charthi=th,box=0,/right,/bottom,charsi=li
if keyword_set(ps) then psclose
;
!p.font=font_store
;
return
end
