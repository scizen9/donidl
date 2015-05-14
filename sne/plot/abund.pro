pro abund,czlimit=czlimit,exlimit=exlimit,inclimit=inclimit,plterr=plterr, $
	merrlim=merrlim,ps=ps,help=help,limleg=limleg,pltype=pltype, $
	sntype=sntype
;
; GALEX and 2MASS SN host CMD
;
; get sn data
COMMON lowz_sne_info
;
; check keywords
if keyword_set(help) then begin
	print,'abund,czlimit=czlimit,exlimit=exlimit,inclimit=inclimit,merrlim=merrlim,pltype=pltype,/ps,/plterr'
	print,'pltype = 1 - Host FUV - K versus O/H'
	print,'       = 2 - Host NUV - K versus O/H'
	print,'       = 3 - Host FUV - NUV versus O/H'
	print,'       = 4 - SN Ia Str versus O/H'
	print,'       = 5 - SN Ia Clr versus O/H'
	return
endif

if keyword_set(czlimit) then $
	czlim = czlimit $
else    czlim = 1.d9
if keyword_set(exlimit) then $
	exlim = exlimit $
else    exlim = 0.0
if keyword_set(inclimit) then $
	inclim = inclimit $
else    inclim = 99.9
if keyword_set(merrlim) then $
	melim = merrlim $
else    melim = 1.
;
; types to plot
if keyword_set(sntype) then begin
	tyran = [ $
		[0,0], $	; I
		[1,1], $	; Ia
		[2,2], $	; Ic
		[3,3], $	; Ib/c
		[4,4], $	; Ib
		[5,5], $	; IIn
		[6,6], $	; IIb
		[7,7], $	; IIL
		[8,8], $	; IIP
		[9,9] $		; II
		]
	tylabs = [ $
		'I', $
		'Ia', $
		'Ic', $
		'Ib_c', $
		'Ib', $
		'IIn', $
		'IIb', $
		'IIL', $
		'IIP', $
		'II' $
		]
;
	iI = 0
	iIa = 1
	iIc = 2
	iIbc = 3
	iIb = 4
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
	psf = 'oab'+tylabs(sntp)
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
	iII = 0
	iIbc = 1
	iIa = 2
	clrs=['B','X','C']
	psm=[6,5,8]
	sntp = -1
	psf = 'oab'
endelse
;
; set up plot
;
; limits legend
if czlim lt 1.e7 or exlim gt 0. then begin
	lem=strarr(4)
	if czlim lt 1.e7 then $
		lem(0) = textoidl('cz \leq '+ $
			strtrim(string(czlim,form='(f12.1)'),2)+' km/s')
	if exlim gt 0. then $
		lem(1) = textoidl('Exptime > '+ $
			 strtrim(string(exlim,form='(f6.1)'),2)+' s')
	if inclim le 90. then $
		lem(2) = textoidl('Host inc \leq '+ $
		    strtrim(string(fix(inclim+0.5),form='(i2)'),2)+'^{\circ}')
	lem(3) = textoidl('Mgerr \leq '+strtrim(string(melim,form='(f4.2)'),2))
endif
;
; check pltype
if keyword_set(pltype) then $
	plt = pltype $
else	plt = 1
;
; get sample
its  = ['hfuv_int_mag','hfuv_int_magerr', $
	'hnuv_int_mag','hnuv_int_magerr', $
	'hK_int_mag','hK_int_magerr', $
	'hO_abund','cz','fuv_exptime','hinc']
itr  = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	[0.,20.],[-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
g = snsample(its,itr,count=ng)
snsam=sndat(g)
print,'Total Sample: ',ng,' SNe'
;
; values common to all plots
xrng = [7.9,9.7]
sn = strtrim(snsam.id,2)
snh = snsam.host
sntn = snsam.tyn
snt = snsam.type
;
; set values particular to plot type
case plt of
	1: begin
		xlb = textoidl('12 + log(O/H)')
		ylb = textoidl('Host FUV - K')
		yrng = [-1.99,13.5]
		lx0 = 0
		ly0 = 0
		lx1 = 0
		ly1 = 0
		plx = snsam.hO_abund
		plxe = plx-plx + 0.01
		ply = snsam.hfuv_int_mag - snsam.hK_int_mag
		plye = sqrt(snsam.hfuv_int_magerr^2 + snsam.hK_int_magerr^2)
		out = where( (sntn ge tyran(0,iIbc) and sntn le tyran(1,iIbc) $
			      and ply gt 9.5) or $
			     (sntn ge tyran(0,iIa) and sntn le tyran(1,iIa) $
			     and plx lt -26.), nout)
		psf  = psf + 'OvsFUVmK'
		legpos='top'
		xo=-0.2
	end
	2: begin
		xlb = textoidl('12 + log(O/H)')
		ylb = textoidl('Host NUV - K')
		yrng = [-1.99,12.5]
		lx0 = 0
		ly0 = 0
		lx1 = 0
		ly1 = 0
		plx = snsam.hO_abund
		plxe = plx-plx + 0.01
		ply = snsam.hnuv_int_mag - snsam.hK_int_mag
		plye = sqrt(snsam.hnuv_int_magerr^2 + snsam.hK_int_magerr^2)
		out = where( (sntn ge tyran(0,iIbc) and sntn le tyran(1,iIbc) $
			      and ply gt 8.0) or $
			     (sntn ge tyran(0,iIa) and sntn le tyran(1,iIa) $
			     and plx lt -26.), nout)
		psf  = psf + 'OvsNUVmK'
		legpos='top'
		xo=-0.2
	end
	3: begin
		xlb = textoidl('12 + log(O/H)')
		ylb = textoidl('Host FUV - NUV')
		yrng = [-0.5,3.5]
		lx0 = 0
		ly0 = 0
		lx1 = 0
		ly1 = 0
		plx = snsam.hO_abund
		plxe = plx-plx + 0.01
		ply = snsam.hfuv_int_mag - snsam.hnuv_int_mag
		plye = sqrt(snsam.hfuv_int_magerr^2 + snsam.hnuv_int_magerr^2)
		out = where( (sntn ge tyran(0,iIbc) and sntn le tyran(1,iIbc) $
			      and ply gt 9.9) or $
			     (sntn ge tyran(0,iIa) and sntn le tyran(1,iIa) $
			     and plx lt -26.), nout)
		psf  = psf + 'OvsFUVmNUV'
		legpos='top'
		xo=-0.2
	end
	4: begin
		xlb = textoidl('12 + log(O/H)')
		ylb = textoidl('stretch')
		yrng = [0.5,1.4]
		lx0 = 0
		ly0 = 0
		lx1 = 0
		ly1 = 0
		plx = snsam.hO_abund
		plxe = plx-plx + 0.01
		ply = snsam.smpl_str
		plye = snsam.smpl_strerr
		out = where( (sntn ge tyran(0,iIbc) and sntn le tyran(1,iIbc) $
			      and ply gt 9.9) or $
			     (sntn ge tyran(0,iIa) and sntn le tyran(1,iIa) $
			     and ply gt 2.0), nout)
		psf  = psf + 'OvsStr'
		legpos='bottom'
		xo=0.2
	end
	5: begin
		xlb = textoidl('12 + log(O/H)')
		ylb = textoidl('(B-V)_{MAX} + 0.057')
		yrng = [-0.5,1.7]
		lx0 = 0
		ly0 = 0
		lx1 = 0
		ly1 = 0
		plx = snsam.hO_abund
		plxe = plx-plx + 0.01
		ply = snsam.smpl_clr
		plye = snsam.smpl_clrerr
		out = -1
		nout = 0
		psf  = psf + 'OvsClr'
		legpos='top'
		xo=-0.2
	end
	else: begin
		print,'ABUND - ERROR: pltype 1 - 5'
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
	th=5
	si=2.7
	li=2.0
	ss=2.0
endif else begin
	print,'Plot: '+psf
	th=3
	si=2.5
	li=2.0
	ss=2.0
endelse
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
	ytitle=ylb,yran=yrng,ysty=1
usersym,cos(a),sin(a),thick=th
;
; are we only one type?
if sntp gt 0 then begin
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
; number of hosts
		hlist=snh(t)
		hlist=hlist(sort(hlist))
		hlist=hlist(uniq(hlist))
		nh=n_elements(hlist)
		if plt ge 4 then $
			xx=where(ply gt -8., nh)
;
; plot all sn type hosts
		oplot,plx(t),ply(t),psym=psm(i),thick=th,color=ci(i), $
			symsi=ss
		if keyword_set(plterr) then begin
			!p.psym=3
			oploterror,plx(t),ply(t),plxe(t),plye(t), $
				errcolor=ci(i)
			!p.psym=0
		endif
	endif else nh = 0
	leg(i) = string(tylabs(i),nh,format='(a-4,i4)')
endfor
;
; outliers
;print,'Found: ',nout,' outliers'
;if nout gt 0 then for j=0,nout-1 do $
;	if not keyword_set(pltstr) or not keyword_set(pltclr) or $
;	  (sntn(out(j)) ge tyran(0,iIa) and sntn(out(j)) le tyran(1,iIa)) then $
;	  begin
;		print,sn(out(j)),snt(out(j)),snh(out(j)),plx(out(j)), $
;			ply(out(j)), form='(a-8,2x,a-6,2x,a-15,2x,2f8.2)'
	;	xyouts,plx(out(j))+xo,ply(out(j)),strmid(sn(out(j)),2),charsi=li
;	endif
;
; legend and psfile
if keyword_set(sntype) then begin
	leg = leg(sntp)
	psm = psm(sntp)
	ci = ci(sntp)
endif
if keyword_set(ps) then begin
	if keyword_set(limleg) then $
		legend,lem,charthi=th,box=0,/right,/bottom,charsi=li
	if strcmp(legpos,'top') then $
	     legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li $
	else legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li, $
		/bottom
	psclose
endif else if strcmp(legpos,'top') then $
		legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th $
	else	legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/bottom
;
!p.font=font_store
;
return
end
