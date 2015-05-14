pro irx_beta_plot,sam,title=title,limleg=limleg,ps=ps,welltyped=welltyped, $
	pltstr=pltstr,pltclr=pltclr,only=only,sntype=sntype,plterr=plterr, $
	outlier=outlier,outrange=outrange
;
; calculate IRX using formulae from Dale et al. (2001)
common sndb_info
common galdb_info
;
if n_params(0) lt 1 then begin
	print,'irx_beta_plot, sample, title=tlab, limleg=lem, /ps'
	return
endif
;
; check keywords
if keyword_set(title) then $
	tlab=title $
else	tlab=''
if keyword_set(outlier) then begin
	if keyword_set(outrange) then begin
	    if n_elements(outrange) eq 4 then begin
		    outx = outrange(0:1)
		    outr = outrange(2:3)
	    endif else begin
		outr = outrange
		outx = [-999.,-999.]
	    endelse
	endif else begin
		outr = [-999.,-999.]
		outx = [-999.,-999.]
	endelse
	outl = outlier
endif else begin
	outl = -1
	outr = [-999.,-999.]
	outx = [-999.,-999.]
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
	psf = 'irxb'+tylabs(sntp)
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
	psf = 'irxb'
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
; get sample
snsam=sndat(sam)
ng=n_elements(snsam)
print,'Total Sample: ',ng,' SNe'
;
; values common to all plots
xo=0.05
sx0 = 2.4
sx1 = 2.5
sy0=0.
syinc=-0.25
if keyword_set(pltstr) then begin
	s0=0.6
	sinc=0.3
	slab='Str'
	strt = snsam.smpl_str
	soff=0.
endif else begin
	s0=0.6
	sinc=0.4
	slab='(B-V)_{MAX}'
	strt = snsam.smpl_clr + 0.6	; offset to avoid negative values
	soff=-0.6
endelse
sn = strtrim(snsam.id,2)
snh = snsam.host
sntn = snsam.tyn
snt = snsam.type
;
; get IRX, beta
hsam = snsam.hind
irx_beta,hsam,irx,irxe,bet,bete
lirx = alog10(irx)
lirxp= alog10(irx+irxe)
lirxm= alog10(irx-irxe)
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
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
a=[findgen(40)*(!pi*2/40.),0.]
usersym,cos(a),sin(a),thick=th;,/fill
nm=n_elements(clrs)
ci=lonarr(nm)
for i=0,nm-1 do ci(i) = colordex(clrs(i))
q=''
;
; plot 'em up
plx = bet
plxe= bete
ply = lirx
plye= ply-ply
plot,[0,0],[1,1],psym=4,charsi=si,charthi=th,thick=th, /nodata, $
	xsty=1,xthick=th,xrange=[-.3,2.7],xtitle=textoidl('FUV - NUV (\beta)'),$
	ysty=1,ythick=th,yrange=[-1.5,2.7], $
	ytitle=textoidl('log TIR / FUV  (IRX)')
;
;
oplot,[0.9,0.9],[-1000,1000],color=colordex('orange'),linesty=2
xx = [-1.,2.]
yy = -0.18 + 2.05 * xx
oplot,xx,yy
yy = -0.15 + 2.00 * xx
oplot,xx,yy,linesty=2
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
; get outliers
		if i eq outl then begin
		    if outx(0) ne outx(1) then begin
			o=where(ply(t) ge outr(0) and ply(t) le outr(1) and $
				plx(t) ge outx(0) and plx(t) le outx(1), nout)
		    endif else begin
			o=where(ply(t) ge outr(0) and ply(t) le outr(1), nout)
		    endelse
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
				xyouts,sx1,sy,string(sts,form='(f4.1)'), $
					charsi=1.2,charthi=th
			endfor
			xyouts,sx0,sy0-j*syinc,textoidl(slab), $
				charsi=1.2,charthi=th
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
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li,symsi=ss
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
