pro pltcolmu,czlimit=czlimit,exlimit=exlimit,inclimit=inclimit, $
	merrlim=merrlim,ps=ps,help=help
;
; GALEX FUV - NUV, FUV - K versus surface brightness
;
; get sn data
COMMON sndb_info
;
; check keywords
if keyword_set(help) then begin
	print,'pltcolmu,czlimit=czlimit,exlimit=exlimit,inclimit=inclimit,merrlim=merrlim,/ps'
	return
endif
if keyword_set(czlimit) then $
	czlim = czlimit $
else	czlim = 1.d9
if keyword_set(exlimit) then $
	exlim = exlimit $
else	exlim = 0.0
if keyword_set(inclimit) then $
	inclim = inclimit $
else	inclim = 99.9
if keyword_set(merrlim) then $
	melim = merrlim $
else	melim = 1.0
;
; types to plot
tyran = [ $
	[2,4], $	; Ib, Ib/c, Ic
	[5,9], $	; II
	[4,4], $	; Ic
	[1,1]]		; Ia
tylabs = [ $
	'Ibc', $
	'II', $
	'Ic', $
	'Ia']
;
; set up plot
font_store=!p.font
if keyword_set(ps) then begin
	psfile,'colmuFUVres'
	!p.font=1
	th=5
endif else th=3
si=2.7
li=2.0
ss=2.0
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
a=[findgen(40)*(!pi*2/40.),0.]
usersym,cos(a),sin(a),thick=th
clrs=['R','G','O','C']
psm=[5,6,4,2]
nm=n_elements(clrs)
ci=lonarr(nm)
for i=0,nm-1 do ci[i] = colordex(clrs(i))
xrng=[32.5,18.25]
yrng=[-1.5,3.5]
apres=sndat[0].ap_res
q=''
;
; mu(FUV) versus FUV - NUV (resolution element aperture size)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site \mu_{FUV}'), title='5.6 arcsec ap', $
	xran=xrng,xsty=1,ytitle=textoidl('SN Site FUV - NUV'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_res_mag','fuv_res_magerr', $
	       'nuv_res_mag','nuv_res_magerr', 'fuv_sbrt', $
	       'tyn','hfuv_exptime','hnuv_exptime','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,40.], $
	       [tyran[0,i],tyran[1,i]],[exlim,1.e9],[exlim,1.e9],[-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat[g].fuv_res_mag - sndat[g].nuv_res_mag
		mg = sndat[g].fuv_sbrt
		oplot,mg,col,psym=psm[i],thick=th,color=ci[i],symsi=ss
	endif
	leg[i] = tylabs[i] + '  '+strtrim(ng,2)
endfor
;
lem=strarr(3)
lem[0] = textoidl('Mgerr \leq '+strtrim(string(melim,form='(f5.2)'),2))
if exlim gt 0. then $
	lem[1] = textoidl('Exptime \geq '+ $
		 strtrim(string(exlim,form='(f6.1)'),2)+' s')
if inclim le 90. then $
	lem[2] = textoidl('i_{HOST} \leq '+ $
		strtrim(string(inclim,form='(f5.0)'),2)+'\circ')
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/left,charsi=li
legend,lem,charthi=th,box=0,/right,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colmuFUVhost'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; mu(FUV) versus FUV - NUV, host integrated
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site \mu_{FUV}'), title='Host', $
	xran=xrng,xsty=1,ytitle=textoidl('SN Host FUV - NUV'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['hfuv_int_mag','hfuv_int_magerr', $
	       'hnuv_int_mag','hnuv_int_magerr', 'fuv_sbrt', $
	       'tyn','cz']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,40.], $
	       [tyran(0,i),tyran(1,i)],[-999.0,czlim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).hfuv_int_mag - sndat(g).hnuv_int_mag
		mg = sndat(g).fuv_sbrt
		oplot,mg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
;
leh=strarr(2)
leh(0) = textoidl('Mgerr \leq '+strtrim(string(melim,form='(f5.2)'),2))
if czlim lt 1.e7 then $
	leh(1) = textoidl('cz \leq '+ $
		 strtrim(string(czlim,form='(f12.1)'),2)+' km/s')
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/left,charsi=li
legend,leh,charthi=th,box=0,/right,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colmuFUV500pc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; mu(FUV) versus FUV - NUV (500 pc aperture size)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site \mu_{FUV}'), title='500 pc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('SN Site FUV - NUV'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_500pc_mag','fuv_500pc_magerr', $
	       'nuv_500pc_mag','nuv_500pc_magerr', 'fuv_sbrt', $
	       'tyn','hfuv_exptime','hnuv_exptime','ap_500pc','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,40.], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[exlim,1.e9],[apres,1.e9], $
	       [-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).fuv_500pc_mag - sndat(g).nuv_500pc_mag
		mg = sndat(g).fuv_sbrt
		oplot,mg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/left,charsi=li
legend,lem,charthi=th,box=0,/right,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colmuFUV1kpc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; mu(FUV) versus FUV - NUV (1 kpc aperture size)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site \mu_{FUV}'), title='1 kpc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('SN Site FUV - NUV'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_1kpc_mag','fuv_1kpc_magerr', $
	       'nuv_1kpc_mag','nuv_1kpc_magerr', 'fuv_sbrt', $
	       'tyn','hfuv_exptime','hnuv_exptime','ap_1kpc','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,40.], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[exlim,1.e9],[apres,1.e9], $
	       [-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).fuv_1kpc_mag - sndat(g).nuv_1kpc_mag
		mg = sndat(g).fuv_sbrt
		oplot,mg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/left,charsi=li
legend,lem,charthi=th,box=0,/right,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colmuFUV2kpc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; mu(FUV) versus FUV - NUV (2 kpc aperture size)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site \mu_{FUV}'), title='2 kpc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('SN Site FUV - NUV'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_2kpc_mag','fuv_2kpc_magerr', $
	       'nuv_2kpc_mag','nuv_2kpc_magerr', 'fuv_sbrt', $
	       'tyn','hfuv_exptime','hnuv_exptime','ap_2kpc','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,40.], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[exlim,1.e9],[apres,1.e9], $
	       [-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).fuv_2kpc_mag - sndat(g).nuv_2kpc_mag
		mg = sndat(g).fuv_sbrt
		oplot,mg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/left,charsi=li
legend,lem,charthi=th,box=0,/right,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colmuNUVres'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; NUV series
;
; mu(NUV) versus FUV - NUV (resolution element aperture size)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site \mu_{NUV}'), title='5.6 arcsec ap', $
	xran=xrng,xsty=1,ytitle=textoidl('SN Site FUV - NUV'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_res_mag','fuv_res_magerr', $
	       'nuv_res_mag','nuv_res_magerr', 'nuv_sbrt', $
	       'tyn','hfuv_exptime','hnuv_exptime','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,40.], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[exlim,1.e9],[-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).fuv_res_mag - sndat(g).nuv_res_mag
		mg = sndat(g).nuv_sbrt
		oplot,mg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/left,charsi=li
legend,lem,charthi=th,box=0,/right,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colmuNUVhost'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; mu(NUV) versus FUV - NUV host integrated
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site \mu_{NUV}'), title='Host', $
	xran=xrng,xsty=1,ytitle=textoidl('SN Host FUV - NUV'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['hfuv_int_mag','hfuv_int_magerr', $
	       'hnuv_int_mag','hnuv_int_magerr', 'nuv_sbrt', $
	       'tyn','cz']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,40.], $
	       [tyran(0,i),tyran(1,i)],[-999.0,czlim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).hfuv_int_mag - sndat(g).hnuv_int_mag
		mg = sndat(g).nuv_sbrt
		oplot,mg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/left,charsi=li
legend,leh,charthi=th,box=0,/right,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colmuNUV500pc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; mu(NUV) versus FUV - NUV (500 pc aperture size)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site \mu_{NUV}'), title='500 pc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('SN Site FUV - NUV'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_500pc_mag','fuv_500pc_magerr', $
	       'nuv_500pc_mag','nuv_500pc_magerr', 'nuv_sbrt', $
	       'tyn','hfuv_exptime','hnuv_exptime','ap_500pc','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,40.], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[exlim,1.e9],[apres,1.e9], $
	       [-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).fuv_500pc_mag - sndat(g).nuv_500pc_mag
		mg = sndat(g).nuv_sbrt
		oplot,mg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/left,charsi=li
legend,lem,charthi=th,box=0,/right,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colmuNUV1kpc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; mu(NUV) versus FUV - NUV (1 kpc aperture size)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site \mu_{NUV}'), title='1 kpc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('SN Site FUV - NUV'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_1kpc_mag','fuv_1kpc_magerr', $
	       'nuv_1kpc_mag','nuv_1kpc_magerr', 'nuv_sbrt', $
	       'tyn','hfuv_exptime','hnuv_exptime','ap_1kpc','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,40.], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[exlim,1.e9],[apres,1.e9], $
	       [-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).fuv_1kpc_mag - sndat(g).nuv_1kpc_mag
		mg = sndat(g).nuv_sbrt
		oplot,mg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/left,charsi=li
legend,lem,charthi=th,box=0,/right,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colmuNUV2kpc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; mu(NUV) versus FUV - NUV (2 kpc aperture size)
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site \mu_{NUV}'), title='2 kpc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('SN Site FUV - NUV'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_2kpc_mag','fuv_2kpc_magerr', $
	       'nuv_2kpc_mag','nuv_2kpc_magerr', 'nuv_sbrt', $
	       'tyn','hfuv_exptime','hnuv_exptime','ap_2kpc','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,40.], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[exlim,1.e9],[apres,1.e9], $
	       [-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).fuv_2kpc_mag - sndat(g).nuv_2kpc_mag
		mg = sndat(g).nuv_sbrt
		oplot,mg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/left,charsi=li
legend,lem,charthi=th,box=0,/right,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colmuFUVIRres'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
yrng=[-1,13.5]
;
; mu(FUV) versus FUV - K (resolution element aperture size)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site \mu_{FUV}'), title='5.6 arcsec ap', $
	xran=xrng,xsty=1,ytitle=textoidl('SN Site FUV - K'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_res_mag','fuv_res_magerr', $
	       'K_res_mag','K_res_magerr','fuv_sbrt', $
	       'tyn','hfuv_exptime','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,40.], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).fuv_res_mag - sndat(g).K_res_mag
		mg = sndat(g).fuv_sbrt
		oplot,mg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li, $
	/left,/bottom
legend,lem,charthi=th,box=0,/right,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colmuFUVIRhost'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; mu(FUV) versus FUV - K host integrated
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site \mu_{FUV}'), title='Host', $
	xran=xrng,xsty=1,ytitle=textoidl('SN Host FUV - K'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['hfuv_int_mag','hfuv_int_magerr', $
	       'hK_int_mag','hK_int_magerr','fuv_sbrt', $
	       'tyn','cz']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,40.], $
	       [tyran(0,i),tyran(1,i)],[-999.0,czlim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).hfuv_int_mag - sndat(g).hK_int_mag
		mg = sndat(g).fuv_sbrt
		oplot,mg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li, $
	/left,/bottom
legend,leh,charthi=th,box=0,/right,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colmuFUVIR500pc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; mu(FUV) versus FUV - K (500 pc aperture size)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site \mu_{FUV}'), title='500 pc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('SN Site FUV - K'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_500pc_mag','fuv_500pc_magerr', $
	       'K_500pc_mag','K_500pc_magerr','fuv_sbrt', $
	       'tyn','hfuv_exptime','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,40.], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).fuv_500pc_mag - sndat(g).K_500pc_mag
		mg = sndat(g).fuv_sbrt
		oplot,mg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li, $
		/left,/bottom
legend,lem,charthi=th,box=0,/right,charsi=li
;
; checkoutput
if keyword_set(ps) then begin
	psclose
	psfile,'colmuFUVIR1kpc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; mu(FUV) versus FUV - K (1 kpc aperture size)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site \mu_{FUV}'), title='1 kpc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('SN Site FUV - K'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_1kpc_mag','fuv_1kpc_magerr', $
	       'K_1kpc_mag','K_1kpc_magerr','fuv_sbrt', $
	       'tyn','hfuv_exptime','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,40.], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).fuv_1kpc_mag - sndat(g).K_1kpc_mag
		mg = sndat(g).fuv_sbrt
		oplot,mg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li, $
	/left,/bottom
legend,lem,charthi=th,box=0,/right,charsi=li
;
; check outputs
if keyword_set(ps) then begin
	psclose
	psfile,'colmuFUVIR2kpc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; mu(FUV) versus FUV - K (2 kpc aperture size)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site \mu_{FUV}'), title='2 kpc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('SN Site FUV - K'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_2kpc_mag','fuv_2kpc_magerr', $
	       'K_2kpc_mag','K_2kpc_magerr','fuv_sbrt', $
	       'tyn','hfuv_exptime','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,40.], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).fuv_2kpc_mag - sndat(g).K_2kpc_mag
		mg = sndat(g).fuv_sbrt
		oplot,mg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li, $
	/left,/bottom
legend,lem,charthi=th,box=0,/right,charsi=li
;
; check outputs
if keyword_set(ps) then $
	psclose
!p.multi=0
!p.font=font_store
;
return
end
