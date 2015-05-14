pro pltsncolhist,czlimit=czlimit,exlimit=exlimit,inclimit=inclimit, $
	merrlim=merrlim,ps=ps,help=help
;
; GALEX color histogram for different SNe types
;
; get sn data
COMMON lowz_sne_info
;
; check keywords
if keyword_set(help) then begin
	print,'Usage - pltnscolhist,czlimit=czlimit,exlimit=exlimit,inclimit=inclimit,merrlim=merrlim,/ps'
	return
endif
if keyword_set(czlimit) then $
	czlim = czlimit $
else	czlim = 1.e9
if keyword_set(exlimit) then $
	exlim = exlimit $
else	exlim = 0.
if keyword_set(inclimit) then $
	inclim = inclimit $
else	inclim = 99.9
if keyword_set(merrlim) then $
	melim = merrlim $
else	melim = 1.
;
; types to plot
tyran = [ $
	[2,4], $        ; Ib, Ib/c, Ic
	[5,9], $        ; II
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
	psfile,'colhistUVres'
	!p.font=1
	th=5
endif else th=3
si=2.7
li=2.0
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
clrs=['R','G','O','C']
nm=n_elements(clrs)
ci=intarr(nm)
for i=0,nm-1 do ci(i) = colordex(clrs(i))
xrng=[-2.1,3.5]
yrng=[-0.25,55]
apres=sndat(0).ap_res
q=''
;
; FUV - NUV, resolution element aperture size
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site FUV - NUV'), title='5.6 arcsec ap', $
	xran=xrng,xsty=1,ytitle=textoidl('% (100 \times N_{TYPE} / \Sigma N_{TYPE})'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_res_mag','fuv_res_magerr', $
	       'nuv_res_mag','nuv_res_magerr', $
	       'tyn','hfuv_exptime','hnuv_exptime','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[exlim,1.e9],[-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).fuv_res_mag - sndat(g).nuv_res_mag
		h = histogram(del,min=-5.,max=5.,bins=0.25,loc=xh)
		h=100.* (h/total(h))
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
;
lem=strarr(3)
lem(0) = textoidl('Mgerr \leq '+strtrim(string(melim,form='(f5.2)'),2))
if exlim gt 0. then $
	lem(1) = textoidl('Exptime \geq '+ $
		strtrim(string(exlim,form='(f6.1)'),2)+' s')
if inclim le 90. then $
	lem(2) = textoidl('i_{HOST} \leq '+ $
		strtrim(string(inclim,form='(f5.0)'),2)+'\circ')
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,/right,charsi=li
legend,lem,charthi=th,box=0,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colhistUVhost'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; FUV - NUV, host integrated
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Host FUV - NUV'), title='Host', $
	xran=xrng,xsty=1,ytitle=textoidl('% (100 \times N_{TYPE} / \Sigma N_{TYPE})'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['hfuv_int_mag','hfuv_int_magerr', $
	       'hnuv_int_mag','hnuv_int_magerr', $
	       'tyn','cz']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[-999.0,czlim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).hfuv_int_mag - sndat(g).hnuv_int_mag
		h = histogram(del,min=-5.,max=5.,bins=0.25,loc=xh)
		h=100.* (h/total(h))
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
;
leh=strarr(2)
leh(0) = textoidl('Mgerr \leq '+strtrim(string(melim,form='(f5.2)'),2))
if czlim lt 1.e7 then $
	leh(1) = textoidl('cz \leq '+ $
		strtrim(string(czlim,form='(f12.1)'),2)+' km/s')
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,/right,charsi=li
legend,leh,charthi=th,box=0,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colhistUV500pc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; FUV - NUV, 500 pc aperture size
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site FUV - NUV'), title='500 pc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('% (100 \times N_{TYPE} / \Sigma N_{TYPE})'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_500pc_mag','fuv_500pc_magerr', $
	       'nuv_500pc_mag','nuv_500pc_magerr', $
	       'tyn','hfuv_exptime','hnuv_exptime','ap_500pc','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[exlim,1.e9],[apres,1.e9], $
	       [-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).fuv_500pc_mag - sndat(g).nuv_500pc_mag
		h = histogram(del,min=-5.,max=5.,bins=0.25,loc=xh)
		h=100.* (h/total(h))
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,/right,charsi=li
legend,lem,charthi=th,box=0,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colhistUV1kpc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; FUV - NUV, 1 kpc aperture size
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site FUV - NUV'), title='1 kpc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('% (100 \times N_{TYPE} / \Sigma N_{TYPE})'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_1kpc_mag','fuv_1kpc_magerr', $
	       'nuv_1kpc_mag','nuv_1kpc_magerr', $
	       'tyn','hfuv_exptime','hnuv_exptime','ap_1kpc','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[exlim,1.e9],[apres,1.e9], $
	       [-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).fuv_1kpc_mag - sndat(g).nuv_1kpc_mag
		h = histogram(del,min=-5.,max=5.,bins=0.25,loc=xh)
		h=100.* (h/total(h))
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,/right,charsi=li
legend,lem,charthi=th,box=0,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colhistUV2kpc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; FUV - NUV, 2 kpc aperture size
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site FUV - NUV'), title='2 kpc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('% (100 \times N_{TYPE} / \Sigma N_{TYPE})'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_2kpc_mag','fuv_2kpc_magerr', $
	       'nuv_2kpc_mag','nuv_2kpc_magerr', $
	       'tyn','hfuv_exptime','hnuv_exptime','ap_2kpc','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[exlim,1.e9],[apres,1.e9], $
	       [-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).fuv_2kpc_mag - sndat(g).nuv_2kpc_mag
		h = histogram(del,min=-5.,max=5.,bins=0.25,loc=xh)
		h=100.* (h/total(h))
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,/right,charsi=li
legend,lem,charthi=th,box=0,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colhistIRres'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
yrng=[-0.25,65]
;
; J - K, resolution element aperture size
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site J - K'), title='5.6 arcsec ap', $
	xran=xrng,xsty=1,ytitle=textoidl('% (100 \times N_{TYPE} / \Sigma N_{TYPE})'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['J_res_mag','J_res_magerr', $
	       'K_res_mag','K_res_magerr','tyn','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[tyran(0,i),tyran(1,i)],$
		[-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).J_res_mag - sndat(g).K_res_mag
		h = histogram(del,min=-5.,max=5.,bins=0.25,loc=xh)
		h=100.* (h/total(h))
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,/right,charsi=li
legend,lem,charthi=th,box=0,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colhistIRhost'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; J - K, host integrated
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Host J - K'), title='Host', $
	xran=xrng,xsty=1,ytitle=textoidl('% (100 \times N_{TYPE} / \Sigma N_{TYPE})'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['hJ_int_mag','hJ_int_magerr', $
	       'hK_int_mag','hK_int_magerr','tyn','cz']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[-999.0,czlim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).hJ_int_mag - sndat(g).hK_int_mag
		h = histogram(del,min=-5.,max=5.,bins=0.25,loc=xh)
		h=100.* (h/total(h))
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,/right,charsi=li
legend,leh,charthi=th,box=0,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colhistIR500pc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; J - K, 500 pc aperture size
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site J - K'), title='500 pc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('% (100 \times N_{TYPE} / \Sigma N_{TYPE})'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['J_500pc_mag','J_500pc_magerr', $
	       'K_500pc_mag','K_500pc_magerr', $
	       'tyn','ap_500pc','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[apres,1.e9],[-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).J_500pc_mag - sndat(g).K_500pc_mag
		h = histogram(del,min=-5.,max=5.,bins=0.25,loc=xh)
		h=100.* (h/total(h))
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,/right,charsi=li
legend,lem,charthi=th,box=0,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colhistIR1kpc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; J - K, 1 kpc aperture size
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site J - K'), title='1 kpc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('% (100 \times N_{TYPE} / \Sigma N_{TYPE})'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['J_1kpc_mag','J_1kpc_magerr', $
	       'K_1kpc_mag','K_1kpc_magerr', $
	       'tyn','ap_1kpc','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[apres,1.e9],[-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).J_1kpc_mag - sndat(g).K_1kpc_mag
		h = histogram(del,min=-5.,max=5.,bins=0.25,loc=xh)
		h=100.* (h/total(h))
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,/right,charsi=li
legend,lem,charthi=th,box=0,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colhistIR2kpc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
;
; J - K, 2 kpc aperture size
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site J - K'), title='2 kpc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('% (100 \times N_{TYPE} / \Sigma N_{TYPE})'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['J_2kpc_mag','J_2kpc_magerr', $
	       'K_2kpc_mag','K_2kpc_magerr', $
	       'tyn','ap_2kpc','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[apres,1.e9],[-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).J_2kpc_mag - sndat(g).K_2kpc_mag
		h = histogram(del,min=-5.,max=5.,bins=0.25,loc=xh)
		h=100.* (h/total(h))
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,/right,charsi=li
legend,lem,charthi=th,box=0,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colhistUVIRres'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
xrng=[-2.5,13.5]
yrng=[-0.25,65]
;
; FUV - K, resolution element aperture size
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site FUV - K'), title='5.6 arcsec ap', $
	xran=xrng,xsty=1,ytitle=textoidl('% (100 \times N_{TYPE} / \Sigma N_{TYPE})'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_res_mag','fuv_res_magerr', $
	       'K_res_mag','K_res_magerr', $
	       'tyn','hfuv_exptime','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).fuv_res_mag - sndat(g).K_res_mag
		h = histogram(del,min=-5.,max=15.,bins=1.0,loc=xh)
		h=100.* (h/total(h))
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,/right,charsi=li
legend,lem,charthi=th,box=0,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colhistUVIRhost'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; FUV - K, host integrated
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Host FUV - K'), title='Host', $
	xran=xrng,xsty=1,ytitle=textoidl('% (100 \times N_{TYPE} / \Sigma N_{TYPE})'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['hfuv_int_mag','hfuv_int_magerr', $
	       'hK_int_mag','hK_int_magerr', $
	       'tyn','hfuv_exptime','cz']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[-999.,czlim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).hfuv_int_mag - sndat(g).hK_int_mag
		h = histogram(del,min=-5.,max=15.,bins=1.0,loc=xh)
		h=100.* (h/total(h))
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,/right,charsi=li
legend,leh,charthi=th,box=0,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colhistUVIR500pc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; FUV - K, 500 pc aperture size
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site FUV - K'), title='500 pc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('% (100 \times N_{TYPE} / \Sigma N_{TYPE})'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_500pc_mag','fuv_500pc_magerr', $
	       'K_500pc_mag','K_500pc_magerr', $
	       'tyn','hfuv_exptime','ap_500pc','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[apres,1.e9],[-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).fuv_500pc_mag - sndat(g).K_500pc_mag
		h = histogram(del,min=-5.,max=15.,bins=1.0,loc=xh)
		h=100.* (h/total(h))
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
;
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,/right,charsi=li
legend,lem,charthi=th,box=0,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colhistUVIR1kpc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; FUV - K, 1 kpc aperture size
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site FUV - K'), title='1 kpc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('% (100 \times N_{TYPE} / \Sigma N_{TYPE})'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_1kpc_mag','fuv_1kpc_magerr', $
	       'K_1kpc_mag','K_1kpc_magerr', $
	       'tyn','hfuv_exptime','ap_1kpc','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[apres,1.e9],[-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).fuv_1kpc_mag - sndat(g).K_1kpc_mag
		h = histogram(del,min=-5.,max=15.,bins=1.0,loc=xh)
		h=100.* (h/total(h))
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,/right,charsi=li
legend,lem,charthi=th,box=0,charsi=li
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'colhistUVIR2kpc'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
; FUV - K, 2 kpc aperture size
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site FUV - K'), title='2 kpc ap', $
	xran=xrng,xsty=1,ytitle=textoidl('% (100 \times N_{TYPE} / \Sigma N_{TYPE})'),$
	yran=yrng, ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_2kpc_mag','fuv_2kpc_magerr', $
	       'K_2kpc_mag','K_2kpc_magerr', $
	       'tyn','hfuv_exptime','ap_2kpc','hinc']
        itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[exlim,1.e9],[apres,1.e9],[-999.,inclim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).fuv_2kpc_mag - sndat(g).K_2kpc_mag
		h = histogram(del,min=-5.,max=15.,bins=1.0,loc=xh)
		h=100.* (h/total(h))
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,/right,charsi=li
legend,lem,charthi=th,box=0,charsi=li
;
; check output
if keyword_set(ps) then $
	psclose
;
!p.font=font_store
!p.multi=0
;
return
end
