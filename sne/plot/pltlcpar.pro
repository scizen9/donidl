pro pltlcpar,czlimit=czlimit,exlimit=exlimit,inclimit=inclimit,merrlim=merrlim,$
	ps=ps,help=help
;
; SN Ia LC par plots
;
; get sn data
COMMON lowz_sne_info
;
if keyword_set(help) then begin
	print,'pltlcpar,czlimit=czlimit,exlimit=exlimit,inclimit=inclimit,merrlim=merrlim,/ps'
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
else    melim = 1.0
;
; types to plot
tyran = [1,1]          ; Ia
tylabs = 'Ia'
aplabs = ['!9m!3','500pc','Host']
;
; set up plot
font_store=!p.font
if keyword_set(ps) then begin
	psfile,'lcparUV'
	!p.font=1
endif
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=3
si=1.2
a=[findgen(40)*(!pi*2/40.),0.]
usersym,cos(a),sin(a),thick=th
clrs=['R','B','O']
psm=[4,5,6]
nm=n_elements(clrs)
ci=intarr(nm)
for i=0,nm-1 do ci(i) = colordex(clrs(i))
sxrng=[0.5,1.25]
cxrng=[-0.5,1.8]
dxrng=[2.0,-0.8]
axrng=[-0.5,2.0]
q=''
;
!p.multi=[0,2,2]
;
; SN stretch versus FUV - NUV
yrng=[-1.9,3.5]
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Light Curve Stretch'), xran=sxrng,xsty=1, $
	ytitle=textoidl('FUV - NUV'),yran=yrng,ysty=1
leg = strarr(nm)
;
; loop over apertures
;
i=0	; surface brightness
;
; get good data
its = ['fuv_sbrt','fuv_res_magerr', $
       'nuv_sbrt','nuv_res_magerr', $
       'smpl_str','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [0.,2.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 1 then begin
	col = sndat(g).fuv_sbrt - sndat(g).nuv_sbrt
	err = sqrt(sndat(g).fuv_res_magerr^2+sndat(g).nuv_res_magerr^2)
	str = sndat(g).smpl_str
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
i=1	; 500pc ap
;
; get good data
its = ['fuv_500pc_mag','fuv_500pc_magerr', $
       'nuv_500pc_mag','nuv_500pc_magerr', $
       'smpl_str','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [0.,2.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_500pc_mag - sndat(g).nuv_500pc_mag
	err = sqrt(sndat(g).fuv_500pc_magerr^2+sndat(g).nuv_500pc_magerr^2)
	str = sndat(g).smpl_str
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
i=2	; Host integrated
;
; get good data
its = ['hfuv_int_mag','hfuv_int_magerr', $
       'hnuv_int_mag','hnuv_int_magerr', $
       'smpl_str','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [0.,2.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).hfuv_int_mag - sndat(g).hnuv_int_mag
	err = sqrt(sndat(g).hfuv_int_magerr^2+sndat(g).hnuv_int_magerr^2)
	str = sndat(g).smpl_str
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/right,/bottom
if czlim lt 1.e7 or exlim gt 0. or inclim le 90. then begin
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
	lem(3) = textoidl('Mgerr \leq '+strtrim(string(melim,form='(f5.2)'),2))
	legend,lem,charthi=th,box=0
endif
;
; SN color versus FUV - NUV
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Light Curve Color: (B-V)_{Bmax} + 0.057'), $
	xran=cxrng,xsty=1,ytitle=textoidl('FUV - NUV'),yran=yrng,ysty=1
leg = strarr(nm)
;
; loop over apertures
;
i=0	; surface brightness
;
; get good data
its = ['fuv_sbrt','fuv_res_magerr', $
       'nuv_sbrt','nuv_res_magerr', $
       'smpl_clr','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [-5,5.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_sbrt - sndat(g).nuv_sbrt
	err = sqrt(sndat(g).fuv_res_magerr^2+sndat(g).nuv_res_magerr^2)
	str = sndat(g).smpl_clr
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
i=1	; 500pc ap
;
; get good data
its = ['fuv_500pc_mag','fuv_500pc_magerr', $
       'nuv_500pc_mag','nuv_500pc_magerr', $
       'smpl_clr','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [-5.,5.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_500pc_mag - sndat(g).nuv_500pc_mag
	err = sqrt(sndat(g).fuv_500pc_magerr^2+sndat(g).nuv_500pc_magerr^2)
	str = sndat(g).smpl_clr
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
i=2	; Host integrated
;
; get good data
its = ['hfuv_int_mag','hfuv_int_magerr', $
       'hnuv_int_mag','hnuv_int_magerr', $
       'smpl_clr','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [-5.,5.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).hfuv_int_mag - sndat(g).hnuv_int_mag
	err = sqrt(sndat(g).hfuv_int_magerr^2+sndat(g).hnuv_int_magerr^2)
	str = sndat(g).smpl_clr
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/right
;
;
; delta versus FUV - NUV
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Light Curve \Delta'), xran=dxrng,xsty=1, $
	ytitle=textoidl('FUV - NUV'),yran=yrng,ysty=1
leg = strarr(nm)
;
; loop over apertures
;
i=0	; surface brightness
;
; get good data
its = ['fuv_sbrt','fuv_res_magerr', $
       'nuv_sbrt','nuv_res_magerr', $
       'mlcs2k_delta','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [-5.,5.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_sbrt - sndat(g).nuv_sbrt
	err = sqrt(sndat(g).fuv_res_magerr^2+sndat(g).nuv_res_magerr^2)
	str = sndat(g).mlcs2k_delta
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
i=1	; 500pc ap
;
; get good data
its = ['fuv_500pc_mag','fuv_500pc_magerr', $
       'nuv_500pc_mag','nuv_500pc_magerr', $
       'mlcs2k_delta','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [-5.,5.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_500pc_mag - sndat(g).nuv_500pc_mag
	err = sqrt(sndat(g).fuv_500pc_magerr^2+sndat(g).nuv_500pc_magerr^2)
	str = sndat(g).mlcs2k_delta
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
i=2	; Host integrated
;
; get good data
its = ['hfuv_int_mag','hfuv_int_magerr', $
       'hnuv_int_mag','hnuv_int_magerr', $
       'mlcs2k_delta','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [-5.,5.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).hfuv_int_mag - sndat(g).hnuv_int_mag
	err = sqrt(sndat(g).hfuv_int_magerr^2+sndat(g).hnuv_int_magerr^2)
	str = sndat(g).mlcs2k_delta
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th
;
;
; A0V versus FUV - NUV
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Light Curve A_0(V)'), xran=axrng,xsty=1, $
	ytitle=textoidl('FUV - NUV'),yran=yrng,ysty=1
leg = strarr(nm)
;
; loop over apertures
;
i=0	; surface brightness
;
; get good data
its = ['fuv_sbrt','fuv_res_magerr', $
       'nuv_sbrt','nuv_res_magerr', $
       'mlcs2k_a0v','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [0.,2.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_sbrt - sndat(g).nuv_sbrt
	err = sqrt(sndat(g).fuv_res_magerr^2+sndat(g).nuv_res_magerr^2)
	str = sndat(g).mlcs2k_a0v
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
i=1	; 500pc ap
;
; get good data
its = ['fuv_500pc_mag','fuv_500pc_magerr', $
       'nuv_500pc_mag','nuv_500pc_magerr', $
       'mlcs2k_a0v','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [0.,2.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_500pc_mag - sndat(g).nuv_500pc_mag
	err = sqrt(sndat(g).fuv_500pc_magerr^2+sndat(g).nuv_500pc_magerr^2)
	str = sndat(g).mlcs2k_a0v
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
i=2	; Host integrated
;
; get good data
its = ['hfuv_int_mag','hfuv_int_magerr', $
       'hnuv_int_mag','hnuv_int_magerr', $
       'mlcs2k_a0v','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [0.,2.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).hfuv_int_mag - sndat(g).hnuv_int_mag
	err = sqrt(sndat(g).hfuv_int_magerr^2+sndat(g).hnuv_int_magerr^2)
	str = sndat(g).mlcs2k_a0v
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/right
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'lcparUVIR'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
;
; SN stretch versus FUV - K
yrng=[-2,12.5]
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Light Curve Stretch'), xran=sxrng,xsty=1, $
	ytitle=textoidl('FUV - K'),yran=yrng,ysty=1
leg = strarr(nm)
;
; loop over apertures
;
i=0	; surface brightness
;
; get good data
its = ['fuv_sbrt','fuv_res_magerr', $
       'K_sbrt','K_res_magerr', $
       'smpl_str','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [0.,2.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_sbrt - sndat(g).K_sbrt
	err = sqrt(sndat(g).fuv_res_magerr^2+sndat(g).K_res_magerr^2)
	str = sndat(g).smpl_str
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
i=1	; 500pc ap
;
; get good data
its = ['fuv_500pc_mag','fuv_500pc_magerr', $
       'K_500pc_mag','K_500pc_magerr', $
       'smpl_str','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [0.,2.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_500pc_mag - sndat(g).K_500pc_mag
	err = sqrt(sndat(g).fuv_500pc_magerr^2+sndat(g).K_500pc_magerr^2)
	str = sndat(g).smpl_str
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
i=2	; Host integrated
;
; get good data
its = ['hfuv_int_mag','hfuv_int_magerr', $
       'hK_int_mag','hK_int_magerr', $
       'smpl_str','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [0.,2.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).hfuv_int_mag - sndat(g).hK_int_mag
	err = sqrt(sndat(g).hfuv_int_magerr^2+sndat(g).hK_int_magerr^2)
	str = sndat(g).smpl_str
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
;	for j=0,ng-1 do begin
;		xyouts,str(j)+0.02,col(j),strmid(sn(j),2)
;	endfor
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/right,/bottom
if czlim lt 1.e7 or exlim gt 0. or inclim le 90. then begin
	legend,lem,charthi=th,box=0,/bottom
endif
;
;
; SN color versus FUV - K
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Light Curve Color: (B-V)_{Bmax} + 0.057'), $
	xran=cxrng,xsty=1,ytitle=textoidl('FUV - K'),yran=yrng,ysty=1
leg = strarr(nm)
;
; loop over apertures
;
i=0	; surface brightness
;
; get good data
its = ['fuv_sbrt','fuv_res_magerr', $
       'K_sbrt','K_res_magerr', $
       'smpl_clr','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [-5.,5.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_sbrt - sndat(g).K_sbrt
	err = sqrt(sndat(g).fuv_res_magerr^2+sndat(g).K_res_magerr^2)
	str = sndat(g).smpl_clr
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
i=1	; 500pc ap
;
; get good data
its = ['fuv_500pc_mag','fuv_500pc_magerr', $
       'K_500pc_mag','K_500pc_magerr', $
       'smpl_clr','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [-5.,5.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_500pc_mag - sndat(g).K_500pc_mag
	err = sqrt(sndat(g).fuv_500pc_magerr^2+sndat(g).K_500pc_magerr^2)
	str = sndat(g).smpl_clr
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
i=2	; Host integrated
;
; get good data
its = ['hfuv_int_mag','hfuv_int_magerr', $
       'hK_int_mag','hK_int_magerr', $
       'smpl_clr','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [-5.,5.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).hfuv_int_mag - sndat(g).hK_int_mag
	err = sqrt(sndat(g).hfuv_int_magerr^2+sndat(g).hK_int_magerr^2)
	str = sndat(g).smpl_clr
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/right,/bottom
;
;
; delta versus FUV - K
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Light Curve \Delta'), xran=dxrng,xsty=1, $
	ytitle=textoidl('FUV - K'),yran=yrng,ysty=1
leg = strarr(nm)
;
; loop over apertures
;
i=0	; surface brightness
;
; get good data
its = ['fuv_sbrt','fuv_res_magerr', $
       'K_sbrt','K_res_magerr', $
       'mlcs2k_delta','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [-5.,5.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_sbrt - sndat(g).K_sbrt
	err = sqrt(sndat(g).fuv_res_magerr^2+sndat(g).K_res_magerr^2)
	str = sndat(g).mlcs2k_delta
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
i=1	; 500pc ap
;
; get good data
its = ['fuv_500pc_mag','fuv_500pc_magerr', $
       'K_500pc_mag','K_500pc_magerr', $
       'mlcs2k_delta','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [-5.,5.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_500pc_mag - sndat(g).K_500pc_mag
	err = sqrt(sndat(g).fuv_500pc_magerr^2+sndat(g).K_500pc_magerr^2)
	str = sndat(g).mlcs2k_delta
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
i=2	; Host integrated
;
; get good data
its = ['hfuv_int_mag','hfuv_int_magerr', $
       'hK_int_mag','hK_int_magerr', $
       'mlcs2k_delta','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [-5.,5.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).hfuv_int_mag - sndat(g).hK_int_mag
	err = sqrt(sndat(g).hfuv_int_magerr^2+sndat(g).hK_int_magerr^2)
	str = sndat(g).mlcs2k_delta
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/right,/bottom
;
;
; A0V versus FUV - K
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Light Curve A_0(V)'), xran=axrng,xsty=1, $
	ytitle=textoidl('FUV - K'),yran=yrng,ysty=1
leg = strarr(nm)
;
; loop over apertures
;
i=0	; surface brightness
;
; get good data
its = ['fuv_sbrt','fuv_res_magerr', $
       'K_sbrt','K_res_magerr', $
       'mlcs2k_a0v','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [0.,2.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_sbrt - sndat(g).K_sbrt
	err = sqrt(sndat(g).fuv_res_magerr^2+sndat(g).K_res_magerr^2)
	str = sndat(g).mlcs2k_a0v
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
i=1	; 500pc ap
;
; get good data
its = ['fuv_500pc_mag','fuv_500pc_magerr', $
       'K_500pc_mag','K_500pc_magerr', $
       'mlcs2k_a0v','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [0.,2.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_500pc_mag - sndat(g).K_500pc_mag
	err = sqrt(sndat(g).fuv_500pc_magerr^2+sndat(g).K_500pc_magerr^2)
	str = sndat(g).mlcs2k_a0v
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
i=2	; Host integrated
;
; get good data
its = ['hfuv_int_mag','hfuv_int_magerr', $
       'hK_int_mag','hK_int_magerr', $
       'mlcs2k_a0v','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,40.],[0.,melim], $
       [0.,2.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).hfuv_int_mag - sndat(g).hK_int_mag
	err = sqrt(sndat(g).hfuv_int_magerr^2+sndat(g).hK_int_magerr^2)
	str = sndat(g).mlcs2k_a0v
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string(aplabs(i),ng,format='(a-6,i4)')
;
legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/right,/bottom
;
; check output
if keyword_set(ps) then begin
	psclose
	psfile,'lcparmuFUV'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
;
; SN stretch versus mu(FUV)
yrng=[31.5,19.25]
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Light Curve Stretch'), xran=sxrng,xsty=1, $
	ytitle=textoidl('\mu_{FUV}'),yran=yrng,ysty=1
leg = strarr(nm)
;
; loop over apertures
;
i=0	; surface brightness
;
; get good data
its = ['fuv_sbrt','fuv_res_magerr', $
       'smpl_str','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,2.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_sbrt
	err = sndat(g).fuv_res_magerr
	str = sndat(g).smpl_str
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string('#SNe',ng,format='(a-6,i4)')
;
legend,leg,thick=th,box=0,charthi=th,/right
if czlim lt 1.e7 or exlim gt 0. or inclim le 90. then begin
	legend,lem,charthi=th,box=0
endif
;
; SN color versus mu(FUV)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Light Curve Color: (B-V)_{Bmax} + 0.057'), $
	xran=cxrng,xsty=1,ytitle=textoidl('\mu_{FUV}'),yran=yrng,ysty=1
leg = strarr(nm)
;
; loop over apertures
;
i=0	; surface brightness
;
; get good data
its = ['fuv_sbrt','fuv_res_magerr', $
       'smpl_clr','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[-5,5.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_sbrt
	err = sndat(g).fuv_res_magerr
	str = sndat(g).smpl_clr
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string('#SNe',ng,format='(a-6,i4)')
;
legend,leg,thick=th,box=0,charthi=th,/right
;
;
; delta versus mu(FUV)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Light Curve \Delta'), xran=dxrng,xsty=1, $
	ytitle=textoidl('\mu_{FUV}'),yran=yrng,ysty=1
leg = strarr(nm)
;
; loop over apertures
;
i=0	; surface brightness
;
; get good data
its = ['fuv_sbrt','fuv_res_magerr', $
       'mlcs2k_delta','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[-5,5.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_sbrt
	err = sndat(g).fuv_res_magerr
	str = sndat(g).mlcs2k_delta
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string('#SNe',ng,format='(a-6,i4)')
;
legend,leg,thick=th,box=0,charthi=th,/right
;
; A0V versus mu(FUV)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Light Curve A_0(V)'), xran=axrng,xsty=1, $
	ytitle=textoidl('\mu_{FUV}'),yran=yrng,ysty=1
leg = strarr(nm)
;
; loop over apertures
;
i=0	; surface brightness
;
; get good data
its = ['fuv_sbrt','fuv_res_magerr', $
       'mlcs2k_a0v','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0,2.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).fuv_sbrt
	err = sndat(g).fuv_res_magerr
	str = sndat(g).mlcs2k_a0v
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string('#SNe',ng,format='(a-6,i4)')
;
legend,leg,thick=th,box=0,charthi=th,/right
;
;
if keyword_set(ps) then begin
	psclose
	psfile,'lcparmuNUV'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
endif else read,'next: ',q
;
;
; SN stretch versus mu(NUV)
yrng=[31.5,19.25]
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Light Curve Stretch'), xran=sxrng,xsty=1, $
	ytitle=textoidl('\mu_{NUV}'),yran=yrng,ysty=1
leg = strarr(nm)
;
; loop over apertures
;
i=0	; surface brightness
;
; get good data
its = ['nuv_sbrt','nuv_res_magerr', $
       'smpl_str','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0.,2.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).nuv_sbrt
	err = sndat(g).nuv_res_magerr
	str = sndat(g).smpl_str
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string('#SNe',ng,format='(a-6,i4)')
;
legend,leg,thick=th,box=0,charthi=th,/right
if czlim lt 1.e7 or exlim gt 0. or inclim le 90. then begin
	legend,lem,charthi=th,box=0
endif
;
; SN color versus mu(NUV)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Light Curve Color: (B-V)_{Bmax} + 0.057'), $
	xran=cxrng,xsty=1,ytitle=textoidl('\mu_{NUV}'),yran=yrng,ysty=1
leg = strarr(nm)
;
; loop over apertures
;
i=0	; surface brightness
;
; get good data
its = ['nuv_sbrt','nuv_res_magerr', $
       'smpl_clr','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[-5,5.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).nuv_sbrt
	err = sndat(g).nuv_res_magerr
	str = sndat(g).smpl_clr
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string('#SNe',ng,format='(a-6,i4)')
;
legend,leg,thick=th,box=0,charthi=th,/right
;
;
; delta versus mu(NUV)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Light Curve \Delta'), xran=dxrng,xsty=1, $
	ytitle=textoidl('\mu_{NUV}'),yran=yrng,ysty=1
leg = strarr(nm)
;
; loop over apertures
;
i=0	; surface brightness
;
; get good data
its = ['nuv_sbrt','nuv_res_magerr', $
       'mlcs2k_delta','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[-5,5.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).nuv_sbrt
	err = sndat(g).nuv_res_magerr
	str = sndat(g).mlcs2k_delta
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string('#SNe',ng,format='(a-6,i4)')
;
legend,leg,thick=th,box=0,charthi=th,/right
;
; A0V versus mu(NUV)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Light Curve A_0(V)'), xran=axrng,xsty=1, $
	ytitle=textoidl('\mu_{NUV}'),yran=yrng,ysty=1
leg = strarr(nm)
;
; loop over apertures
;
i=0	; surface brightness
;
; get good data
its = ['nuv_sbrt','nuv_res_magerr', $
       'mlcs2k_a0v','hfuv_exptime','hnuv_exptime','tyn','hinc','cz']
itr = [[0.,40.],[0.,melim],[0,2.],[exlim,1.e9],[exlim,1.e9], $
       [tyran(0),tyran(1)],[-999.,inclim],[-999.,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	col = sndat(g).nuv_sbrt
	err = sndat(g).nuv_res_magerr
	str = sndat(g).mlcs2k_a0v
	sn = sndat(g).id
	oplot,str,col,psym=psm(i),thick=th,color=ci(i)
	errplot,str,col-err,col+err,thick=th,color=ci(i)
endif else ng = 0
leg(i) = string('#SNe',ng,format='(a-6,i4)')
;
legend,leg,thick=th,box=0,charthi=th,/right
;
;
if keyword_set(ps) then psclose
;
!p.multi=0
!p.font=font_store
;
return
end
