pro pltsngcmd,czlimit=czlimit,exlimit=exlimit,inclimit=inclimit, $
	merrlim=merrlim,dojrk=dojrk,ps=ps,help=help
;
; GALEX and 2MASS SN host CMD
;
; get sn data
COMMON lowz_sne_info
;
; check keywords
if keyword_set(help) then begin
	print,'pltsngcmd,czlimit=czlimit,exlimit=exlimit,inclimit=inclimit,dojrk=dojrk,/ps'
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
tyran = [ $
	[2,4], $        ; Ib, Ib/c, Ic
	[5,9], $        ; II (all)
	[4,4], $        ; Ic
	[1,1]]          ; Ia
tylabs = [ $
	'Ibc', $
	'II', $
	'Ic', $
	'Ia']
;
iIbc = 0
iII = 1
iIIP = 2
iIa = 3
;
; set up plot
font_store=!p.font
if keyword_set(ps) then begin
	psfile,'cmdhostMKFUVmK'
	!p.font=1
	th=5
	si=2.7
	li=2.0
	ss=2.0
endif else begin
	!p.multi=[0,2,2]
	th=3
	si=1.2
	li=1.0
	ss=1.0
endelse
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
a=[findgen(40)*(!pi*2/40.),0.]
usersym,cos(a),sin(a),thick=th
clrs=['R','G','O','C']
psm=[5,6,4,2]
nm=n_elements(clrs)
ci=lonarr(nm)
for i=0,nm-1 do ci(i) = colordex(clrs(i))
q=''
;
if keyword_set(dojrk) then $
	jrk=iIa $
else	jrk=99
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
; Host M(K) versus Host FUV - K
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Host M_K Mag'), xran=[-15,-27.5],xsty=1, $
	ytitle=textoidl('Host FUV - K'),yran=[-1.99,13.5],ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['hfuv_int_mag','hfuv_int_magerr', $
	       'hK_int_mag','hK_int_magerr', $
	       'tyn','cz','hfuv_exptime','hinc']
	itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[tyran(0,i),tyran(1,i)],$
	       [-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).hfuv_int_mag - sndat(g).hK_int_mag
		mkmg = sndat(g).hK_int_mag - sndat(g).hmu
		del = sndat(g).mlcs2k_delta
		sn = sndat(g).id
		oplot,mkmg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
		hlist=sndat(g).host
		hlist=hlist(sort(hlist))
		hlist=hlist(uniq(hlist))
		nh=n_elements(hlist)
;
; plot MLCS2K data
		if i eq jrk then begin
			t=where(del gt -9., nj)
			for j=0,nj-1 do begin
				p = t(j)
				plots,mkmg(p),col(p),psym=4,symsi=1.+del(p)
			endfor
		endif
;
; outliers
		t=where(col lt -2. or col gt 11.5 or $
			mkmg gt -15. or mkmg lt -27.5, n)
		if n gt 0 then for j=0,n-1 do begin
			print,'SN          FUV-K    M(K)'
			print,sn(t(j)),col(t(j)),mkmg(t(j)),form='(a-8,2f9.2)'
		endfor
	endif else nh = 0
	leg(i) = string(tylabs(i),nh,format='(a-4,i4)')
endfor
;
; legend and psfile
if keyword_set(ps) then begin
	legend,lem,charthi=th,box=0,/right,/bottom,charsi=li
	legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li
	psclose
	psfile,'cmdhostMKNUVmK'
endif else legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th
;
; Host M(K) versus Host NUV - K
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Host M_K Mag'), xran=[-15,-27.5],xsty=1, $
	ytitle=textoidl('Host NUV - K'),yran=[-1.99,13.5],ysty=1
leg = strarr(nm)
;
; loop over files
for i=0,nm-1 do begin
;
; get good data
	its = ['hnuv_int_mag','hnuv_int_magerr', $
	       'hK_int_mag','hK_int_magerr', $
	       'tyn','cz','hnuv_exptime','hinc']
	itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[tyran(0,i),tyran(1,i)],$
	       [-999.0,czlim],[exlim,1.e9],[-999.,inclim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).hnuv_int_mag - sndat(g).hK_int_mag
		mkmg = sndat(g).hK_int_mag - sndat(g).hmu
		del = sndat(g).mlcs2k_delta
		oplot,mkmg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
		hlist=sndat(g).host
		hlist=hlist(sort(hlist))
		hlist=hlist(uniq(hlist))
		nh=n_elements(hlist)
;
; plot MLCS2K data
		if i eq jrk then begin
			t=where(del gt -9., nj)
			for j=0,nj-1 do begin
				p = t(j)
				plots,mkmg(p),col(p),psym=4,symsi=1.+del(p)
			endfor
		endif
	endif else nh = 0
	leg(i) = string(tylabs(i),nh,format='(a-4,i4)')
endfor
;
; legend and psfile
if keyword_set(ps) then begin
	legend,lem,charthi=th,box=0,/right,/bottom,charsi=li
	legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li
	psclose
	psfile,'cmdhostMKFUVmNUV'
endif else begin
	legend,lem,charthi=th,box=0,/right,/bottom
	legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th
endelse
;
; Host M(K) versus Host FUV - NUV
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Host M_K Mag'), xran=[-15,-27.5],xsty=1, $
	ytitle=textoidl('Host FUV - NUV'),yran=[-0.5,3.5],ysty=1
leg = strarr(nm)
;
; loop over files
for i=0,nm-1 do begin
;
; get good data
	its = ['hfuv_int_mag','hfuv_int_magerr', $
	       'hnuv_int_mag','hnuv_int_magerr', $
	       'hK_int_mag','hK_int_magerr', $
	       'tyn','cz','hfuv_exptime','hnuv_exptime','hinc']
	itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[-999.0,czlim], $
	       [exlim,1.e9],[exlim,1.e9],[-999.,inclim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).hfuv_int_mag - sndat(g).hnuv_int_mag
		mkmg = sndat(g).hK_int_mag - sndat(g).hmu
		del = sndat(g).mlcs2k_delta
		sn = sndat(g).id
		snh = sndat(g).host
		snt = sndat(g).type
		oplot,mkmg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
		hlist=sndat(g).host
		hlist=hlist(sort(hlist))
		hlist=hlist(uniq(hlist))
		nh=n_elements(hlist)
;
; plot MLCS2K data
		if i eq jrk then begin
			t=where(del gt -9., nj)
			for j=0,nj-1 do begin
				p = t(j)
				plots,mkmg(p),col(p),psym=4,symsi=1.+del(p)
			endfor
		endif
;
; identify outliers
		xo=-0.2
		xa=[0.9, 0.2,-0.2,-0.2,0.9,-0.2,-0.2,-0.2]
		ya=[0.0,-0.2, 0.0, 0.0,0.0,-0.2, 0.1, 0.1]
		if i eq iIbc then begin
			t=where(col gt 1.2 and col lt 3., n)
			if n gt 0 then for j=0,n-1 do begin
				print,sn(t(j)),snt(t(j)),snh(t(j)),mkmg(t(j)), $
					col(t(j)), $
					form='(a-8,2x,a-6,2x,a-15,2x,2f6.2)'
				xyouts,mkmg(t(j))+xo,col(t(j)), $
					strmid(sn(t(j)),2),charsi=li
			endfor
		endif
		if i eq iIa then begin
			t=where(col gt 1.3, n)
			if n gt 0 then for j=0,n-1 do begin
				print,sn(t(j)),snt(t(j)),snh(t(j)),mkmg(t(j)), $
					col(t(j)), $
					form='(a-8,2x,a-6,2x,a-15,2x,2f6.2)'
;				xyouts,mkmg(t(j))+xa(j),col(t(j))+ya(j), $
;					strmid(sn(t(j)),2)
			endfor
			t=where(mkmg gt -19.5, n)
			if n gt 0 then for j=0,n-1 do begin
				print,sn(t(j)),snt(t(j)),snh(t(j)),mkmg(t(j)), $
					col(t(j)), $
					form='(a-8,2x,a-6,2x,a-15,2x,2f6.2)'
				xyouts,mkmg(t(j))+xo,col(t(j)), $
					strmid(sn(t(j)),2),charsi=li
			endfor
		endif
	endif else nh = 0
	leg(i) = string(tylabs(i),nh,format='(a-4,i4)')
endfor
;
; legend and psfile
if keyword_set(ps) then begin
	legend,lem,charthi=th,box=0,/right,charsi=li
	legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li
	psclose
	psfile,'cmdhostNUVmKFUVmNUV'
endif else legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th
;
; Host NUV - K versus Host FUV - NUV
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Host NUV - K'), xran=[-1.99,13.5],xsty=1, $
	ytitle=textoidl('Host FUV - NUV'),yran=[3.5,-0.5],ysty=1
leg = strarr(nm)
;
; loop over files
for i=0,nm-1 do begin
;
; get good data
	its = ['hfuv_int_mag','hfuv_int_magerr', $
	       'hnuv_int_mag','hnuv_int_magerr', $
	       'hK_int_mag','hK_int_magerr', $
	       'tyn','cz','hfuv_exptime','hnuv_exptime','hinc']
	itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[-999.0,czlim], $
	       [exlim,1.e9],[exlim,1.e9],[-999.,inclim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		clx = sndat(g).hnuv_int_mag - sndat(g).hK_int_mag
		cly = sndat(g).hfuv_int_mag - sndat(g).hnuv_int_mag
		del = sndat(g).mlcs2k_delta
		oplot,clx,cly,psym=psm(i),thick=th,color=ci(i),symsi=ss
		hlist=sndat(g).host
		hlist=hlist(sort(hlist))
		hlist=hlist(uniq(hlist))
		nh=n_elements(hlist)
;
; plot MLCS2K data
		if i eq jrk then begin
			t=where(del gt -9., nj)
			for j=0,nj-1 do begin
				p = t(j)
				plots,clx(p),cly(p),psym=4,symsi=1.+del(p)
			endfor
		endif
	endif else nh = 0
	leg(i) = string(tylabs(i),nh,format='(a-4,i4)')
endfor
;
; legend and psfile
if keyword_set(ps) then begin
	legend,lem,charthi=th,box=0,charsi=li,/right,/bottom
	legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li,/bottom
	psclose
	psfile,'cmdsiteMKFUVmK'
endif else begin
	legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/bottom
	read,'next: ',q
endelse
;
; Host M(K) versus SN Site FUV - K
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Host M_K Mag'), xran=[-15,-27.5],xsty=1, $
	ytitle=textoidl('SN Site FUV - K'),yran=[-1.99,13.5],ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_res_mag','fuv_res_magerr', $
	       'K_res_mag','K_res_magerr', $
	       'hK_int_mag','hK_int_magerr', $
	       'tyn','cz','hfuv_exptime','hinc']
	itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[-999.0,czlim],[exlim,1.e9], $
	       [-999.,inclim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).fuv_res_mag - sndat(g).K_res_mag
		mkmg = sndat(g).hK_int_mag - sndat(g).hmu
		del = sndat(g).mlcs2k_delta
		sn = sndat(g).id
		oplot,mkmg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
;
; plot MLCS2K data
		if i eq jrk then begin
			t=where(del gt -9., nj)
			for j=0,nj-1 do begin
				p = t(j)
				plots,mkmg(p),col(p),psym=4,symsi=1.+del(p)
			endfor
		endif
;
; outliers
;		t=where(col lt -2. or col gt 11.5 or $
;			mkmg gt -15. or mkmg lt -27.5, n)
;		if n gt 0 then for j=0,n-1 do begin
;			print,'SN          FUV-K    M(K)'
;			print,sn(t(j)),col(t(j)),mkmg(t(j)),form='(a-8,2f9.2)'
;		endfor
	endif else ng = 0
	leg(i) = string(tylabs(i),ng,format='(a-4,i4)')
endfor
;
; legend and psfile
if keyword_set(ps) then begin
	legend,lem,charthi=th,box=0,charsi=li
	legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li,/bottom
	psclose
	psfile,'cmdsiteMKNUVmK'
endif else legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th
;
; M(K) versus SN Site NUV - K
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Host M_K Mag'), xran=[-15,-27.5],xsty=1, $
	ytitle=textoidl('SN Site  NUV - K'),yran=[-1.99,13.5],ysty=1
leg = strarr(nm)
;
; loop over files
for i=0,nm-1 do begin
;
; get good data
	its = ['nuv_res_mag','nuv_res_magerr', $
	       'K_res_mag','K_res_magerr', $
	       'hK_int_mag','hK_int_magerr', $
	       'tyn','cz','hnuv_exptime','hinc']
	itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[-999.0,czlim],[exlim,1.e9], $
	       [-999.,inclim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).nuv_res_mag - sndat(g).K_res_mag
		mkmg = sndat(g).hK_int_mag - sndat(g).hmu
		del = sndat(g).mlcs2k_delta
		oplot,mkmg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
;
; plot MLCS2K data
		if i eq jrk then begin
			t=where(del gt -9., nj)
			for j=0,nj-1 do begin
				p = t(j)
				plots,mkmg(p),col(p),psym=4,symsi=1.+del(p)
			endfor
		endif
	endif else ng = 0
	leg(i) = string(tylabs(i),ng,format='(a-4,i4)')
endfor
;
; legend and psfile
if keyword_set(ps) then begin
	legend,lem,charthi=th,box=0,/right,/bottom,charsi=li
	legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li
	psclose
	psfile,'cmdsiteMKFUVmNUV'
endif else begin
	legend,lem,charthi=th,box=0,/right,/bottom
	legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th
endelse
;
; Host M(K) versus SN Site FUV - NUV
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Host M_K Mag'), xran=[-15,-27.5],xsty=1, $
	ytitle=textoidl('SN Site FUV - NUV'),yran=[-0.5,3.5],ysty=1
leg = strarr(nm)
;
; loop over files
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_res_mag','fuv_res_magerr', $
	       'nuv_res_mag','nuv_res_magerr', $
	       'hK_int_mag','hK_int_magerr', $
	       'tyn','cz','hfuv_exptime','hnuv_exptime','hinc']
	itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[-999.0,czlim], $
	       [exlim,1.e9],[exlim,1.e9],[-999.,inclim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		col = sndat(g).fuv_res_mag - sndat(g).nuv_res_mag
		mkmg = sndat(g).hK_int_mag - sndat(g).hmu
		del = sndat(g).mlcs2k_delta
		sn = sndat(g).id
		snh = sndat(g).host
		snt = sndat(g).type
		oplot,mkmg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
;
; plot MLCS2K data
		if i eq jrk then begin
			t=where(del gt -9., nj)
			for j=0,nj-1 do begin
				p = t(j)
				plots,mkmg(p),col(p),psym=4,symsi=1.+del(p)
			endfor
		endif
;
; identify outliers
;		xo=-0.2
;		xa=[0.9, 0.2,-0.2,-0.2,0.9,-0.2,-0.2,-0.2]
;		ya=[0.0,-0.2, 0.0, 0.0,0.0,-0.2, 0.1, 0.1]
;		if i eq iIbc then begin
;			t=where(col gt 1.2 and col lt 3., n)
;			if n gt 0 then for j=0,n-1 do begin
;				print,sn(t(j)),snt(t(j)),snh(t(j)),mkmg(t(j)), $
;					col(t(j)), $
;					form='(a-8,2x,a-6,2x,a-15,2x,2f6.2)'
;				xyouts,mkmg(t(j))+xo,col(t(j)), $
;					strmid(sn(t(j)),2)
;			endfor
;		endif
;		if i eq iIa then begin
;			t=where(col gt 1.3, n)
;			if n gt 0 then for j=0,n-1 do begin
;				print,sn(t(j)),snt(t(j)),snh(t(j)),mkmg(t(j)), $
;					col(t(j)), $
;					form='(a-8,2x,a-6,2x,a-15,2x,2f6.2)'
;;				xyouts,mkmg(t(j))+xa(j),col(t(j))+ya(j), $
;;					strmid(sn(t(j)),2)
;			endfor
;			t=where(mkmg gt -19.5, n)
;			if n gt 0 then for j=0,n-1 do begin
;				print,sn(t(j)),snt(t(j)),snh(t(j)),mkmg(t(j)), $
;					col(t(j)), $
;					form='(a-8,2x,a-6,2x,a-15,2x,2f6.2)'
;				xyouts,mkmg(t(j))+xo,col(t(j)), $
;					strmid(sn(t(j)),2)
;			endfor
;		endif
	endif else ng = 0
	leg(i) = string(tylabs(i),ng,format='(a-4,i4)')
endfor
;
; legend and psfile
if keyword_set(ps) then begin
	legend,lem,charthi=th,box=0,/right,charsi=li
	legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li
	psclose
	psfile,'cmdsiteNUVmKFUVmNUV'
endif else legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th
;
; SN Site NUV - K versus SN Site FUV - NUV
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('SN Site NUV - K'), xran=[-1.99,13.5],xsty=1, $
	ytitle=textoidl('SN Site FUV - NUV'),yran=[3.5,-0.5],ysty=1
leg = strarr(nm)
;
; loop over files
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_res_mag','fuv_res_magerr', $
	       'nuv_res_mag','nuv_res_magerr', $
	       'K_res_mag','K_res_magerr', $
	       'tyn','cz','hfuv_exptime','hnuv_exptime','hinc']
	itr = [[0.,30.],[0.,melim],[0.,30.],[0.,melim],[0.,30.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[-999.0,czlim], $
	       [exlim,1.e9],[exlim,1.e9],[-999.,inclim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		clx = sndat(g).nuv_res_mag - sndat(g).K_res_mag
		cly = sndat(g).fuv_res_mag - sndat(g).nuv_res_mag
		del = sndat(g).mlcs2k_delta
		oplot,clx,cly,psym=psm(i),thick=th,color=ci(i),symsi=ss
;
; plot MLCS2K data
		if i eq jrk then begin
			t=where(del gt -9., nj)
			for j=0,nj-1 do begin
				p = t(j)
				plots,clx(p),cly(p),psym=4,symsi=1.+del(p)
			endfor
		endif
	endif else ng = 0
	leg(i) = string(tylabs(i),ng,format='(a-4,i4)')
endfor
;
; legend and psfile
if keyword_set(ps) then begin
	legend,lem,charthi=th,box=0,/right,/bottom,charsi=li
	legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li,/bottom
	psclose
endif else legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,/bottom
;
!p.multi=0
!p.font=font_store
;
return
end
