pro pltprofan,melim,ps=ps
;
; Plot UV,IR profile analysis results
;
; get sn data
COMMON sndb_info
;
if n_params(0) lt 1 then melim=1.0
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
	psfile,'profanmuFUV'
	!p.font=1
	th=5
	si=2.7
	li=2.0
endif else begin
	!p.multi=[0,2,2]
	th=3
	si=1.2
	li=1.0
endelse
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
clrs=['R','G','O','C']
nm=n_elements(clrs)
ci=lonarr(nm)
for i=0,nm-1 do ci(i) = colordex(clrs(i))
q=''
lslim=1000.
;
; Del FUV
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th, $
	charsi=si,charthi=th, $
	xtitle=textoidl('\Delta\mu_{FUV} (SN Site - Host Profile)'), $
	xran=[5,-5],xsty=1,ytitle='N Sites',yran=[-5,115],ysty=1
oplot,[0,0],[-100,1000]
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_pdelerr','tyn','hinc','linear_scale']
	itr = [[0.,melim],[tyran(0,i),tyran(1,i)],[0.,70.],[0.,lslim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).fuv_pdel
		h = histogram(del,min=-6.,max=7.,bins=0.5,loc=xh)
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
		faint=where(sndat(g).fuv_pdel ge 0., nf)
		bright=where(sndat(g).fuv_pdel lt 0., nb)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(nf,2)+':'+strtrim(nb,2)
endfor
;
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,charsi=li
lem = [textoidl('Merr \leq '+strtrim(string(melim,form='(f5.2)'),2)),'']
legend,lem,charthi=th,box=0,/right,charsi=li
if keyword_set(ps) then begin
	psclose
	psfile,'profanmuNUV'
endif
;
; Del NUV
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th, $
	charsi=si,charthi=th, $
	xtitle=textoidl('\Delta\mu_{NUV} (SN Site - Host Profile)'), $
	xran=[5,-5],xsty=1,ytitle='N Sites',yran=[-5,145],ysty=1
oplot,[0,0],[-100,1000]
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['nuv_pdelerr','tyn','hinc','linear_scale']
	itr = [[0.,melim],[tyran(0,i),tyran(1,i)],[0.,70.],[0.,lslim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).nuv_pdel
		h = histogram(del,min=-6.,max=7.,bins=0.5,loc=xh)
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
		faint=where(sndat(g).nuv_pdel ge 0., nf)
		bright=where(sndat(g).nuv_pdel lt 0., nb)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(nf,2)+':'+strtrim(nb,2)
endfor
;
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,charsi=li
legend,lem,charthi=th,box=0,/right,charsi=li
if keyword_set(ps) then begin
	psclose
	psfile,'profanFUVfrac'
endif
;
; Frac FUV histogram
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Fraction of Host FUV Light Interior to SN'), $
	xran=[-0.1,1.1],xsty=1,ytitle='N Sites',yran=[-1,70],ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_frac','tyn','hinc','linear_scale']
	itr = [[0.,1.0],[tyran(0,i),tyran(1,i)],[0.,70.],[0.,lslim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		frc = sndat(g).fuv_frac
		h = histogram(frc,min=-1.,max=1.2,bins=0.05,loc=xh)
		oplot,xh+(i*0.002),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
;
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,charsi=li,/righ
if keyword_set(ps) then begin
	psclose
	psfile,'profanNUVfrac'
endif
;
; Frac NUV histogram
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Fraction of Host NUV Light Interior to SN'), $
	xran=[-0.1,1.1],xsty=1,ytitle='N Sites',yran=[-1,70],ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['nuv_frac','tyn','hinc','linear_scale']
	itr = [[0.,1.0],[tyran(0,i),tyran(1,i)],[0.,70.],[0.,lslim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		frc = sndat(g).nuv_frac
		h = histogram(frc,min=-1.,max=1.2,bins=0.05,loc=xh)
		oplot,xh+(i*0.002),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
;
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,charsi=li,/righ
if keyword_set(ps) then begin
	psclose
	psfile,'profanmuJ'
endif else read,'next: ',q
;
; Del J
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th, $
	charsi=si,charthi=th, $
	xtitle=textoidl('\Delta\mu_J (SN Site - Host Profile)'), $
	xran=[5,-5],xsty=1,ytitle='N Sites',yran=[-0.5,23],ysty=1
oplot,[0,0],[-100,100]
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['J_pdelerr','tyn','hinc','linear_scale']
	itr = [[0.,melim],[tyran(0,i),tyran(1,i)],[0.,70.],[0.,lslim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).J_pdel
		h = histogram(del,min=-6.,max=7.,bins=0.5,loc=xh)
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
		faint=where(sndat(g).J_pdel ge 0., nf)
		bright=where(sndat(g).J_pdel lt 0., nb)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(nf,2)+':'+strtrim(nb,2)
endfor
;
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,charsi=li
legend,lem,charthi=th,box=0,/right,charsi=li
if keyword_set(ps) then begin
	psclose
	psfile,'profanmuK'
endif
;
; Del K
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th, $
	charsi=si,charthi=th, $
	xtitle=textoidl('\Delta\mu_K (SN Site - Host Profile)'), $
	xran=[5,-5],xsty=1,ytitle='N Sites',yran=[-0.5,28],ysty=1
oplot,[0,0],[-100,100]
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['K_pdelerr','tyn','hinc','linear_scale']
	itr = [[0.,melim],[tyran(0,i),tyran(1,i)],[0.,70.],[0.,lslim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		del = sndat(g).K_pdel
		h = histogram(del,min=-6.,max=7.,bins=0.5,loc=xh)
		oplot,xh+(i*0.02),h,psym=10,thick=th,color=ci(i)
		faint=where(sndat(g).K_pdel ge 0., nf)
		bright=where(sndat(g).K_pdel lt 0., nb)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(nf,2)+':'+strtrim(nb,2)
endfor
;
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,charsi=li
legend,lem,charthi=th,box=0,/right,charsi=li
if keyword_set(ps) then begin
	psclose
	psfile,'profanJfrac'
endif
;
; Frac J histogram
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Fraction of Host J-band Light Interior to SN'), $
	xran=[-0.1,1.1],xsty=1,ytitle='N Sites',yran=[-1,20],ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['J_frac','tyn','hinc','linear_scale']
	itr = [[0.,1.0],[tyran(0,i),tyran(1,i)],[0.,70.],[0.,lslim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		frc = sndat(g).J_frac
		h = histogram(frc,min=-1.,max=1.2,bins=0.1,loc=xh)
		oplot,xh+(i*0.002),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
;
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,charsi=li
if keyword_set(ps) then begin
	psclose
	psfile,'profanKfrac'
endif
;
; Frac K histogram
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Fraction of Host K-band Light Interior to SN'), $
	xran=[-0.1,1.1],xsty=1,ytitle='N Sites',yran=[-1,20],ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['K_frac','tyn','hinc','linear_scale']
	itr = [[0.,1.0],[tyran(0,i),tyran(1,i)],[0.,70.],[0.,lslim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		frc = sndat(g).K_frac
		h = histogram(frc,min=-1.,max=1.2,bins=0.1,loc=xh)
		oplot,xh+(i*0.002),h,psym=10,thick=th,color=ci(i)
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)
endfor
;
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,charsi=li
if keyword_set(ps) then begin
	psclose
	psfile,'profanFUVcum'
endif else read,'next: ',q
;
; Frac FUV cumulative
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Fraction of Host FUV Light Interior to SN'), $
	xran=[-0.1,1.1],xsty=1,ytitle='Fraction of SNe',yran=[-0.1,1.1],ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['fuv_frac','tyn','hinc','linear_scale']
	itr = [[0.,1.0],[tyran(0,i),tyran(1,i)],[0.,70.],[0.,lslim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		frc = sndat(g).fuv_frac
		frc = frc(sort(frc))
		frc = frc(where(frc lt 1.))
		nfrc = n_elements(frc)
		cum = frc-frc
		for j=0,nfrc-1 do cum(j) = float(j+1)/float(nfrc+1)
		lght = findgen(nfrc) / float(nfrc)
		kstwo,frc,lght,ksD,ksP
		ksP = fix(ksP * 100. + 0.5)
		oplot,frc,cum,psym=10,thick=th,color=ci(i)
;		oplot,frc,cum,psym=(-(i+4)),thick=th,color=ci(i)
		oplot,[-5,5],[-5,5],linesty=1,thick=th
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)+': '+string(ksP,form='(i3)') + $
		'%'
endfor
;
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,charsi=li;, $
;	psym=(-(indgen(4)+4))
if keyword_set(ps) then begin
	psclose
	psfile,'profanNUVcum'
endif
;
; Frac NUV cumulative
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Fraction of Host NUV Light Interior to SN'), $
	xran=[-0.1,1.1],xsty=1,ytitle='Fraction of SNe',yran=[-0.1,1.1],ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['nuv_frac','tyn','hinc','linear_scale']
	itr = [[0.,1.0],[tyran(0,i),tyran(1,i)],[0.,70.],[0.,lslim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		frc = sndat(g).nuv_frac
		frc = frc(sort(frc))
		frc = frc(where(frc lt 1.))
		nfrc = n_elements(frc)
		cum = frc-frc
		for j=0,nfrc-1 do cum(j) = float(j+1)/float(nfrc+1)
		lght = findgen(nfrc) / float(nfrc)
		kstwo,frc,lght,ksD,ksP
		ksP = fix(ksP * 100. + 0.5)
		oplot,frc,cum,psym=10,thick=th,color=ci(i)
;		oplot,frc,cum,psym=(-(i+4)),thick=th,color=ci(i)
		oplot,[-5,5],[-5,5],linesty=1,thick=th
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)+': '+string(ksP,form='(i3)') + $
		'%'
endfor
;
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,charsi=li;, $
;	psym=(-(indgen(4)+4))
if keyword_set(ps) then begin
	psclose
	psfile,'profanJcum'
endif
;
; Frac J cumulative
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Fraction of Host J-band Light Interior to SN'), $
	xran=[-0.1,1.1],xsty=1,ytitle='Fraction of SNe',yran=[-0.1,1.1],ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['J_frac','tyn','hinc','linear_scale']
	itr = [[0.,1.0],[tyran(0,i),tyran(1,i)],[0.,70.],[0.,lslim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		frc = sndat(g).J_frac
		frc = frc(sort(frc))
		frc = frc(where(frc lt 1.))
		nfrc = n_elements(frc)
		cum = frc-frc
		for j=0,nfrc-1 do cum(j) = float(j+1)/float(nfrc+1)
		lght = findgen(nfrc) / float(nfrc)
		kstwo,frc,lght,ksD,ksP
		ksP = fix(ksP * 100. + 0.5)
		oplot,frc,cum,psym=10,thick=th,color=ci(i)
;		oplot,frc,cum,psym=(-(i+4)),thick=th,color=ci(i)
		oplot,[-5,5],[-5,5],linesty=1,thick=th
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)+': '+string(ksP,form='(i3)') + $
		'%'
endfor
;
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,charsi=li;, $
;	psym=(-(indgen(4)+4))
if keyword_set(ps) then begin
	psclose
	psfile,'profanKcum'
endif
;
; Frac K cumulative
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Fraction of Host K-band Light Interior to SN'), $
	xran=[-0.1,1.1],xsty=1,ytitle='Fraction of SNe',yran=[-0.1,1.1],ysty=1
leg = strarr(nm)
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['K_frac','tyn','hinc','linear_scale']
	itr = [[0.,1.0],[tyran(0,i),tyran(1,i)],[0.,70.],[0.,lslim]]
	g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		frc = sndat(g).K_frac
		frc = frc(sort(frc))
		frc = frc(where(frc lt 1.))
		nfrc = n_elements(frc)
		cum = frc-frc
		for j=0,nfrc-1 do cum(j) = float(j+1)/float(nfrc+1)
		lght = findgen(nfrc) / float(nfrc)
		kstwo,frc,lght,ksD,ksP
		ksP = fix(ksP * 100. + 0.5)
		oplot,frc,cum,psym=10,thick=th,color=ci(i)
;		oplot,frc,cum,psym=(-(i+4)),thick=th,color=ci(i)
		oplot,[-5,5],[-5,5],linesty=1,thick=th
	endif
	leg(i) = tylabs(i) + '  '+strtrim(ng,2)+': '+string(ksP,form='(i3)') + $
		'%'
endfor
;
legend,leg,thick=th,box=0,color=ci,linesty=intarr(nm),charthi=th,charsi=li;, $
;	psym=(-(indgen(4)+4))
;
if keyword_set(ps) then psclose
!p.multi=0
!p.font=font_store
;
return
end
