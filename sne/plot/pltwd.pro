pro pltwd,czlimit=czlimit,exlimit=exlimit,merrlim=merrlim,plterr=plterr, $
	pltstr=pltstr,pltclr=pltclr,only=only,ps=ps,help=help,limleg=limleg, $
	indices=indices
;
; GALEX SN host CMD
;
; get sn data
COMMON lowz_sne_info
;
; get wyder diagram data
fl=!GALEX_DATA+'wyder_cmd.dat'
readcol,fl,mr,nuvmr,form='f,f'
;
; check keywords
if keyword_set(help) then begin
	print,'pltwd,czlimit=czlimit,exlimit=exlimit,/pltstr,/pltclr,/only,/limleg,/ps'
	return
endif

if keyword_set(czlimit) then $
	czlim = czlimit $
else    czlim = 1.d9
if keyword_set(exlimit) then $
	exlim = exlimit $
else    exlim = 0.0
if keyword_set(merrlim) then $
	melim = merrlim $
else    melim = 0.25
;
; types to plot
tyran = [ $
	[2,4], $        ; Ib, Ib/c, Ic
	[5,9], $        ; II (all)
;	[3,3], $        ; Ib/c
	[1,1]]          ; Ia
tylabs = [ $
	'Ibc', $
	'II', $
;	'Ib/c', $
	'Ia']
;
iIbc = 0
iII = 1
;iIIP = 2
iIa = 2
;
; set up plot
font_store=!p.font
if keyword_set(ps) then begin
	psfile,'wdcmd'
	!p.font=1
	th=5
	si=2.7
	li=2.0
	ss=1.0
endif else begin
	th=3
	si=2.7
	li=2.0
	ss=2.0
endelse
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
a=[findgen(40)*(!pi*2/40.),0.]
usersym,cos(a),sin(a),thick=th
;clrs=['R','B','O','C']
clrs=['R','B','C']
;psm=[5,6,4,8]
psm=[5,6,8]
nm=n_elements(clrs)
ci=lonarr(nm)
for i=0,nm-1 do ci(i) = colordex(clrs(i))
q=''
;
if keyword_set(pltstr) or keyword_set(pltclr) then begin
	jrk=iIa
endif else begin
	jrk=99
endelse
if keyword_set(pltstr) then begin
	s0=0.6
	sinc=0.3
	slab='Str'
	soff=0.
endif else begin
	s0=0.3
	sinc=0.3
	slab='B-V+0.06'
	soff=-0.6
endelse
sx0 = -25.0
sx1 = -24.5
sy0=3.0
syinc=-0.5
;
; limits legend
lem=strarr(3)
lem(0) = textoidl('cz \leq '+ $
	strtrim(string(czlim,form='(f12.1)'),2)+' km/s')
lem(1) = textoidl('Exptime > '+ $
	 strtrim(string(exlim,form='(f6.1)'),2)+' s')
lem(2) = textoidl('Mgerr \leq '+strtrim(string(melim,form='(f4.2)'),2))
;
; Host M(r) versus Host NUV - r
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=textoidl('Host M_r Mag'), xran=[-25.75,-14.25],xsty=1, $
	ytitle=textoidl('Host NUV - r'),yran=[-0.10,7.99],ysty=1
leg = strarr(nm)
done=(1 eq 0)
;
; Wyder galaxies
;oplot,[-100,-18],[8.75,8.75],color=colordex('orange'),thick=th
oplot,mr,nuvmr,psym=3
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['hnuv_int_mag','hnuv_int_magerr', $
	       'hr_int_mag','hr_int_magerr', $
	       'hr_abs_mag','hr_abs_magerr', $
	       'tyn','cz','hnuv_exptime']
	itr  = [[0.,30.],[0.,melim], $
		[0.,30.],[0.,melim], $
		[-30,0.],[0.,melim], $
	       [tyran(0,i),tyran(1,i)],[-999.0,czlim],[exlim,1.e9]]
	if keyword_set(indices) then begin
		snsam = sndat[indices]
		g = where(snsam.tyn ge tyran[0,i] and $
			  snsam.tyn le tyran[1,i],ng)
		snsam = snsam[g]
	endif else begin
		g = snsample(its,itr,count=ng)
		snsam = sndat[g]
	endelse
	if ng gt 0 then begin
		col = snsam.hnuv_int_mag - snsam.hr_int_mag
		cole = sqrt( (snsam.hnuv_int_magerr > 0.01)^2 + $
			     (snsam.hr_int_magerr > 0.01)^2)
		if keyword_set(pltstr) then $
			strt = snsam.smpl_str $
		else	strt = snsam.smpl_clr + 0.6
		sn = snsam.id
		typ = snsam.type
		hsts = snsam.host
		czs = snsam.cz
		mus = muz(czs/!phys_c)
		mrmg = snsam.hr_int_mag - mus
		mrmge= snsam.hr_int_magerr
		hlist=snsam.host
		hlist=hlist(sort(hlist))
		hlist=hlist(uniq(hlist))
		nh=n_elements(hlist)
;
; outliers
		out=where( (col gt 3.5 and col lt 8. and i ne iIa) or $
			   (mrmg gt -18.0 and mrmg lt -15. and $
			    col gt -0.1 and col lt 8.), nout)
		if nout gt 0 then begin
			if not done then begin
		print,'# SN    Type      NUV-r     M(r)  Host               cz'
			done = (1 eq 1)
			endif
			for j=0,nout-1 do begin
	print,sn(out(j)),typ(out(j)),col(out(j)),mrmg(out(j)),hsts(out(j)), $
			czs(out(j)),format='(a-8,a-6,2f9.2,2x,a-15,f9.1)'
				xyouts,mrmg(out(j))+0.11,col(out(j))+0.08, $
					strmid(sn(out(j)),2),charthi=th, $
					charsi=si/1.8
			endfor
		endif
;
; plot stretch
		if i eq jrk then begin
			nh = 0
			for j=0,ng-1 do begin
			    if strt(j) gt -8. then begin
			    	plots,mrmg(j),col(j),psym=8,symsi=strt(j)*2.0
				nh = nh + 1
			    endif else if not keyword_set(only) then begin
			    	plots,mrmg(j),col(j),psym=1,symsi=ss, $
			    		thick=th,color=ci(i)
				nh = nh + 1
			    endif
			endfor
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
		endif else begin
			oplot,mrmg,col,psym=psm(i),thick=th,color=ci(i),symsi=ss
			tt = where(mrmg gt -25. and mrmg lt -15. and $
				   col gt -0.1 and col lt 8., nh)
			   ;nh = n_elements(mrmg)
			if keyword_set(plterr) then begin
				!p.psym=3
				oploterror,mrmg,col,mrmge,cole,errcolor=ci(i)
				!p.psym=0
			endif
		endelse
	endif else nh = 0
	leg(i) = string(tylabs(i),nh,format='(a-4,i4)')
endfor
;
; legend and psfile
if keyword_set(ps) then begin
	if keyword_set(limleg) then begin
		legend,lem,charthi=th,box=0,/right,charsi=li
		legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li,/bottom
	endif else $
		legend,leg,thick=th,box=1,/right,color=ci,psym=psm,charthi=th,charsi=li
	psclose
endif else legend,leg,thick=th,box=1,color=ci,psym=psm,charthi=th,charsi=li
;
!p.font=font_store
;
return
end
