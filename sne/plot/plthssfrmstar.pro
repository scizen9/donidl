pro plthssfrmstar,svfil,ps=ps
;+
; 	plthssfrmstar - Plot sSFR versus M* from zpeg results
;
; INPUTS:
;	svfil	- save file with zpeg results
;
; KEYWORDS:
;	ps	- set to output a postscript plot (ssfr.ps)
;
; HISTORY:
;	09-NOV-2010 - jdn: Initial Version
;
;-
common lowz_sne_info
;
restore,svfil
;
; pull out pegase data
get_peg,data,agel,agel_m,agel_p,mstar,mstar_m,mstar_p, $
	sfr,sfr_m,sfr_p,ssfr,ssfr_m,ssfr_p,ebv,chi2,name,count=nsn, $
	ssfrlims=[-13.,-3.],sfrlims=[-5,8]
name=strtrim(name,2)
;
notes=strarr(nsn)
tyn = intarr(nsn)
;
for i=0,nsn-1 do begin
	s=snfind(name[i])
	if s[0] ge 0 then begin
		notes[i] = sndat[s].type
		tyn[i]   = sndat[s].tyn
	endif else	print,'Error with ',name[i]
endfor
;
; types to plot
tyran = [ $
	[2,4], $        ; Ib, Ib/c, Ic
	[5,9], $        ; II (all)
	[1,1]]          ; Ia
tylabs = [ $
	'Ibc', $
	'II', $
	'Ia']
;
iIbc = 0
iII = 1
iIa = 2
;
; get wyder07 data
; mass estimates
ifil='~/gcmd/wyder07/ir1.1_cmd_nuv.fits'
wmass  = mrdfits(ifil,7)
wline  = mrdfits(ifil,6)
;
; photometry
ifil='~/gcmd/wyder07/galex_sdss_phot_z0.1.dat'
readcol,ifil,wfmg,wfme,wnmg,wnme,wumg,wume,wgmg,wgme,wrmg,wrme, $
	form='f,f,f,f,f,f,f,f,f,f',/silent
ng = n_elements(wnmg)
;
; ssfr
wssfr = fltarr(ng)
z = fltarr(ng) + 0.1
lumd = sullivanlumdist(0.1,omega_l=0.73,omega_m=0.27,h0=73.,/silent)
mu = -5. + 5. * alog10( lumd * 10.^6 )
for i=0,ng-1 do begin
	uv_sfr,z[i],wnmg[i]+mu,wnme[i],wrmg[i]+mu,wrme[i],sf,sfe, $
		rab=(wline[i].h_alpha_flux/wline[i].h_beta_flux)
;		dn4k=wmass[i].d4000n_cor
	wssfr[i] = sf/10.^(wmass[i].mass)
endfor
wssfr = alog10(wssfr)
;
; set up plot
font_store=!p.font
if keyword_set(ps) then begin
	tmp = svfil
	rute = gettok(tmp,'.')
	psfile,rute+'_ssfr_mstar'
	!p.font=0
	xlab = 'log M!D*!N (M'+sunsymbol()+')'
	th=5
	si=2.2
	li=2.0
	ss=0.8
	xo=-0.20
	yo=0.12
	ym=-0.24
	sf=0.6
endif else begin
	xlab = 'log M!D*!N (M!D!9n!3!N)'
	th=3
	si=2.7
	li=2.0
	ss=2.0
	xo=-0.15
	yo=0.08
	ym=-0.17
	sf=2.0
endelse
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
a=[findgen(40)*(!pi*2/40.),0.]
usersym,cos(a),sin(a),thick=th,/fill
clrs=['R','B','C']
psm=[5,6,8]
nm=n_elements(clrs)
ci=lonarr(nm)
for i=0,nm-1 do ci(i) = colordex(clrs(i))
;
; Host sSFR
xrng=[6.,13]
yrng=[-13.1,-7.99]
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=xlab,xran=xrng,xsty=1, $
	ytitle='log SFR/M!D*!N (yr!U-1!N)', yran=yrng,ysty=1
;
; Wyder07 galaxies
oplot,wmass.mass,wssfr,psym=3,color=colordex('C')
make2dhist,wmass.mass,wssfr,xrng,0.2,[-12.5,-8.5],0.1,f
mxf=max(f.f)
nlevs=16
cl=lonarr(nlevs)
for i=0,nlevs-1 do begin
	gsl = 255 - i*(256/nlevs)
	cl[i] = colordex(rgb=[gsl,gsl,gsl])
endfor
levs=(indgen(nlevs)+1)*(mxf/float(nlevs))
contour,f.f,f.x,f.y,/noerase,/overplot,levels=levs,c_colors=cl,/fill
contour,f.f,f.x,f.y,/noerase,/overplot,levels=levs
;
; hosts
;
; loop over tyran
for i=0,nm-1 do begin
	g=where(tyn ge tyran[0,i] and tyn le tyran[1,i], ng)
	if ng gt 0 then begin
		oplot,mstar[g],ssfr[g],psym=psm[i],symsi=ss*.8,thick=th, $
			color=ci[i]
	endif
endfor
;
legend,tylabs,psym=psm,symsi=[ss*.8,ss,ss], thick=[th,th,th], $
	charsi=sf+1.0,charthi=th,/bottom,color=ci
;
; LMC SFH
lmstar = [10.114,10.114]
lmstar_m = [0.336,0.336]
lmstar_p = [0.187,0.187]
lmssfr = [-10.512,-10.512]
lmssfr_m = [0.391,0.391]
lmssfr_p = [0.648,0.648]
oplot,lmstar,lmssfr,psym=8,symsi=2.
oploterror,lmstar,lmssfr,lmstar_m,lmssfr_m,/lobar,psym=3,thick=th
oploterror,lmstar,lmssfr,lmstar_p,lmssfr_p,/hibar,psym=3,thick=th
xyouts,10.2,-10.9,'LMC',charsi=si,charthi=th
;
if keyword_set(ps) then psclose
!p.font=font_store
;
return
end
