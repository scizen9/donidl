pro plot_pecvel_stats,indiv=indiv,scale=scale,title=title
;
type='indiv'
if keyword_set(scale) then type='scale'
ifile='wom_stats_'+type+'.dat'
readcol,ifile,mdstr,wipars,witype,wioff,wistdev,wirms,wichi2all,wichi2loz,$
	form='a,a,a,f,f,f,f,f'
ifile='omol_stats_'+type+'.dat'
readcol,ifile,mdstr,oipars,oitype,oioff,oistdev,oirms,oichi2all,oichi2loz,$
	form='a,a,a,f,f,f,f,f'
;
if keyword_set(title) then $
	tlab = 'COSMOLOGY FIT STATS '+strupcase(type) $
else	tlab = ''
nmods=n_elements(mdstr)
mdl=intarr(nmods)
for i=0,nmods-1 do mdl(i)=fix(strmid(mdstr(i),3))
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
!p.font=1
;!p.multi=[0,1,3]
ss=1.8
si=2.0
th=3
g=where(mdl ge 1. and mdl lt 17)
mxmdl = max(mdl)
a=[findgen(16)*(!pi*2/16.),0.]
usersym,cos(a)*1.1,sin(a)*1.1,thick=th;,/fill
plot,mdl(g),wirms(g)-wirms(0),psym=4, position=[0.12,0.67,0.96,0.94], $
	xtitle='', xthick=th, xran=[0.01,16.99], xsty=1, $
	ytitle=textoidl('\Delta RMS_{z<0.15}'), ythick=th, yran=[-0.014,0.027],$
	ysty=1, symsi=ss, $
	charsi=si, charthi=th, thick=th, xticks=15, xtickv=indgen(16)+1, $
	xtickname=replicate(' ',16),title=tlab
oplot,mdl(g),oirms(g)-oirms(0),psym=8,thick=th,symsi=ss
oplot,[0,100],[0,0],linesty=2
legend,[textoidl('[w,\Omega_m]'), textoidl('[\Omega_m,\Omega_{\Lambda}]')], $
	psym=[4,8],thick=th, symsi=[ss,ss],charthi=th,spac=2.0, $
	charsi=1.5,pos=[16.7,0.025],/right
;
plot,mdl(g),wichi2all(g)-wichi2all(0),psym=4, position=[0.12,0.40,0.96,0.67], $
	xtitle='', xthick=th, xran=[0.01,16.99], xsty=1, /noerase, $
	ytitle=textoidl('\Delta \chi^2_{ALL}'), ythick=th, yran=[-9,19], $
	ysty=1, symsi=ss, $
	charsi=si, charthi=th, thick=th, xticks=15, xtickv=indgen(16)+1, $
	xtickname=replicate(' ',16)
oplot,mdl(g),oichi2all(g)-oichi2all(0),psym=8,thick=th,symsi=ss
oplot,[0,100],[0,0],linesty=2
;
plot,mdl(g),wichi2loz(g)-wichi2loz(0),psym=4, position=[0.12,0.13,0.96,0.40], $
	xtitle='PECULIAR VELOCITY MODEL #', xthick=th, xran=[0.01,16.99], $
	xsty=1, ytitle=textoidl('\Delta \chi^2_{z<0.15}'), ythick=th, $
	yran=[-5,14], ysty=1, symsi=ss, $
	charsi=si, charthi=th, thick=th, xticks=15, xtickv=indgen(16)+1,/noeras
oplot,mdl(g),oichi2loz(g)-oichi2loz(0),psym=8,thick=th,symsi=ss
oplot,[0,100],[0,0],linesty=2
;
;!p.multi=0
!p.font=-1
;
return
end
