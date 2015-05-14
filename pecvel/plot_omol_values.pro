pro plot_omol_values,ifile
;
readcol,ifile,mdl,dh1,dhe,om,omp,omm,ol,olp,olm,al,alp,alm,bl,blp,blm, $
	form='i,f,f,f,f,f,f,f,f,f,f,f,f,f,f'
;
!p.multi=[0,1,2]
a=[findgen(16)*(!pi*2/16.),0.]
si=1.8
th=3
g=where(mdl ge 1. and mdl lt 17)
xbh=[5,6,7,8,13,14,15,16]
mxmdl = max(mdl)
mxmdl=-1
usersym,cos(a),sin(a),thick=th
plot,mdl(g),om(g),psym=8, $
	xtitle='', xthick=th, xran=[0,17], xsty=1, $
	ytitle=textoidl('\Omega_m'), ythick=th, yran=[.235,.315], ysty=1, $
	charsi=si, charthi=th, thick=th, symsi=si, title=ifile
errplot,mdl(g),om(g)-omm(g),om(g)+omp(g),thick=th
oplot,[0,100],[om(0),om(0)]
oplot,[0,100],[om(0)-omm(0),om(0)-omm(0)],linesty=3
oplot,[0,100],[om(0)+omp(0),om(0)+omp(0)],linesty=3
usersym,cos(a),sin(a),/fill
oplot,xbh,om(xbh),psym=8,symsi=si
;
usersym,cos(a),sin(a),thick=th
plot,mdl(g),ol(g),psym=8, $
	xtitle='MODEL #', xthick=th, xran=[0,17], xsty=1, $
	ytitle=textoidl('\Omega_Lambda'), ythick=th, yran=[.64,.93], ysty=1, $
	charsi=si, charthi=th, thick=th, symsi=si
errplot,mdl(g),ol(g)-olm(g),ol(g)+olp(g),thick=th
oplot,[0,100],[ol(0),ol(0)]
oplot,[0,100],[ol(0)-olm(0),ol(0)-olm(0)],linesty=3
oplot,[0,100],[ol(0)+olp(0),ol(0)+olp(0)],linesty=3
usersym,cos(a),sin(a),/fill
oplot,xbh,ol(xbh),psym=8,symsi=si
;
!p.multi=0
q=''
read,'next: ',q
;
usersym,cos(a),sin(a),thick=th
plot,dh1(g),ol(g),psym=8, $
	xtitle=textoidl('\Delta H'), xthick=th, $
	ytitle=textoidl('\Omega_\Lambda'), ythick=th, yran=[.64,0.93], ysty=1, $
	charsi=si, charthi=th, thick=th, symsi=si, title=ifile
errplot,dh1(g),ol(g)-olm(g),ol(g)+olp(g),thick=th
xyouts,dh1(15),ol(15),strn(15),charsi=si,charthi=th, align=1.0
xyouts,dh1(16),ol(16),strn(16),charsi=si,charthi=th, align=1.0
xyouts,dh1(7),ol(7),strn(7),charsi=si,charthi=th, align=1.0
xyouts,dh1(8),ol(8),strn(8),charsi=si,charthi=th, align=1.0
oplot,[-100,100],[ol(0),ol(0)]
oplot,[-100,100],[ol(0)-olm(0),ol(0)-olm(0)],linesty=3
oplot,[-100,100],[ol(0)+olp(0),ol(0)+olp(0)],linesty=3
usersym,cos(a),sin(a),/fill
oplot,dh1(xbh),ol(xbh),psym=8,symsi=si
;
read,'next: ',q
;
usersym,cos(a),sin(a),thick=th
plot,dh1(g),om(g),psym=8, $
	xtitle=textoidl('\Delta H'), xthick=th, $
	ytitle=textoidl('\Omega_m'), ythick=th, yran=[.235,0.315], ysty=1, $
	charsi=si, charthi=th, thick=th, symsi=si, title=ifile
errplot,dh1(g),om(g)-omm(g),om(g)+omp(g),thick=th
xyouts,dh1(15),om(15),strn(15),charsi=si,charthi=th, align=1.0
xyouts,dh1(16),om(16),strn(16),charsi=si,charthi=th, align=1.0
xyouts,dh1(7),om(7),strn(7),charsi=si,charthi=th, align=1.0
xyouts,dh1(8),om(8),strn(8),charsi=si,charthi=th, align=1.0
oplot,[-100,100],[om(0),om(0)]
oplot,[-100,100],[om(0)-omm(0),om(0)-omm(0)],linesty=3
oplot,[-100,100],[om(0)+omp(0),om(0)+omp(0)],linesty=3
usersym,cos(a),sin(a),/fill
oplot,dh1(xbh),om(xbh),psym=8,symsi=si
;
read,'next: ',q
;
usersym,cos(a),sin(a),thick=th
plot,dh1(g),al(g),psym=8, $
	xtitle=textoidl('\Delta H'), xthick=th, $
	ytitle=textoidl('\alpha'), ythick=th, yran=[1.33,1.83], ysty=1, $
	charsi=si, charthi=th, thick=th, symsi=si, title=ifile
errplot,dh1(g),al(g)+alm(g),al(g)+alp(g),thick=th
xyouts,dh1(15),al(15),strn(15),charsi=si,charthi=th, align=1.0
xyouts,dh1(16),al(16),strn(16),charsi=si,charthi=th, align=1.0
xyouts,dh1(7),al(7),strn(7),charsi=si,charthi=th, align=1.0
xyouts,dh1(8),al(8),strn(8),charsi=si,charthi=th, align=1.0
oplot,[-100,100],[al(0),al(0)]
oplot,[-100,100],[al(0)+alm(0),al(0)+alm(0)],linesty=3
oplot,[-100,100],[al(0)+alp(0),al(0)+alp(0)],linesty=3
usersym,cos(a),sin(a),/fill
oplot,dh1(xbh),al(xbh),psym=8,symsi=si
;
read,'next: ',q
;
usersym,cos(a),sin(a),thick=th
plot,dh1(g),bl(g),psym=8, $
	xtitle=textoidl('\Delta H'), xthick=th, $
	ytitle=textoidl('\beta'), ythick=th, yran=[1.53,2.03], ysty=1, $
	charsi=si, charthi=th, thick=th, symsi=si, title=ifile
errplot,dh1(g),bl(g)+blm(g),bl(g)+blp(g),thick=th
xyouts,dh1(15),bl(15),strn(15),charsi=si,charthi=th, align=1.0
xyouts,dh1(16),bl(16),strn(16),charsi=si,charthi=th, align=1.0
xyouts,dh1(7),bl(7),strn(7),charsi=si,charthi=th, align=1.0
xyouts,dh1(8),bl(8),strn(8),charsi=si,charthi=th, align=1.0
oplot,[-100,100],[bl(0),bl(0)]
oplot,[-100,100],[bl(0)+blm(0),bl(0)+blm(0)],linesty=3
oplot,[-100,100],[bl(0)+blp(0),bl(0)+blp(0)],linesty=3
usersym,cos(a),sin(a),/fill
oplot,dh1(xbh),bl(xbh),psym=8,symsi=si
;
return
end
