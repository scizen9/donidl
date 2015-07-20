pro tp_plot
;+
; plot the data
;-
readcol,'tp_solve.dat',id,rast,decst,ra0,dec0,ra1,dec1,format='A,A,A,D,D,D,D'
s = where(ra0 lt 100)
ra0[s] = ra0[s] + 360.d0
ra1[s] = ra1[s] + 360.d0
;
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=3
si=1.75
ci = [colordex('Red'),colordex('Green')]
tstr = '30 July 2014 TPOINT Run'
rarng = [220,430]
decrng= [-35,90]
;
plot,ra0,dec0,psym=1,symsi=si,title=tstr, $
	charsi=si,charthi=th,/nodata, $
	xtitle='R.A. (deg)',xthick=th,xran=rarng,/xs, $
	ytitle='Dec  (deg)',ythick=th,yran=decrng,/ys
oplot,ra0,dec0,psym=1,symsi=si,color=ci[0]
oplot,ra1,dec1,psym=1,symsi=si,color=ci[1]
legend,['HDR','Solved'],psym=[1,1],symsi=[si,si],color=ci, $
	charsi=si,charthi=th,/right,/bottom,box=0
;
dra = (ra0 - ra1) * cos(dec1/!RADEG) * 3600.d0
ddec = (dec0 - dec1) * 3600.d0
dpos = sqrt(dra^2 + ddec^2)
;
q=''
read,'next: ',q
plot,ra1,dra,psym=2,symsi=si,title=tstr, $
	charsi=si,charthi=th, $
	xtitle='R.A. Solved (deg)',xthick=th,xran=rarng,/xs, $
	ytitle='RA HDR - RA Solved (asec)',ythick=th;,yran=decrng,/ys
oplot,!x.crange,[0,0],linesty=2
;
read,'next: ',q
plot,ra1,ddec,psym=2,symsi=si,title=tstr, $
	charsi=si,charthi=th, $
	xtitle='R.A. Solved (deg)',xthick=th,xran=rarng,/xs, $
	ytitle='Dec HDR - Dec Solved (asec)',ythick=th;,yran=decrng,/ys
oplot,!x.crange,[0,0],linesty=2
;
read,'next: ',q
plot,ra1,ddec,psym=2,symsi=si,title=tstr, $
	charsi=si,charthi=th, $
	xtitle='R.A. Solved (deg)',xthick=th,xran=rarng,/xs, $
	ytitle='Dec HDR - Dec Solved (asec)',ythick=th,yran=[-100,800],/ys
oplot,!x.crange,[0,0],linesty=2
;
read,'next: ',q
plot,dec1,dra,psym=2,symsi=si,title=tstr, $
	charsi=si,charthi=th, $
	xtitle='Dec Solved (deg)',xthick=th,xran=decrng,/xs, $
	ytitle='RA HDR - RA Solved (asec)',ythick=th;,yran=decrng,/ys
oplot,!x.crange,[0,0],linesty=2
;
read,'next: ',q
plot,dec1,ddec,psym=2,symsi=si,title=tstr, $
	charsi=si,charthi=th, $
	xtitle='Dec Solved (deg)',xthick=th,xran=decrng,/xs, $
	ytitle='Dec HDR - Dec Solved (asec)',ythick=th;,yran=decrng,/ys
oplot,!x.crange,[0,0],linesty=2
;
read,'next: ',q
plot,dec1,ddec,psym=2,symsi=si,title=tstr, $
	charsi=si,charthi=th, $
	xtitle='Dec Solved (deg)',xthick=th,xran=decrng,/xs, $
	ytitle='Dec HDR - Dec Solved (asec)',ythick=th,yran=[-100,800],/ys
oplot,!x.crange,[0,0],linesty=2
;
return
end
