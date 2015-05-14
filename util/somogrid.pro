pro somogrid,n
;
ras=randomu(seed,n)*360.d0
decs=randomu(seed,n)*180.d0 - 90.d0
;
;ra0=251.d0
;dec0=10.d0
ra0=270.d0
dec0=30.d0
ra1=ra0-180.d0
dec1=-dec0
;
si=2.
th=3
plot,ras,decs,psym=3,charsi=si,charthi=th,xthick=th,ythick=th, $
	xran=[360,0],yran=[-90,90],xsty=1,ysty=1, $
	xtitle='RA',ytitle='Dec'
oplot,[ra0,ra0],[dec0,dec0],psym=4,symsi=3,thick=th
oplot,[ra1,ra1],[dec1,dec1],psym=1,symsi=2,thick=th
oplot,[0,360],[0,0]
for i=0,n-1 do begin
	ra=ras(i) & dec=decs(i)
	somo,ras(i),decs(i),100.,dr,dd,/silent
	rac=ra+dr & decc=dec+dd
	oplot,[ra,rac],[dec,decc]
	oplot,[ra,ra],[dec,dec],psym=4
endfor
;
return
end
