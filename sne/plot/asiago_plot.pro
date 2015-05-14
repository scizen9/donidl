pro asiago_plot,match,zrange=zrange,vrange=vrange
;
read_asiago_sn,sn,host,hra,hdec,snra,sndec,gtyp,gtc,ginc,gpa,vz,gbt,glgd25,$
	                ofew,ofns,snfilt,snmag,mty,type,ddate,disc
;
c = 2.99792458d5
;
; convert all to cz
t=where(vz gt 0. and vz lt 2.)
cz=vz
cz(t) = vz(t) * c
;
; convert all to z
t=where(vz gt 2. or vz lt 0.)
z=vz
z(t) = vz(t) / c
;
if keyword_set(zrange) then begin
	g=where((z lt zrange(1) and z gt zrange(0)) and $
		strpos(type,match) ge 0,n)
	xlab='z'
	tlab=match+': '+strn(n)+', '+strn(zrange(0),form='(f6.3)')+' < z < '+ $
		strn(zrange(1),form='(f6.3)')+',  '+systime(0)
	xran=zrange
	bins=0.01
	rs=z
endif else begin
	if keyword_set(vrange) then $
		vran=vrange $
	else	vran=[5.,3000.]
	g=where((cz lt vran(1) and cz gt vran(0)) and $
		strpos(type,match) ge 0,n)
	xlab='V!DHELIO!N km s!U-1!N'
	tlab=match+': '+strn(n)+', '+strn(vran(0),form='(f7.1)')+ $
		' < V!DHELIO!N < '+strn(vran(1),form='(f7.1)')+',  '+systime(0)
	xran=vran
	bins=50.
	rs=cz
endelse
;
th=3
si=1.5
plot,hra(g),hdec(g),psym=1,yran=[-90,90],ysty=1,ytitle='DEC',ythick=th, $
	xran=[0,360],xsty=1,xtitle='RA',xthick=th,charsi=si,charthi=th, $
	title=tlab
;
q=''
read,'next: ',q
hv=histogram(rs(g),min=xran(0),max=xran(1),binsi=bins,loc=xhv)
plot,xhv,hv,psym=10,xran=xran,xsty=1,xtitle=xlab,xthick=th, $
	ytitle='N',ythick=th,charsi=si,charthi=th, $
	title=tlab
;
q=''
read,'next: ',q
hi=histogram(ginc(g),min=0,max=90,binsi=2.0,loc=xhi)
plot,xhi,hi,psym=10,xran=[0,90],xsty=1,xtitle='INC',xthick=th, $
	ytitle='N',ythick=th,charsi=si,charthi=th, $
	title=tlab
;
return
end
