pro testdvol
;
;	compare with Figure 5 in David W. Hogg's "Distance Measures in
;	cosmology", astro-ph/9905116.
;
c=2.9979d5
h=70.
om=0.3
ov=0.7
dh=c/h
dv=fltarr(501)
zs=findgen(501)*0.01
for i=0,500 do begin
    dz=0.01
    z=i*dz
    dv(i)=dvol(z,dz,1.d0,h,om,ov)*(1.d0/dh)^3/dz
endfor
;
plot,zs,dv,linesty=1,xran=[0,5],yran=[0,1.1],ysty=1,xtit='redshift z',$
	ytitle='comoving volume element [1/D!DH!N]!U3!N dV!DC!N/dz/d!7X!3',$
	charsi=2.0,charthi=3,thick=3,xthick=3,ythick=3
;
om=0.2
ov=0.8
dv=fltarr(501)
for i=0,500 do begin
    dz=0.01
    z=i*dz
    dv(i)=dvol(z,dz,1.d0,h,om,ov)*(1.d0/dh)^3/dz
endfor
oplot,zs,dv,linesty=2,thick=3
;
om=1.0
ov=0.
dv=fltarr(501)
for i=0,500 do begin
    dz=0.01
    z=i*dz
    dv(i)=dvol(z,dz,1.d0,h,om,ov)*(1.d0/dh)^3/dz
endfor
oplot,zs,dv,linesty=0,thick=3
;
legend,['!7X!3!DM!N=0.2,!7X!DK!N!3=0.8', $
	'!7X!3!DM!N=0.3,!7X!DK!N!3=0.7', $
	'!7X!3!DM!N=1.0,!7X!DK!N!3=0.0'],linesty=[2,1,0],charsi=2,charthi=3,$
	thick=3,box=0
;
return
end
