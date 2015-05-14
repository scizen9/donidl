;readsne,z,s,se,ebv,ebve,c,ce,b,be,chi2,fp,pp,nr,nt,cl,dl,dmaxe,ldl,bad,mwebv
readcol,'nearby_pub.list',nname,nz,nmb,nmber,ns,nser,nc,ncer,nmub,nmuber, $
	format='(a,f,f,f,f,f,f,f,f,f)'
readcol,'snls_pub.list',sname,sz,smb,smber,ss,sser,sc,scer,smub,smuber, $
	format='(a,f,f,f,f,f,f,f,f,f)'
mu_b = [nmub, smub]
mu_b_e = [nmuber, smuber]
se = [nser, sser]
ce = [ncer, scer]
;
z = [nz,sz]
;
;M = -19.31
;Me = 0.03
alpha = 1.52
;alpha_e = 0.14
beta = 1.57
;beta_e = 0.15
h0=70.475  ; best for total offset
;h0=69.6    ; best for hi-z offset
;h0=71.78   ; best for lo-z offset
om=0.263
ov=0.737
;mu_b = b - M + alpha * (s - 1.) - beta * c
dz=0.001d0
mu = mu_b-mu_b
for i=0,n_elements(z)-1 do $
    mu(i) = muz2(z(i),dz,h0,om,ov)
resid = mu_b - mu
l=sort(z)

zp = findgen(1200)/1000
mup = fltarr(1200)
for i=0,1199 do mup(i) = muz2(zp(i),dz,h0,om,ov)
mu_b_e = sqrt(mu_b_e^2 + (alpha*se)^2 + (beta*ce)^2 + (0.15)^2)

!p.multi=[0,1,2]
a=[findgen(16)*(!pi*2/16.),0.]
usersym,cos(a)*0.5,sin(a)*0.5,/fill
;
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
!p.font=0
;
plot,z,mu_b,psym=8,xtitle='z',ytitle='!Mm!3!DB!N',yran=[33,46],ysty=1,$
  xran=[0.,1.11],xsty=1,charsi=1.5,charthi=3,xthi=3,ythi=3,/nodata
oploterr,z,mu_b,mu_b_e,8
oplot,zp,mup,thick=3
;
t=where(z ge 0.8)
oplot,z(t),mu_b(t),psym=8,color=colordex('Zpurple')
t=where(z ge 0.6 and z lt 0.8)
oplot,z(t),mu_b(t),psym=8,color=colordex('Ired')
t=where(z ge 0.4 and z lt 0.6)
oplot,z(t),mu_b(t),psym=8,color=colordex('Rorange')
t=where(z ge 0.2 and z lt 0.4)
oplot,z(t),mu_b(t),psym=8,color=colordex('Ggreen')
t=where(z lt 0.2)
oplot,z(t),mu_b(t),psym=8,color=colordex('Ublue')
;
plot,z,resid,psym=8,xtitle='z',ytitle='!MD m!3!DB!N',yran=[-1,1.2],ysty=1,$
  xran=[0.,1.11],xsty=1,charsi=1.5,charthi=3,xthi=3,ythi=3,/nodata
oploterr,z,resid,mu_b_e,8
oplot,[-100,100],[0,0],thick=3
;
t=where(z ge 0.8)
oplot,z(t),resid(t),psym=8,color=colordex('Zpurple')
t=where(z ge 0.6 and z lt 0.8)
oplot,z(t),resid(t),psym=8,color=colordex('Ired')
t=where(z ge 0.4 and z lt 0.6)
oplot,z(t),resid(t),psym=8,color=colordex('Rorange')
t=where(z ge 0.2 and z lt 0.4)
oplot,z(t),resid(t),psym=8,color=colordex('Ggreen')
t=where(z lt 0.2)
oplot,z(t),resid(t),psym=8,color=colordex('Ublue')

hiz=where(z ge 0.2)
loz=where(z lt 0.2)
mean=wmean(resid,mu_b_e)
stdv=wstdev(resid,mu_b_e)
print,'Total offset: ',mean,' +- ',stdv
print,'Hi-z offset : ',wmean(resid(hiz),mu_b_e(hiz)),' +- ', $
  wstdev(resid(hiz),mu_b_e(hiz))
print,'Lo-z offset : ',wmean(resid(loz),mu_b_e(hiz)),' +- ', $
  wstdev(resid(loz),mu_b_e(loz))
;
!p.multi=0
!p.font=-1
