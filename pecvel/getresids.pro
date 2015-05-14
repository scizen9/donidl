pro getresids
;
readcol,'snls_pub.list',name,z,b,be,s,se,c,ce,mu_b,mu_b_e, $
	format='(a,f,f,f,f,f,f,f,f,f)'
readcol,'snls_ids_pub.list',pname,ra,dec,format='(a,a,a)',skip=1
;
; convert coords
np=n_elements(pname)
rad=dblarr(np)
decd=dblarr(np)
for i=0,np-1 do begin
    radec_parse,ra(i),dec(i),':',alpha,delta
    rad(i)=alpha
    decd(i)=delta
endfor
;
;M = -19.31
;Me = 0.03
alpha = 1.52
;alpha_e = 0.14
beta = 1.57
;beta_e = 0.15
;mu_b = b - M + alpha * (s - 1.) - beta * c
h0=69.6   ; best for hi-z offset
om=0.263
ov=0.737
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
oplot,zp,mup,thick=3
oploterr,z,mu_b,mu_b_e,8
;
t=where(z ge 0.8)
oplot,z(t),mu_b(t),psym=8,color=colordex('Zpurple')
t=where(z ge 0.6 and z lt 0.8)
oplot,z(t),mu_b(t),psym=8,color=colordex('Ired')
t=where(z ge 0.4 and z lt 0.6)
oplot,z(t),mu_b(t),psym=8,color=colordex('Rorange')
t=where(z ge 0.2 and z lt 0.4)
oplot,z(t),mu_b(t),psym=8,color=colordex('Ggreen')
;t=where(z lt 0.2)
;oplot,z(t),mu_b(t),psym=8,color=colordex('Ublue')
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
;t=where(z lt 0.2)
;oplot,z(t),resid(t),psym=8,color=colordex('Ublue')
;
!p.multi=0
!p.font=-1

openw,1,'resid.dat'
for i=0,n_elements(z)-1 do begin
    x=strpos(pname, name(i))
    t=where(x ge 0, n)
    if n ge 1 then begin
        alpha=rad(t(0))
        delta=decd(t(0))
    endif else begin
        alpha = 0.0
        delta = 0.0
    endelse
    printf,1,name(i),z(i),resid(i),alpha,delta,b(i),be(i),s(i),se(i),$
      c(i),ce(i),mu_b(i),mu_b_e(i), $
      format='(a6,f6.3,f7.3,2f11.6,f8.3,3f6.3,4f7.3)'
endfor
close,1
return
end
