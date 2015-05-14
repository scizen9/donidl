function volz,zl,h0,om,ov
;
; return the total volume of the universe out to zl in Mpc^3
;
dz=0.00001d0
;
c=2.9979d5	; kms^-1
dh=c/h0
;
n=long(zl/dz)
;
v=0.
for i=0L,n do begin
	z=i*dz
	ez=sqrt(om*(1.d0+z)^3.d0 + ov)
	v = v + dz/ez
endfor
dc=dh * v
vc = 4.0d0*!dpi/3.d0 * dc^3
;
return,vc
end
