function comd, z, dz, h0, om, ov
;
; calculate comoving distance at redshift z (in Mpc)
;	dz determines how accurate the results are:
;	   the smaller dz the better the result (but
;	   the longer it takes to calculate)  start at 0.01
;
c=2.9979d5	; kms^-1
dh=c/h0
;
n=long(z/dz)
;
vi=0.
for i=0L,n do begin
	z=i*dz
	ez=sqrt(om*(1.+z)^3 + ov)
	vi = vi + dz/ez
endfor
dc=dh * vi
;
return,dc
end
