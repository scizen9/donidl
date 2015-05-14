function dvol,z,dz,domega,h0,om,ov
;
; calculate comoving volume element at z within dz,domega
;  in Mpc^3
;
c=2.9979d5	; kms^-1
dh=c/h0
;
ez = sqrt(om*(1.+z)^3 + ov)
da = comd(z,dz,h0,om,ov)/(1.+z)
dvc = dz * domega * dh * ( (1.+z)^2 * da^2 ) / ez
;
return,dvc
end
