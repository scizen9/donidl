function costime,zl
;
; what is the age of the universe at redshift zl?
;
dz=0.01d0
h0=1.d0/13.7d9	; WMAP value (years^-1)
ov=0.7d0	; omega lambda
om=0.3d0	; omega matter
;
n=long((10000.0-zl)/dz)
z=dindgen(n)*dz + zl
ez=dz/(h0*(1.+z)*sqrt(om*(1.d0+z)^3.d0 + ov))
return,total(ez)
;
end
