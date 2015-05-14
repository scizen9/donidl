pro survol,z0,zl,h0,om,ov,angdeg,svol,silent=silent
;
;
; check inputs
if n_params(0) le 1 then begin
	print,'SURVOL - Usage: survol, z0, z1, h0, om, ov, angdeg, svol, </silent>'
	return
endif
;
; calculate survey volume for angdeg between redshift limits z0, zl
;
v0=volz(z0,h0,om,ov)
vl=volz(zl,h0,om,ov)
v=vl-v0
vdg=v/41253.d0
svol=vdg*angdeg
;
if not keyword_set(silent) then begin
print,'Total All-sky Volume from z of ',z0,' to ',zl,' in Mpc^3: ',v,$
	format='(a,f5.2,a,f5.2,a,g10.4)'
print,'Volume per square degree in Mpc^3: ',vdg,format='(a,g9.3)'
print,'Survey volume for ',angdeg,' square degrees in Mpc^3: ',vdg*angdeg,$
	format='(a,f6.3,a,g11.4)'
endif
;
return
end
