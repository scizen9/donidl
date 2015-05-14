pro lcur_phase_prt
;

;
; get the data

jd	= lcur_com_twin('jd')
phase	= lcur_com_twin('phase')

;

p = 0.d0
d = 0.d0

read,'Enter phase, delta phase: ',p,d

if (p le 1.0 and p ge 0.0) then begin

	good = where(phase le (p+d) and phase ge (p-d), ngood)

	if ngood gt 0 then begin

		gjd = jd(good)
		gph = phase(good)
		t = sort(gjd)
		gph = gph(t)
		gjd = gjd(t)
		out = dblarr(2,ngood)
		out(0,*) = gph(*)
		out(1,*) = gjd(*)
		print,out,form='(4(f7.3,f12.4))'
;		for i=0,ngood-1 do $
;			print,gph(i),gjd(i),form='(f5.3, f12.4)'

	endif else print,'No points found in phase range: ',p,d


endif else print,'Invalid phase, delta phase: ',p,d

;
return
end	; pro lcur_phase_prt
