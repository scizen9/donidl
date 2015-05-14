pro planck_fit,wave,flux,t,scale
;
dt=1.
;
ep = min(flux); * 0.001
nt=9000./dt
;
scale=1.0
for i=0,nt do begin
	t=10000. - float(i)*dt
	bbflux=planck(wave,t)
	rats = flux/bbflux
        scale = avg(rats)
        diff = bbflux * scale - flux
	em = max(diff)
        print,t,em
;        stop
	if em le ep then break
endfor
;
return
end
