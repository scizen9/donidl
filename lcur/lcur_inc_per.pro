pro lcur_inc_per,up=up,down=down,smaller=smaller,larger=larger, $
	double=double, half=half
;
;

inc = lcur_com_get('phinc')

if keyword_set(smaller) then begin
	inc = inc / 10.0
	print,'Inc: ',strn(inc)
	ret = lcur_com_put('phinc', inc)
	return
endif

if keyword_set(larger) then begin
	inc = inc * 10.0
	print,'Inc: ',strn(inc)
	ret = lcur_com_put('phinc', inc)
	return
endif

per = lcur_com_get('period')

if keyword_set(up) then per = per + inc

if keyword_set(down) then per = per - inc

if keyword_set(double) then per = per * 2.0

if keyword_set(half) then per = per / 2.0

ret = lcur_com_put('period', per)
lcur_phase_get
lcur_plt_phase,/mags

;
return
end
