function lineflx,w,ws,fx,fxe,sn,cw,lnam,pointer=p,silent=silent
;
tol=10.
dif=abs(ws-w)
t=where(dif le tol,nt)
if nt ge 1 then  begin
	dif=dif(t)
	s=sort(dif)
	p=t(s(0))
	flx = fx(p)
endif else begin
	p = -1
	flx = 0.
endelse
if not keyword_set(silent) and p ge 0 then begin
	if n_params(0) ge 7 then $
		name = lnam(p) $
	else	name = 'Unk'
	if n_params(0) ge 6 then begin
		dw = cw(p)-ws(p)
		v = !phys_c * (dw/ws(p))
		wobs = cw(p)
	endif else begin
		dw = 0.
		wobs = -999.9
		v = -1.e9
	endelse
	if n_params(0) ge 5 then $
		s2n = sn(p) $
	else	s2n = -99.
	if n_params(0) ge 4 then $
		fe = fxe(p) $
	else	fe = -1.e9
	print,name,ws(p),wobs,dw,v,fx(p),fe,s2n, $
		format='(a8,3f9.2,3g11.4,f9.1)'
endif
return,flx
end
