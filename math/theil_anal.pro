pro theil_anal,x,y,zero,slope,nres,sigz,sigs,silent=silent
;
; perform bootstrap resampling to estimate errors on slope and zero
;
; x,y - input quantities to estimate slope from
; zero,slope - fit values
; nres - number of resamplings to perform
; sigz,sigs - stddev of resampled values
;
; first full sample estimation:
;
pars=theil(x,y,sigma=sig,/complete)
slope=pars(0)
zero=pars(1)
;
n=n_elements(x)
slopes = fltarr(nres)
zeros = fltarr(nres)
;
for i = 0,nres-1 do begin
    ind = fix(randomu(seed,n)*n)
    xsam = x(ind)
    ysam = y(ind)
    pars=theil(xsam,ysam,/complete)
    slopes(i)=pars(0)
    zeros(i)=pars(1)
    if i mod 10 eq 0 and not keyword_set(silent) then $
      print,string(13B),'Resamplings: ',i+1,'/',nres,form='($,a1,a,i6,a,i6)'
endfor
if not keyword_set(silent) then begin
    print,string(13B),'Resamplings: ',i,'/',nres,form='($,a1,a,i6,a,i6)'
    print,' '
endif
;
sigz=stddev(zeros)
sigs=stddev(slopes)
;
if not keyword_set(silent) then $
    print,'Zp, Slope: ',zero,' +- ',sigz,slope,' +- ',sigs, $
      form='(a,f7.3,a,f5.3,2x,f7.3,a,f5.3)'
;
return
end
