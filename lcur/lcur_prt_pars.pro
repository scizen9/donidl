pro lcur_prt_pars
;
COMMON phot_data
COMMON parm_data
COMMON cnts_data
COMMON scar_data
COMMON phase_data
COMMON dao_data
;
if npts le 0 then begin
	print,'LCUR_PRT_PARS: ERROR, no data read in yet'
	return
endif
;
; find out what star we are on
if ( dao_varth(0) ge 0. and dao_varth(1) ge 0. and $
     dao_varth(1) ge dao_varth(0) ) then begin
     t = where(dao_var ge dao_varth(0) and dao_var le dao_varth(1) and $
	     dao_rnei ge dao_neith and dao_enframes ge dao_nflim, nfound)
     if nfound ge 1 then $
     		temp_ids = dao_id(t) $
     else	temp_ids = dao_id
endif else begin
	temp_ids = dao_id
endelse

temp_ids = temp_ids(sort(temp_ids))

t = where(temp_ids eq dao_cid, nfound)
if nfound ge 1 then $
	index = t(0) + 1 $
else	index = 0

print,''
print,'Phot file       : ',pfile,'   Filter: ',filt,' Npts: ',strn(ntpts),$
	' Errlim: ',string(merrlim,form='(f4.2)'), $
	' NeiRThr: ',string(dao_neith,form='(f4.1)')
print,'JD range (days) : ',strn(jdran(0)),' ',strn(jdran(1)), $
	'  JD zero: ',jdzero
print,'Window (days)   : ',strn(twin(0)),' ',strn(twin(1)), $
	'  Win indices: ',strn(t0),' ',strn(t1)
print,'Var Thresh      : ',strn(dao_varth(0)),' ',strn(dao_varth(1)), $
	'  var/Nvar: ',strn(index),'/',strn(dao_nvar), $
	' DAO ID     : ',strn(dao_cid)

if npks gt 0 then begin
print,'Use default pran: ',def_pran, '  Period range (days): ',strn(pran)
print,'Power threshholds(99%,FAP=0.01) Periodogram: ',strn(signi), $
	'  Noise: ',strn(noise)
print,'# Ind Freq : ',nif, '  Min,Max Fq : ',fmin,fmax
print,'Min Period : ',1.d0/fmax,' d, ',24.d0/fmax, ' h '
print,'Max Period : ',1.d0/fmin,' d, ',24.d0/fmin, ' h '
print,' '
print,'Pks   Frequency   Period(d)   Period(h)   Power  Lg10(FAP)  delFrq   Can dF'
for i=0,npks-1 do $
	print,i+1,nu(pkis(i)), pd(pkis(i)), pd(pkis(i))*24.0, px(pkis(i)), $
		faps(pkis(i)), pkerrs(i), 0.1*nu(pkis(i)) / (twin(1)-twin(0)), $
		form='(i3,f12.7,f12.7,f12.7,f9.4,f8.3,f9.3,f9.3)'
print,'Do fit subtraction: ',strn(do_sub)
print,''
endif
return
end	; pro lcur_prt_pars
