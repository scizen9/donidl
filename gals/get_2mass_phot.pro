pro get_2mass_phot,obj,j_t=j_t,errj_t=errj_t,h_t=h_t,errh_t=errh_t,k_t=k_t, $
	errk_t=errk_t,r_asec=r_asec,silent=silent
;
; read xsc cat
readcol,!2MASS_DATA+'2massXSC.dat',j,je,h,he,k,ke,dis,gal, $
	form='f,f,f,f,f,f,f,a',silent=silent
t=where(strcmp(gal,strtrim(obj,2)) eq 1, n)
if n ge 1 then begin
	t=t(0)
	j_t = j(t)
	errj_t = je(t)
	h_t = h(t)
	errh_t = he(t)
	k_t = k(t)
	errk_t = ke(t)
	r_asec = dis(t)
endif else begin
	j_t = -99.99
	errj_t = -9.99
	h_t = -99.99
	errh_t = -9.99
	k_t = -99.99
	errk_t = -9.99
	r_asec = -9.99
endelse
;
return
end
