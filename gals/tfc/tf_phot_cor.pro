function tf_phot_cor, galind, logwmxi,  $
	w1phot=w1phot, w2phot=w2phot, iphot=iphot, $
	tf_axrat=tf_axrat, gal_axrat=gal_axrat, $
	verbose=verbose
;+
;	tf_phot_cor - return the photometric correction for TF calibration
;
; INPUTS:
;	galind	- index into glgadat GLGA photometry data struct
;-
common galdb_info
common glgadb_info
;
; init
cor = -999.
;
; get extinction ratio between W1 and W2
; using values in Yuan, Liu & Xiang 2013,
; Table 2 from Fitzpatrick 1999, PASP, 111, 63
Rw1 = 0.186
Rw2 = 0.123
RI  = 1.722
dust_fact = 1.0
if keyword_set(w2phot) then begin
	Rph = Rw2
	dust_fact = Rw2/Rw1
	Apcor = -0.041
endif else if keyword_set(w1phot) then begin
	Rph = Rw1
	Apcor = -0.034
endif else if keyword_set(iphot) then begin
	Rph = RI
	Apcor = 0.0
endif
;
; get ID
l = galind[0]
g = gfind(glgadat[l].id)
if g ge 0 then begin
	;
	; redshift
	z = galdat[g].cz/!phys_c
	;
	; k-correction (fainter -) roughly the same for W1 or W2
	; see Figure 6 from Huang et al. 2007
	Ak = -2.27*z
	;
	; for I-band use Chilingarian et al. 2010
	if keyword_set(iphot) then $
		Ak = 0.302*z + 8.768*z^2 - 68.680*z^3 + 181.904*z^4
	;
	; get axial ratio (a/b)
	axrat = glgadat[l].majax/glgadat[l].minax
	if keyword_set(gal_axrat) then $
		axrat = galdat[g].majax/galdat[g].minax
	if keyword_set(tf_axrat) then $
		axrat = 1./tf_axrat
	;
	; inclination correction (brighter +)
	Ai = (0.12 + 0.21*(logwmxi-2.5)) * alog10(axrat) * dust_fact
	;
	; for I-band use factor in Tully & Courtois 2012
	if keyword_set(iphot) then $
		Ai = (0.92 + 1.63*(logwmxi-2.5)) * alog10(axrat)
	;
	; Milky Way extinction correction (brighter +)
	; Assume SFD E(B-V) is 14% too high
	Ab = galdat[g].mwebmv*0.86*Rph
	;
	; WISE aperture correction (fainter -)
	Aa = Apcor
	;
	; totals
	cor = -(Ak + Ai + Ab + Aa)
	;
	; print
	if keyword_set(verbose) then begin
		print,'z      = ',z,format='(a,f9.4)'
		print,'Axr a/b= ',axrat,format='(a,f9.4)'
		print,'E(B-V) = ',galdat[g].mwebmv,format='(a,f9.4)'
		print,' '
		print,'Ak     = ',Ak,format='(a,f9.4)'
		print,'Ai     = ',Ai,format='(a,f9.4)'
		print,'Ab     = ',Ab,format='(a,f9.4)'
		print,'Aa     = ',Aa,format='(a,f9.4)'
		print,'-------------------'
		print,'Total  = ',cor,format='(a,f9.4)'
	endif
endif else begin
	print,'TF_PHOT_COR: ERRROR - ID not found: ',galid
endelse
;
return,cor
end
