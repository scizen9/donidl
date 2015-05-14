pro uv_sfr,z,nmg,nme,rmg,rme,sfr,sfre,verbose=verbose,dn4k=dn4k,rab=rab
;+
;	uv_sfr - calculate the SFR in M_sun/yr given NUV and r-band mags
;		uses Schiminovich et al. 2007 and Treyer et al. 2007
;
;	dn4k = use Dn(4000) to calculate extinction (Johnson et al. 2006)
;	rab  = use R(alpha/beta), Balmer decrement to calculate extinction
;-
; calculate A(NUV)
if rmg lt 0. or (nmg-rmg) gt 4.0 then begin
	Anuv = 0.
	Afuv = 0.
	Ar   = 0.
endif else begin
	Anuv = 1.71 * (nmg - rmg) - 2.86	; Magnitudes of extinction
	Afuv = Anuv / 0.81
	Ar   = 0.35 * Afuv
endelse
;
; if Dn(4000) provided used eq 10 from Wyder07
if keyword_set(dn4k) then begin
	ac = dn4k - 1.25
	bc = (nmg - rmg) - 2.
	Afuv = 1.27 - 1.56*ac + 1.35*bc - 1.24*ac*bc
	Anuv = 0.81 * Afuv
	Ar   = 0.35 * Afuv
endif
if keyword_set(rab) then begin
	if rab gt 0. then $
		ebmvg = 2.5*alog10(rab/2.87) / 1.163 $ ; Wyder (2007), SS 3.5.1
	else	ebmvg = 0.0
	Anuv = 3.63 * ebmvg
	Afuv = Anuv / 0.81
	Ar   = 1.57 * ebmvg
endif
;
; limit to > 0
Anuv = Anuv > 0.
if keyword_set(verbose) then begin
	print,'A(FUV) = ',Afuv,format='(a,f9.3)'
	print,'A(NUV) = ',Anuv,format='(a,f9.3)'
	print,'A(r) = ',Ar,format='(a,f9.3)'
endif
;
; de-extinct NUV mag
nmg0 = nmg - Anuv
nflx = 3630.78*10.^(-0.4*nmg0) * 10.^(-23)	; flux in erg/s/Hz/cm^2
nfle = nflx * nme / 1.0857362d0			; flux error
;
; get luminosity distance
d = sullivanlumdist(z,/silent)*3.085677581d+24	; cm
;
; luminosity
Lnuv = nflx * 4. * !DPI * d^2.			; erg/s/Hz
Lnuvp= (nflx+nfle) * 4. * !DPI * d^2.		; erg/s/Hz
Lnuvm= (nflx-nfle) * 4. * !DPI * d^2.		; erg/s/Hz
;
; calculate SFR
sfr = Lnuv * 10.^(-28.165)
sfrp= Lnuvp* 10.^(-28.165)
sfrm= Lnuvm* 10.^(-28.165)
sfre= ( (sfrp-sfr) + (sfr-sfrm) ) / 2.0
;
if keyword_set(verbose) then $
	print,'SFR = ',sfr,' +- ',sfre,form='(a,f7.3,a,f7.3)'
;
return
end
