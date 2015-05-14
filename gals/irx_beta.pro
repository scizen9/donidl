pro irx_beta,sam,irx,irxe,bet,bete
;
; calculate IRX using formulae from Dale et al. (2001)
common galdb_info
;
if n_params(0) lt 1 then begin
	print,'irx_beta, sample, irx, irx_err, beta, beta_err'
	return
endif
;
; get sample
galsam=galdat(sam)
ng=n_elements(galsam)
;
; get IRAS fluxes (janskys)
fnu60	= 3631.d0 * 10.^(-0.4*galsam.i60m_int_mag)
fnu60e	= fnu60 * galsam.i60m_int_magerr / 1.0857362d0
fnu100	= 3631.d0 * 10.^(-0.4*galsam.i100m_int_mag)
fnu100e = fnu100 * galsam.i100m_int_magerr / 1.0857362d0
;
; FIR in W m^(-2)
fir = 1.26d-14 * ( 2.58*fnu60 + fnu100 )
fire= sqrt( (1.26d-14*2.48*fnu60e)^2 + (1.26d-14*fnu100e)^2 )
;
; log(TIR/FIR)
x = alog10( fnu60 / fnu100 )
xe= sqrt( (fnu60*fnu100e)^2 + (fnu100*fnu60e)^2 ) / (fnu60/fnu100)
a = [ 0.2738, -0.0282, 0.7281, 0.6208, 0.9118 ]
ltf = a(0) + a(1) * x + a(2) * x^2 + a(3) * x^3 + a(4) * x^4
ltfe = xe * (a(1) + 2.*a(2)*x + 3.*a(3)*x^2 + 4.*a(4)*x^3)
;
; TIR W m^(-2)
tir = fir * 10.d0^ltf
tire = sqrt( fire^2 + ((tir/fir)*alog(10.)*ltfe)^2 )
;
; get FUV flux (janskys)
fnufuv	= 3631.d0 * 10.^(-0.4*galsam.fuv_int_mag)
fnufuve	= fnufuv * galsam.fuv_int_magerr / 1.0857362d0
;
; convert to nuFnu W m^(-2)
nufnufuv = !PHYS_C/(1516.d0*1.d-13) * ( fnufuv * 1.d-26 )
nufnufuve= !PHYS_C/(1516.d0*1.d-13) * ( fnufuve * 1.d-26 )
;
; IRX
irx = tir / nufnufuv
irxe= sqrt( (tir*nufnufuve)^2 + (nufnufuv*tire)^2 )
;
; beta
bet  = galsam.fuv_int_mag - galsam.nuv_int_mag
bete = sqrt(galsam.fuv_int_magerr^2 + galsam.nuv_int_magerr^2)
;
return
end
