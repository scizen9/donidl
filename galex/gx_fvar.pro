function gx_fvar,mg,mge,fuv=fuv,nuv=nuv,avmg=avmg,sgmg=sgmg,mgran=mgran
;+
; gx_fvar - calculate variability for galex mags
;-
nmg = n_elements(mg)
;
if keyword_set(fuv) then $
	zp = 18.82 $
else	zp = 20.08
if nmg gt 1 then begin
	fm = 10.0^(-0.4*(mg-zp))	; flux
	fme = mge * fm / 1.0857
	wf = 1./fme^2
	fmav = total(fm*wf)/total(wf)
	del = abs(fm-fmav)
	wf = wf / ( 1. + (del/2.)^2 )
	fmav = total(fm*wf)/total(wf)
	s2 = total( (fm - fmav)^2 ) / float(nmg)
	d2 = total( fme^2 ) / float(nmg)
	fvar = sqrt( (s2 - d2)>0. ) / fmav
	avmg = -2.5*alog10(fmav) + zp
	sgmg = (sqrt(s2) / fmav) * 1.0857
	mgran = max(mg) - min(mg)
endif else begin
	avmg = mg[0]
	sgmg = mge[0]
	fvar = -1.
	mgran= 0.
endelse
;
return,fvar
end
