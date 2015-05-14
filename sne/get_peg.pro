pro get_peg,infcat,agel,agel_m,agel_p,mstar,mstar_m,mstar_p, $
	sfr,sfr_m,sfr_p,ssfr,ssfr_m,ssfr_p,ebv,chi2,name,nbnd,noclean=noclean, $
	count=count, silent=silent, range_errors=range_errors, $
	sfrlims = sfrlims, ssfrlims = ssfrlims
;+
; get_peg - return pegase data
;-
; check struct
t=where(strpos(tag_names(infcat),'SEDFITS') ge 0, nt)
if nt ge 1 then $
	zpeg = infcat.sedfits $
else	zpeg = infcat
;
; pull out pegase data
if keyword_set(noclean) then begin
	nsn=n_elements(zpeg)
	g=lindgen(nsn)
endif else	g=get_good_peg(zpeg,count=nsn)
count=nsn
;
; limits
age_llm  = 1.
age_ulm  = 1.37e10
mstar_llm= 1.
mstar_ulm= 13.
if keyword_set(sfrlims) then begin
	sfr_llm = sfrlims[0]
	sfr_ulm = sfrlims[1]
endif else begin
	sfr_llm  = -2.
	sfr_ulm  = 3.
endelse
if keyword_set(ssfrlims) then begin
	ssfr_llm = ssfrlims[0]
	ssfr_ulm = ssfrlims[1]
endif else begin
	ssfr_llm = -12.
	ssfr_ulm = -3.
endelse
;
; convert age to years
agel	= zpeg[g].zphots[0].agestars[0] * 1.e9 > age_llm < age_ulm
agel_m	= zpeg[g].zphots[0].agestars[1] * 1.e9 > age_llm
agel_p	= zpeg[g].zphots[0].agestars[2] * 1.e9 < age_ulm
mstar	= zpeg[g].zphots[0].mass[0] > mstar_llm < mstar_ulm
mstar_m	= zpeg[g].zphots[0].mass[1] > mstar_llm
mstar_p	= zpeg[g].zphots[0].mass[2] < mstar_ulm
sfr	= zpeg[g].zphots[0].sfr[0] > sfr_llm < sfr_ulm
sfr_m	= zpeg[g].zphots[0].sfr[1] > sfr_llm
sfr_p	= zpeg[g].zphots[0].sfr[2] < sfr_ulm
ebv	= zpeg[g].zphots[0].ebv
chi2	= zpeg[g].chi2
nbnd	= zpeg[g].nbands
name	= strtrim(zpeg[g].id,2)
ssfr	= (sfr - mstar) > ssfr_llm < ssfr_ulm
ssfr_m	= fltarr(nsn)
ssfr_p	= fltarr(nsn)
for i=0,nsn-1 do begin
	vec = [ (sfr_m(i)-mstar_p(i)), (sfr_m(i)-mstar_m(i)), $
		(sfr_p(i)-mstar_p(i)), (sfr_p(i)-mstar_m(i)) ]
	ssfr_m(i)  = min(vec) > ssfr_llm
	ssfr_p(i)  = max(vec) < ssfr_ulm
endfor
;
if not keyword_set(range_errors) then begin
	agel_m	= agel - agel_m
	agel_p	= agel_p - agel
	mstar_m	= mstar - mstar_m
	mstar_p = mstar_p - mstar
	sfr_m	= sfr - sfr_m
	sfr_p	= sfr_p - sfr
	ssfr_m	= ssfr - ssfr_m
	ssfr_p	= ssfr_p - ssfr
endif
;
return
end
;
