pro print_peg,infcat,id
;+
;	print_peg - print pegase info for object id
;-
; check struct
t=where(strpos(tag_names(infcat),'SEDFITS') ge 0, nt)
if nt ge 1 then $
	zpeg = infcat.sedfits $
else	zpeg = infcat
;
; find id
if n_params(0) gt 1 then begin
	g=where(strmatch(strtrim(zpeg.id,2),strtrim(id,2)) eq 1, ng)
	if ng lt 1 then begin
		print,'Object not found: ',id
		return
	endif
	if ng gt 1 then begin
		print,'Object ambiguous: ',id
		return
	endif
endif else g=get_good_peg(infcat,count=ng)
;
agel	= alog10(zpeg[g].zphots[0].agestars[0] * 1.e9)
agel_m	= alog10(zpeg[g].zphots[0].agestars[1] * 1.e9)
agel_p	= alog10(zpeg[g].zphots[0].agestars[2] * 1.e9)
mstar	= zpeg[g].zphots[0].mass[0]
mstar_m	= zpeg[g].zphots[0].mass[1]
mstar_p	= zpeg[g].zphots[0].mass[2]
sfr	= zpeg[g].zphots[0].sfr[0]
sfr_m	= zpeg[g].zphots[0].sfr[1]
sfr_p	= zpeg[g].zphots[0].sfr[2]
ebv	= zpeg[g].zphots[0].ebv
chi2	= zpeg[g].chi2
name	= zpeg[g].id
zspc	= zpeg[g].xpix
;
print,'ID        zspc  lgAge lgAge- lgAge+  lgM*   lgM*-  lgM*+  lgSFR lgSFR- lgSFR+   EBmV   Chi2'
for i=0,ng-1 do begin
	print,name[i],zspc[i],agel[i],agel_m[i],agel_p[i], $
		mstar[i],mstar_m[i],mstar_p[i],sfr[i],sfr_m[i],sfr_p[i], $
		ebv[i],chi2[i], format='(a-7,f7.3,11f7.2)'
endfor
;
return
end
;
