pro lcur_plt_mags, finder=finder
;
if ( lcur_com_get('ntpts') le 0 ) then begin
	print,'LCUR_PLT_MAGS: ERROR, no data read in yet'
	return
endif
;
twin_orig = lcur_com_get('twin')
;
ajd = lcur_com_get('jd')
img = lcur_com_get('mags')
ime = lcur_com_get('merrs')
erl = lcur_com_get('merrlim')
;
good = where(ime le erl and img lt 30.0, ngood)
if ngood gt 1 then begin
	img = img(good)
	ime = ime(good)
endif
mystats,img,vmean
ymxv = max(img+ime)
ymnv = min(img-ime)
vran = max([1.0,max([(ymxv-vmean),(vmean-ymnv)])*2.3])
ylimits = [vmean + vran*0.5, vmean - vran*0.5]
;
; get xrange and relevant julian days
;
ljd = long( ajd )
frac= ajd - ljd
ljd = ljd ( sort( ljd ) )
ljd = ljd ( uniq( ljd ) )
njd = n_elements( ljd )
if njd lt 5 then begin
    xran = max(frac) - min(frac)
    xlims = [(min(frac) - xran*0.1), (max(frac) + xran*0.1)]
endif else $
    xlims = [(min(ljd)-2.d0), (max(ljd)+2.d0)]
;
if njd lt 5 then begin
    !p.multi = [ 0, njd, 1 ]
;
; loop over days
;
    for i = 0, njd-1 do begin
;
; get relevant points
	t = where( long(ajd) eq ljd(i))
	sjd = ajd(t)
	lcur_set_twin, min(sjd), max(sjd), /noplot
	if keyword_set(finder) then $
		lcur_plt_phot, ylimits, xlims+ljd(i), finder=i+1 $
	else	lcur_plt_phot, ylimits, xlims+ljd(i)
    endfor
;
    !p.multi=0
endif else begin
    if keyword_set(finder) then $
		lcur_plt_phot, ylimits, xlims, finder=finder $
	else	lcur_plt_phot, ylimits, xlims
endelse
;
lcur_set_twin, twin_orig(0), twin_orig(1), /noplot

;
return
end
