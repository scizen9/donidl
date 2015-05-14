pro lcur_plt_phot, ylimits, ixlimits, noannotate = noannotate, finder=finder
;
if ( lcur_com_get('ntpts') le 0 ) then begin
	print,'LCUR_PLT_PHOT: ERROR, no data read in yet'
	return
endif
psarr = [4, 5, 2, 6, 1, 2 ]	; psym value array
;
yblnk = [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']
;
ijd = lcur_com_twin('jd')
ien = lcur_com_twin('epnum')
img = lcur_com_twin('mags')
ime = lcur_com_twin('merrs')
erl = lcur_com_get('merrlim')
;
if not keyword_set(noannotate) then $
	do_annote = (lcur_com_get('dao_imgno') ge 0) $
else	do_annote = ( 1 eq 0 )
;
; get good points
good = where(ime le erl and img lt 30.0, ngood)
if ngood lt 2 then begin
	print,'LCUR_PLT_PHOT: ERROR, error limit too strict; only ', $
		strn(ngood), ' points left.'
endif else begin
	ijd = ijd(good)
	ien = ien(good)
	img = img(good)
	ime = ime(good)
endelse
;
; get epoch data
eps = ien
eps = eps(sort(eps))
eps = eps(uniq(eps))
neps = n_elements(eps)
;
; plot data points
;
mystats,img,vmean,vsig,vkurt
averr = mean(ime)
delmg = max(img) - min(img)
if n_params(0) lt 1 then begin
	ymxv = max(img+ime)+2.*max(ime)
	ymnv = min(img-ime)-2.*max(ime)
;	vran = ymxv - ymnv
	vran = max([0.5,max([(ymxv-vmean),(vmean-ymnv)])*2.3])
	ymxv = vmean + vran*0.5
	ymnv = vmean - vran*0.5
	ylimits = [ymxv, ymnv]
endif else begin
	vran = max(ylimits) - min(ylimits)
	ymnv = min(ylimits)
endelse
;
; x limits
zd = lcur_com_get('jdzero')
delx = ( max(ijd) - min(ijd) ) / 10.0
if n_params(0) lt 2 then $
	xlimits = [min(ijd)-delx, max(ijd)+delx] - zd $
else	xlimits = ixlimits - zd
;
if do_annote then begin
	pfile= lcur_com_get('pfile')
	tstr = pfile+'  '+ systime(0)
endif else tstr = ''
;
cid  = lcur_com_get('dao_cid')
if cid gt 0 then $
	tstr = 'ID: '+strn(cid)+' '+tstr
;
if do_annote then $
	xlab = 'RJD - '+strtrim(zd,2) $
else	xlab = 'RJD'
;
filt = lcur_com_get('filt')
ylab = filt + ' MAG'
if ngood lt 2 then begin
	pp = 7 
	tstr = tstr + ' BAD'
endif else begin
	pp = 3
endelse

pfile = lcur_com_get('pfile')
root = gettok(pfile,'.')

if not keyword_set(finder) then begin
    plot,ijd-zd,img,psym=pp,yran=ylimits,xran=xlimits,xsty=1,ysty=1,$
	thick=3,charsize=2.0,ytitle=ylab,xtitle=xlab, $
	title=tstr,xthick=2.0,ythick=2.0,charthick=1.5;,font=1
    ntt=0L
    for i=0,neps-1 do begin
        t=where(ien eq eps(i), nt)
	if nt gt 0 then $
	    oplot,ijd(t)-zd, img(t), psym=psarr(eps(i)<5), thick=3, symsiz=1.0
        ntt = ntt+nt
    endfor
    errplot,ijd-zd,img-ime,img+ime
    oplot,xlimits,[vmean,vmean]
    oplot,xlimits,[vmean+vsig,vmean+vsig],linesty=3
    oplot,xlimits,[vmean-vsig,vmean-vsig],linesty=3
    if do_annote then $
	xyouts,xlimits(0)+(xlimits(1)-xlimits(0))*0.1,ymnv+0.95*vran,$
		'Npts: '+string(ntt,form='(i3)')+ $
		' Mean: '+string(vmean,form='(f6.2)')+ $
		' Del: '+string(delmg,form='(f6.2)')+ $
	' Sig: '+string(vsig,form='(f6.2)'),charthick=1.5,charsize=2.0,font=1
    print,'Mn, del, sig, averr, rat: ',vmean,delmg,vsig,averr,vsig/averr,form='(a,5f9.3)'
    if cid gt 0 and do_annote then begin
	cx = lcur_com_get('dao_cx')
	cy = lcur_com_get('dao_cy')
	cvar = lcur_com_get('dao_cvar')
	xyouts,xlimits(0)+(xlimits(1)-xlimits(0))*0.1,ymnv+0.07*vran,$
	'Var: '+string(cvar,form='(f6.2)')+ $
	'   X,Y: '+string(cx,form='(f8.2)') $
	        +string(cy,form='(f8.2)'),charthick=1.5,charsize=2.0,font=1
    endif
endif else begin
    case finder of
    1: plot,ijd-zd,img,psym=pp,yran=ylimits,xran=xlimits,xsty=1,ysty=1,$
	thick=3,charsize=2.0,ytitle=ylab, $
	position=[0.05, 0.6, 0.33, 0.99], $
	xthick=2.0,ythick=2.0,charthick=1.5; , xtickform='(f4.1)',xticks=3
    2: plot,ijd-zd,img,psym=pp,yran=ylimits,xran=xlimits,xsty=1,ysty=1,$
	thick=3,charsize=2.0,xtitle=xlab, /noerase, $
	position=[0.38, 0.6, 0.66, 0.99], $
	title=root+':'+string(cid,form='(i6)'), $
	xthick=2.0,ythick=2.0,charthick=1.5; , xtickform='(f4.1)',xticks=3
    3: plot,ijd-zd,img,psym=pp,yran=ylimits,xran=xlimits,xsty=1,ysty=1,$
	thick=3,charsize=2.0, /noerase, $
	position=[0.71, 0.6, 0.99, 0.99], $
	xthick=2.0,ythick=2.0,charthick=1.5; , xtickform='(f4.1)',xticks=3
    10: plot,ijd-zd,img,psym=pp,yran=ylimits,xran=xlimits,xsty=1,ysty=1,$
	thick=3,charsize=1.5,ytitle=ylab, xtitle=xlab, $
	position=[0.05, 0.6, 0.99, 0.99], $
	title=root+':'+string(cid,form='(i6)'), $
	xthick=2.0,ythick=2.0,charthick=1.5; , xtickform='(f4.1)',xticks=3
    11: plot,ijd-zd,img,psym=pp,yran=ylimits,xran=xlimits,xsty=1,ysty=1,$
	thick=3,charsize=1.5,ytitle=ylab, xtitle=xlab, $
	title=root+':'+string(cid,form='(i6)'), $
	xthick=2.0,ythick=2.0,charthick=1.5; , xtickform='(f4.1)',xticks=3
    endcase

    ntt=0L
    for i=0,neps-1 do begin
        t=where(ien eq eps(i), nt)
	if nt gt 0 then $
	    oplot,ijd(t)-zd, img(t), psym=psarr(eps(i)<5), thick=3, symsiz=1.0
        ntt=ntt+nt
    endfor
    errplot,ijd-zd,img-ime,img+ime
    oplot,xlimits,[vmean,vmean]
    oplot,xlimits,[vmean+vsig,vmean+vsig],linesty=3
    oplot,xlimits,[vmean-vsig,vmean-vsig],linesty=3
    xyouts,xlimits(0)+(xlimits(1)-xlimits(0))*0.1,ymnv+0.1*vran,$
		'Mn: '+string(vmean,form='(f4.1)')+ $
	'  s: '+string(vsig,form='(f4.2)'),charthick=1.5,charsize=2.0,font=1
    xyouts,xlimits(0)+(xlimits(1)-xlimits(0))*0.05,ymnv+0.95*vran,$
		's/AvErr: '+string(vsig/averr,form='(f5.1)')+ $
	    	'  Np: '+string(ntt,form='(i3)'), $
		charthick=1.5,charsize=2.0,font=1
endelse
;
return
end
