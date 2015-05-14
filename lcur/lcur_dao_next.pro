pro lcur_dao_next,inext,previous=previous
;
COMMON dao_data
;
inc = 1L
if keyword_set(previous) then inc = -1L

do_thresh = (dao_varth(0) gt 0. or dao_varth(1) gt 0.)

if n_params(0) lt 1 then $
	inext = dao_cid + inc

if do_thresh then begin
	p = where(dao_var ge dao_varth(0) and dao_var le dao_varth(1) and $
		  dao_enframes ge dao_nflim and dao_rnei ge dao_neith, nfound)
	if inc gt 0 then begin
		diff = dao_id(p) - inext > (-1)
		z = where(diff eq -1, nfound)
		if nfound gt 0 then diff(z) = 1000000L
		t = where(diff eq min(diff), nfound)
	endif else begin
		diff = dao_id(p) - inext < 1
		z = where(diff eq 1, nfound)
		if nfound gt 0 then diff(z) = -1000000L
		t = where(diff eq max(diff), nfound)
	endelse
	if nfound ne 1 then begin
	    print,'lcur_dao_next: cannot increment anymore in this direction'
	    return
	endif
	p = p(t(0))
	inext = dao_id(p)
	nfound = 1
endif else begin
	if inc gt 0 then begin
		p = where(dao_id ge inext and dao_rnei ge dao_neith and $
			  dao_enframes ge dao_nflim, nfound)
		diff = dao_id(p) - inext > (-1)
		z = where(diff eq -1, nfound)
		if nfound gt 0 then diff(z) = 1000000L
		t = where(diff eq min(diff), nfound)
	endif else begin
		p = where(dao_id le inext and dao_rnei ge dao_neith, nfound)
		diff = dao_id(p) - inext < 1
		z = where(diff eq 1, nfound)
		if nfound gt 0 then diff(z) = -1000000L
		t = where(diff eq max(diff), nfound)
	endelse
	if nfound le 0 then begin
	    print,'lcur_dao_next: cannot increment anymore in this direction'
	    return
	endif
	p = p(t(0))
	inext = dao_id(p)
	nfound = 1
endelse

if nfound ne 1 then begin
	print,'lcur_dao_next: ERROR - no good id found, id, nfound: ', $
		inext,nfound
	return
endif

dao_cid = inext
;print,'lcur_dao_next: Found id; ',strn(dao_cid)

p = p(0)

dao_cx	= dao_x(p)
dao_cy	= dao_y(p)
dao_cvar= dao_var(p)
dao_cvaro= dao_varo(p)

nframes	= dao_nframes(p)
chi	= dao_chi(p)
sharp	= dao_sharp(p)
blunder	= dao_blunder(p)

print,dao_cid,': x,y,nf,ch,sh,vr,blnd= ', $
	dao_cx,dao_cy,nframes, chi, sharp, dao_cvar, blunder, $
	format='(i6,a,2f8.2,i4,f6.2,f6.2,f7.2,f6.2)'

imgs	= reform( dao_mags(0:(dao_npts-1),p) )
imes	= reform( dao_merrs(0:(dao_npts-1),p) )

ret	= lcur_com_put('mags', imgs)
ret	= lcur_com_put('merrs', imes)

ret	= lcur_com_put('phinc', 0.001)	; reset phinc to default
;ret	= lcur_com_put('merrlim', 0.05)	; reset merrlim to default

lcur_scar_get

;lcur_plt_mags
lcur_plt_phot

lcur_dao_zoom

;
return
end	; pro lcur_data_get
