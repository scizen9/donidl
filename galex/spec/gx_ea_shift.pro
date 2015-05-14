pro gx_ea_shift,std,gspf,sid
;+
; GX_EA_SHIFT	- shift effective area curve and re-calibrate fluxes
;
; USAGE:
;
;	gx_ea_shift, std_name, gsp_fits_file, specid_in_gsp_file
;
; INPUTS:
;
;	std	- name of the standard star (e.g. 'hz21')
;	gspf	- name of the *-xg-gsp.fits file (e.g. 'WDST_HZ_21_0001-xg-gsp.fits'
;	specid	- id number to find in gsp file (e.g. 4581)
;
; OUTPUTS:
;
;	None
;
; KEYWORDS:
;
;	None
;
; HISTORY:
;
;	03oct07 jdn	- Initial revision
;-
;
if n_params(0) lt 3 then begin
	print,'gx_ea_shift, std_name, gsp_file, spec_id'
	return
endif
;
gx_read_gsp,gspf,sid,flx,flxe,w
gx_read_ea,fea,fw,nea,nw
fuv = where(w gt 1290 and w lt 1780)
nuv = where(w gt 1840 and w lt 3000)
fres = interpol(fea,fw,w)
nres = interpol(nea,nw,w)
fcnt = flx * fres
ncnt = flx * nres
;
; get standard flux
sfil = '~/ref/calib/galex/spec/'+std+'_flux.tbl'
if file_test(sfil) then begin
	readcol,sfil,sw,sf,sfe,sfn,s2n,form='f,f,f,f,f',/silent
endif else begin
	print,'No reference flux file found for: ',std
	return
endelse
;
; where are we above s2n of 5?
ngo = where(s2n(nuv) gt 5.,nngo)
fgo = where(s2n(fuv) gt 5.,nfgo)
;
; re-set wavelength ranges
nwl = [w(nuv(ngo(0))), w(nuv(ngo(nngo-1)))]
fwl = [w(fuv(fgo(0))), w(fuv(fgo(nfgo-1)))]
nuv = where(w gt nwl(0) and w lt nwl(1))
fuv = where(w gt fwl(0) and w lt fwl(1))
;
; get flux range
mxflx = max(sf([fuv,nuv]))
mxflx = mxflx + mxflx * 0.45
yrng = [mxflx*(-0.05),mxflx]
ydel = yrng(1) - yrng(0)

;
font_save=!p.font
th=3
si=1.5
deepcolor
if !d.name eq 'PS' then begin
	!p.background=colordex('white')
	!p.color=colordex('black')
	!p.font=1
endif else begin
	!p.background=colordex('black')
	!p.color=colordex('white')
endelse
cols = [colordex('P'),colordex('B')]    ; FUV, NUV
;
s=-1.0
;
done = (1 eq 0)
while not done do begin
    read,'Shift? (0 - quit): ',s
    if s ne 0 then begin
	sfres=interpol(fea,fw,w+s)
	snres=interpol(nea,nw,w+s)
	sfflx=fcnt/sfres
	snflx=ncnt/snres
;	sfcnt=shift(fcnt,s)
;	sncnt=shift(ncnt,s)
;	sfflx=sfcnt/fres
;	snflx=sncnt/nres
	plot,w(fuv),flx(fuv),charsi=si,charthi=th, $
	    title=gspf+'   EA Shift: '+string(s,form='(f6.1)'), $
	    xtitle='WAVELENGTH (ANG)',xran=[1250,3050],xsty=1,xthick=th, $
	    ytitle='FLUX (pho/sec/cm!U2!N/Ang)', yran=yrng,ysty=1,ythick=th
	oplot,w(nuv),flx(nuv)
	oplot,w(fuv),sfflx(fuv),thick=th,color=colordex('orange')
	oplot,w(nuv),snflx(nuv),thick=th,color=colordex('orange')
	oplot,sw,s2n/10000.,color=colordex('A'),thick=th
	oplot,sw,sfn,color=colordex('green')
	oplot,sw,sf,color=colordex('red'),thick=th
	oplot,[nwl(0),nwl(0)],[-100,100],thick=th,color=cols(1)
	oplot,[nwl(1),nwl(1)],[-100,100],thick=th,color=cols(1)
	oplot,[fwl(0),fwl(0)],[-100,100],thick=th,color=cols(0)
	oplot,[fwl(1),fwl(1)],[-100,100],thick=th,color=cols(0)
	frat = sfflx/sf
	nrat = snflx/sf
	mystats,frat(fuv),fmn,fsg
	mystats,nrat(nuv),nmn,nsg
	xyouts,1345,0,'<FUV> = '+string(fmn*100.,form='(f5.1)')+' +- '+ $
		string(fsg*100.,form='(f5.1)')+' %',charsi=li,color=cols(0)
	xyouts,2250,0,'<NUV> = '+string(nmn*100.,form='(f5.1)')+' +- '+ $
		string(nsg*100.,form='(f5.1)')+' %',charsi=li,color=cols(1)
	legend,['STIS','STIS+Nse','GALEX','(S/N)/1.E5','SHIFTED'], $
		linesty=[0,0,0,0,0],$
		thick=[th,1,1,th,th],charthi=th, charsi=li, $
		color=[colordex('red'),colordex('green'),!p.color,$
		colordex('A'),colordex('orange')], box=0, $
		/clear,clr_color=!p.background,pos=[2620,yrng(1)-ydel*0.03]
    endif else done = (1 eq 1)
endwhile
;
!p.font=font_save
;
return
end
