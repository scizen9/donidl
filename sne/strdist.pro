pro strdist,ps=ps,table=table
;
; plot stretch distribution and compare observed with total sample
;
common lowz_sne_info
;
; set up plot
font_store=!p.font
if keyword_set(ps) then begin
	psfile,'strdist'
	!p.font=1
	th=4
	si=2.5
endif else begin
	th=2
	si=2.7
endelse
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
bs=0.1
xrng=[0.4,1.4]
sam=where(sndat.smpl_ustr eq 1, nsam)
snsam=sndat(sam)
plothist,snsam.smpl_str,bin=bs,charsi=si,charth=th,thick=th,xran=xrng, $
	xthick=th,ythick=th,xtitle='STRETCH', ytitle='N',/halfbin
;
; get observed sample
o=where(snsam.hglga_uv_proc ge 2 and snsam.hglga_sdss_proc ge 2, no)
;o=where(snsam.hfuv_int_mag gt 0., no)
;
plothist,snsam(o).smpl_str,bin=bs,/fill,/overplot,/halfbin
;
legend,['OPEN (TOTAL): '+strn(nsam),'FILLED (OBSERVED): '+strn(no)], $
	charsi=1.8,charthi=th, box=0
;
; cz distribution
q=''
if keyword_set(ps) then begin
	psclose
	psfile,'czdist'
endif else read,'<cr> ',q
cz=snsam.cz > 100.0
lcz = alog10(cz)
bs=0.3
plothist,lcz,bin=bs,charsi=si,charth=th,thick=th, $
	xthick=th,ythick=th,xtitle=textoidl('LOG_{10} cz (km s^{-1})'), $
	ytitle='N',/halfbin
cz=snsam(o).cz > 100.0
lcz = alog10(cz)
plothist,lcz,bin=bs,/fill,/overplot,/halfbin
;
legend,['OPEN (TOTAL): '+strn(nsam),'FILLED (OBSERVED): '+strn(no)], $
	charsi=1.8,charthi=th, box=0,/right
;
; Spirals
if keyword_set(ps) then begin
	psclose
	psfile,'strlate'
endif else read,'<cr> ',q
bs=0.1
l=where(snsam.htyn gt 0 and snsam.htyn lt 12,nl)
plothist,snsam(l).smpl_str,bin=bs,charsi=si,charth=th,thick=th,xran=xrng, $
	xthick=th,ythick=th,xtitle='STRETCH', ytitle='N',/halfbin, $
	title='SPIRALS'
lo=where(snsam.hfuv_int_mag gt 0. and snsam.htyn gt 0 and snsam.htyn lt 12,nlo)
plothist,snsam(lo).smpl_str,bin=bs,/fill,/overplot,/halfbin
legend,['OPEN (TOTAL): '+strn(nl),'FILLED (OBSERVED): '+strn(nlo)], $
	charsi=1.8,charthi=th, box=0
;
; Ellipticals
if keyword_set(ps) then begin
	psclose
	psfile,'strearly'
endif else read,'<cr> ',q
yrng=[0,16]
e=where(snsam.htyn le 0,nel)
plothist,snsam(e).smpl_str,bin=bs,charsi=si,charth=th,thick=th,xran=xrng, $
	xthick=th,ythick=th,xtitle='STRETCH', ytitle='N',/halfbin,yran=yrng, $
	ysty=1,title='ELLIPTICALS'
elo=where(snsam.hfuv_int_mag gt 0. and snsam.htyn le 0,nelo)
plothist,snsam(elo).smpl_str,bin=bs,/fill,/overplot,/halfbin
legend,['OPEN (TOTAL): '+strn(nel),'FILLED (OBSERVED): '+strn(nelo)], $
	charsi=1.8,charthi=th, box=0
;
if keyword_set(ps) then psclose
!p.font=font_store
;
if keyword_set(table) then begin
	tfil=table
	filestamp,tfil
	openw,ol,tfil,/get_lun
	printf,ol,'#STRDIST: '+systime(0)
	printf,ol,'#ALL SNe with fitted stretch'
	printf,ol,'#SN         STR   B-V     FUV      NUV       V        g        K       60mu       cz   NED GLGA SDSS  RC3  NIR  Host'
	for i=0,nsam-1 do begin
		printf,ol,snsam(i).id, snsam(i).smpl_str, snsam(i).smpl_clr, $
			snsam(i).hfuv_int_mag, snsam(i).hnuv_int_mag, $
			snsam(i).hVJ_int_mag, snsam(i).hg_int_mag, $
			snsam(i).hK_int_mag, $
			snsam(i).h60m_int_mag, snsam(i).cz, $
			snsam(i).hned_gal, snsam(i).hglga_uv_proc, $
			snsam(i).hglga_sdss_proc, snsam(i).hvj_int_src, $
			snsam(i).hk_int_src, snsam(i).host, $
		format='(a-8, 2f7.2, 6f9.3, f9.1, 5i5, 2x,a-15)'
	endfor
	free_lun,ol
endif
;
return
end
