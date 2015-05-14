pro samplt,sam,limleg=limleg,ps=ps,title=title,table=table
;
; plot sample properties
;
; get sn data
COMMON lowz_sne_info
;
; check inputs
if n_params(0) lt 1 then begin
	print,'samplt,sam,limleg=limleg,/ps'
	return
endif
;
; get host data
fl=!NGA_DATA+'hostdb.dat'
readcol,fl,host,fuv,fuve,nuv,nuve,bm,bme,km,kme,ty,tye,ebmv,mu,dmpc, $
	form='a,f,f,f,f,f,f,f,f,f,f,f,f,f',/silent
nnga = n_elements(fuv)
mkm=km-mu-(ebmv*0.367)
ells=where(ty lt -0.5, nells)
dsks=where(ty ge -0.5 and ty le 9.5, ndsks)
irrs=where(ty gt 9.5 and ty le 12.0, nirrs)
unks=where(ty gt 12.0, nunks)
;
; check keywords
if keyword_set(title) then $
	tlab=title $
else	tlab=''
;
; output file
psf = 'sam'
snsam=sndat(sam)
nsam=n_elements(sam)
;
; host types
sells=where(snsam.htyn lt -0.5 and snsam.htyn gt -9.9, nsells)
sdsks=where(snsam.htyn ge -0.5 and snsam.htyn le 9.5, nsdsks)
sirrs=where(snsam.htyn gt 9.5 and snsam.htyn le 12., nsirrs)
sunks=where(snsam.htyn gt 12.0 or snsam.htyn le -9.9, nsunks)
;
; set up plot
font_store=!p.font
if keyword_set(ps) then begin
	psfile,psf
	print,'Plotting to: '+psf+'.ps'
	!p.font=1
	th=4
	si=1.5
	li=1.2
	ss=1.2
endif else begin
	print,'Plot: '+psf
	th=2
	si=2.7
	li=2.0
	ss=2.0
endelse
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
a=[findgen(40)*(!pi*2/40.),0.]
usersym,cos(a),sin(a),thick=th,/fill
q=''
;
; plot mu distribution
!p.multi=[0,1,2]
bins=0.5
bmin=20
bmax=40
xrng = [22,39]
yrng = [-1,150]
xlb = textoidl('(m - M)_0')
ylb = 'N'
tlab = strn(nsam) + ' ' + tlab
;
; plot sample galaxies
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=xlb, xran=xrng,xsty=1, $
	ytitle=ylb, yran=yrng,ysty=1,title=tlab
h=histogram(snsam.hmu,min=bmin,max=bmax,binsi=bins,loc=xh)
oplot,xh,h,psym=10,thick=th,color=colordex('orange')
if nsells gt 1 then begin
	hell=histogram(snsam(sells).hmu,min=bmin,max=bmax,binsi=bins,loc=xhell)
	oplot,xhell,hell,psym=10,thick=th,color=colordex('red')
endif
if nsdsks gt 1 then begin
	hdsk=histogram(snsam(sdsks).hmu,min=bmin,max=bmax,binsi=bins,loc=xhdsk)
	oplot,xhdsk,hdsk,psym=10,thick=th,color=colordex('green')
endif
if nsirrs gt 1 then begin
	hirr=histogram(snsam(sirrs).hmu,min=bmin,max=bmax,binsi=bins,loc=xhirr)
	oplot,xhirr,hirr,psym=10,thick=th,color=colordex('blue')
endif
if nsunks gt 1 then begin
	hunk=histogram(snsam(sunks).hmu,min=bmin,max=bmax,binsi=bins,loc=xhunk)
	oplot,xhunk,hunk,psym=10,thick=th,color=colordex('black')
endif
;
; limits legend
if keyword_set(limleg) then $
	legend,textoidl(limleg),charthi=th,box=0,charsi=li,/right
;
; histogram legend
legend,[textoidl('ALL: '+string(nsam,form='(i4)')), $
	textoidl('DSK: '+string(nsdsks,form='(i4)')),$
	textoidl('ELL: '+string(nsells,form='(i4)')),$
	textoidl('IRR: '+string(nsirrs,form='(i4)')), $
	textoidl('UNK: '+string(nsunks,form='(i4)'))], $
color=[colordex('O'),colordex('G'),colordex('R'),colordex('B'),colordex('C')], $
	linesty=[0,0,0,0,0],charthi=th,box=0,charsi=li,thick=[th,th,th,th,th]
;
; NGA galaxies
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=xlb, xran=xrng,xsty=1, $
	ytitle=ylb, yran=yrng,ysty=1,title='GALEX NGA'
hall=histogram(mu,min=bmin,max=bmax,binsi=bins,loc=xhall)
oplot,xhall,hall,psym=10,thick=th,color=colordex('orange');,linesty=2
if nells gt 0 then begin
	hell=histogram(mu(ells),min=bmin,max=bmax,binsi=bins,loc=xhell)
	oplot,xhell,hell,psym=10,thick=th,color=colordex('red');,linesty=2
endif
if ndsks gt 0 then begin
	hdsk=histogram(mu(dsks),min=bmin,max=bmax,binsi=bins,loc=xhdsk)
	oplot,xhdsk,hdsk,psym=10,thick=th,color=colordex('green');,linesty=2
endif
if nirrs gt 0 then begin
	hirr=histogram(mu(irrs),min=bmin,max=bmax,binsi=bins,loc=xhirr)
	oplot,xhirr,hirr,psym=10,thick=th,color=colordex('blue');,linesty=2
endif
if nunks gt 0 then begin
	hunk=histogram(mu(unks),min=bmin,max=bmax,binsi=bins,loc=xhunk)
	oplot,xhunk,hunk,psym=10,thick=th,color=colordex('black');,linesty=2
endif
;
; histogram legend
legend,[textoidl('ALL: '+string(nnga,form='(i4)')), $
	textoidl('DSK: '+string(ndsks,form='(i4)')),$
	textoidl('ELL: '+string(nells,form='(i4)')),$
	textoidl('IRR: '+string(nirrs,form='(i4)')), $
	textoidl('UNK: '+string(nunks,form='(i4)'))], $
color=[colordex('O'),colordex('G'),colordex('R'),colordex('B'),colordex('C')], $
	linesty=[0,0,0,0,0],charthi=th,box=0,charsi=li,thick=[th,th,th,th,th]
;
; next plot
if not keyword_set(ps) then $
	read,'eh? ',q
;
; plot SN type distribution
tylabs = [ $
	'Ia', $
	'Ib', $
	'Ib/c', $
	'Ic', $
	'IIn', $
	'IIb', $
	'IIL', $
	'IIP', $
	'II' $
	]
clrs = [colordex('black'), $
	colordex('orange'), $
	colordex('orange'), $
	colordex('orange'), $
	colordex('blue'), $
	colordex('blue'), $
	colordex('blue'), $
	colordex('blue'), $
	colordex('blue')]
!p.multi=0
bins=1.0
bmin=1
bmax=9
xlb = textoidl('SN TYPE')
ylb = 'N'
;
; plot sample galaxies
h=histogram(snsam.tyn,min=bmin,max=bmax,binsi=bins,loc=xh)
!p.charsize=si
!p.charthick=th
!p.thick=th
bar_plot,h,barnames=tylabs,xtitle=xlb,ytitle=ylb,title=tlab, $
	backgr=colordex('white'),colors=clrs,/outline
if keyword_set(ps) then psclose
;
!p.font=font_store
!p.charsize=0
!p.charthick=0
!p.thick=0
;
; output table if requested
if keyword_set(table) then begin
	if strlen(table) le 1 then $
		tfil='table.txt' $
	else	tfil=table
	filestamp,tfil
	openw,ol,tfil,/get_lun
	printf,ol,'# SN      Type            cz   Host            HType     D25"    FUVi    FUVie   NUVi    NUVie   FUVap   FUVae   NUVap   NUVae'
;
; loop over types
	for j=0,9 do begin
;
; loop over sample
	for i=0,nsam-1 do begin
	    if snsam(i).tyn eq j then $
		printf,ol,snsam(i).id,snsam(i).type,snsam(i).cz, $
			snsam(i).host,snsam(i).htype,snsam(i).hd25, $
			snsam(i).hfuv_int_mag,snsam(i).hfuv_int_magerr, $
			snsam(i).hnuv_int_mag,snsam(i).hnuv_int_magerr, $
			snsam(i).hfuv_ap_mag,snsam(i).hfuv_ap_magerr, $
			snsam(i).hnuv_ap_mag,snsam(i).hnuv_ap_magerr, $
			format='(2a-10,f9.1,2x,a-16,a-8,f7.1,8f8.2)'
	endfor
	endfor
	free_lun,ol
endif
;
return
end
