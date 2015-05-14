pro gx_comp_anal,ps=ps
g=g
;+
;	gx_comp_anal - analyze standard star comparisons
;-
; get list of files to plot
flist=file_search('*/*comp.dat',count=nf)
if nf eq 0 then begin
	top = (1 eq 1)
	tlab = 'All Standards'
	flist=file_search('*/*/*comp.dat',count=nf)
	if nf le 0 then begin
		print,'No *comp.dat files found.'
		return
	endif
endif else begin
	top = (1 eq 0)
	cd,'.',curr=cwd
	sta=strsplit(cwd,'/',/extract)
	tlab = strupcase(sta[n_elements(sta)-1])
endelse
;
; set up vectors
vis	= [0]
svis	= [0]
ext	= [0.]
sid	= [0L]
gra	= [0.]
xrms	= [0.]
mdrs	= [0.]
frat	= [0.]
fsig	= [0.]
nrat	= [0.]
nsig	= [0.]
;
; read in data
for i=0,nf-1 do begin
	gx_read_comp,flist[i],vs,sv,e,sd,g,x,m,fr,fs,nr,ns
	vis	=[vis,vs]
	svis	=[svis,sv]
	ext	=[ext,e]
	sid	=[sid,sd]
	gra	=[gra,g]
	xrms	=[xrms,x]
	mdrs	=[mdrs,m]
	frat	=[frat,fr]
	fsig	=[fsig,fs]
	nrat	=[nrat,nr]
	nsig	=[nsig,ns]
endfor
;
; trim vectors
vis	= vis[1:*]
svis	= svis[1:*]
ext	= ext[1:*]
sid	= sid[1:*]
gra	= gra[1:*]
xrms	= xrms[1:*]
mdrs	= mdrs[1:*]
frat	= frat[1:*]
fsig	= fsig[1:*]
nrat	= nrat[1:*]
nsig	= nsig[1:*]
;
; get good data
fg = where(frat gt 0. and fsig gt 0.)
ng = where(nrat gt 0. and nsig gt 0.)
;
; set up plot
if keyword_set(ps) then begin
	psfile,'std_comp_gra_rat'
endif
q=''
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
!p.multi=[0,1,2]
th=3
si=1.5
yrnr = [0.,200.]
yrns = [0.,100.]
;
; plot versus gra
xrn	= [-10,370.]
;
; ratio
plot,gra,frat,charsi=si,charthi=th,title=tlab, $
	xran=xrn,xtitle='GRA(degrees)',xthick=th,xsty=1, $
	yran=yrnr,ytitle='RATIO(%)', ythick=th,ysty=1, $
	/nodata
oplot,[-1.e9,1.e9],[100.,100.],linesty=2,thick=th
oplot,gra(fg),frat(fg),psym=4,color=colordex('V'),thick=th,symsi=si
errplot,gra(fg),frat(fg)-fsig(fg),frat(fg)+fsig(fg),color=colordex('V')
plot,gra(ng),nrat(ng),charsi=si,charthi=th, title=tlab, $
	xran=xrn,xtitle='GRA(degrees)',xthick=th,xsty=1, $
	yran=yrnr,ytitle='RATIO(%)', ythick=th,ysty=1, $
	/nodata
oplot,[-1.e9,1.e9],[100.,100.],linesty=2,thick=th
oplot,gra(ng),nrat(ng),psym=5,color=colordex('B'),thick=th,symsi=si
errplot,gra(ng),nrat(ng)-nsig(ng),nrat(ng)+nsig(ng),color=colordex('B')
;
if keyword_set(ps) then begin
	psclose
	psfile,'std_comp_gra_sig'
endif else read,'next: ',q
;
; sigma
plot,gra,fsig,charsi=si,charthi=th, title=tlab, $
	xran=xrn,xtitle='GRA(degrees)',xthick=th,xsty=1, $
	yran=yrns,ytitle='RMS(%)', ythick=th,ysty=1, $
	/nodata
oplot,gra(fg),fsig(fg),psym=4,color=colordex('V'),thick=th,symsi=si
plot,gra(ng),nsig(ng),charsi=si,charthi=th, title=tlab, $
	xran=xrn,xtitle='GRA(degrees)',xthick=th,xsty=1, $
	yran=yrns,ytitle='RMS(%)', ythick=th,ysty=1, $
	/nodata
oplot,gra(ng),nsig(ng),psym=5,color=colordex('B'),thick=th,symsi=si
;
if keyword_set(ps) then begin
	psclose
	psfile,'std_comp_xrms_rat'
endif else read,'next: ',q
;
; plot versus xrms
xrn	= [0,max(xrms)*1.1]
;
; ratio
plot,xrms,frat,charsi=si,charthi=th, title=tlab, $
	xran=xrn,xtitle='XRMS(arcseconds)',xthick=th,xsty=1, $
	yran=yrnr,ytitle='RATIO(%)', ythick=th,ysty=1, $
	/nodata
oplot,[-1.e9,1.e9],[100.,100.],linesty=2,thick=th
oplot,xrms(fg),frat(fg),psym=4,color=colordex('V'),thick=th,symsi=si
errplot,xrms(fg),frat(fg)-fsig(fg),frat(fg)+fsig(fg),color=colordex('V')
plot,xrms,nrat,charsi=si,charthi=th, title=tlab, $
	xran=xrn,xtitle='XRMS(arcseconds)',xthick=th,xsty=1, $
	yran=yrnr,ytitle='RATIO(%)', ythick=th,ysty=1, $
	/nodata
oplot,[-1.e9,1.e9],[100.,100.],linesty=2,thick=th
oplot,xrms(ng),nrat(ng),psym=5,color=colordex('B'),thick=th,symsi=si
errplot,xrms(ng),nrat(ng)-nsig(ng),nrat(ng)+nsig(ng),color=colordex('B')
;
if keyword_set(ps) then begin
	psclose
	psfile,'std_comp_xrms_sig'
endif else read,'next: ',q
;
; sigma
plot,xrms,fsig,charsi=si,charthi=th, title=tlab, $
	xran=xrn,xtitle='XRMS(arcseconds)',xthick=th,xsty=1, $
	yran=yrns,ytitle='RMS(%)', ythick=th,ysty=1, $
	/nodata
oplot,xrms(fg),fsig(fg),psym=4,color=colordex('V'),thick=th,symsi=si
plot,xrms,nsig,charsi=si,charthi=th, title=tlab, $
	xran=xrn,xtitle='XRMS(arcseconds)',xthick=th,xsty=1, $
	yran=yrns,ytitle='RMS(%)', ythick=th,ysty=1, $
	/nodata
oplot,xrms(ng),nsig(ng),psym=5,color=colordex('B'),thick=th,symsi=si
;
if keyword_set(ps) then begin
	psclose
	psfile,'std_comp_mdrs_rat'
endif else read,'next: ',q
;
; plot versus mdrs
xrn	= [0,max(mdrs)*1.1]
;
; ratio
plot,mdrs,frat,charsi=si,charthi=th, title=tlab, $
	xran=xrn,xtitle='MDRS(arcseconds)',xthick=th,xsty=1, $
	yran=yrnr,ytitle='RATIO(%)', ythick=th,ysty=1, $
	/nodata
oplot,[-1.e9,1.e9],[100.,100.],linesty=2,thick=th
oplot,mdrs(fg),frat(fg),psym=4,color=colordex('V'),thick=th,symsi=si
errplot,mdrs(fg),frat(fg)-fsig(fg),frat(fg)+fsig(fg),color=colordex('V')
plot,mdrs,nrat,charsi=si,charthi=th, title=tlab, $
	xran=xrn,xtitle='MDRS(arcseconds)',xthick=th,xsty=1, $
	yran=yrnr,ytitle='RATIO(%)', ythick=th,ysty=1, $
	/nodata
oplot,[-1.e9,1.e9],[100.,100.],linesty=2,thick=th
oplot,mdrs(ng),nrat(ng),psym=5,color=colordex('B'),thick=th,symsi=si
errplot,mdrs(ng),nrat(ng)-nsig(ng),nrat(ng)+nsig(ng),color=colordex('B')
;
if keyword_set(ps) then begin
	psclose
	psfile,'std_comp_mdrs_sig'
endif else read,'next: ',q
;
; sigma
plot,mdrs,fsig,charsi=si,charthi=th, title=tlab, $
	xran=xrn,xtitle='MDRS(arcseconds)',xthick=th,xsty=1, $
	yran=yrns,ytitle='RMS(%)', ythick=th,ysty=1, $
	/nodata
oplot,mdrs(fg),fsig(fg),psym=4,color=colordex('V'),thick=th,symsi=si
plot,mdrs(ng),nsig(ng),charsi=si,charthi=th, title=tlab, $
	xran=xrn,xtitle='MDRS(arcseconds)',xthick=th,xsty=1, $
	yran=yrns,ytitle='RMS(%)', ythick=th,ysty=1, $
	/nodata
oplot,mdrs(ng),nsig(ng),psym=5,color=colordex('B'),thick=th,symsi=si
;
if keyword_set(ps) then $
	psclose
;
;
return
end
