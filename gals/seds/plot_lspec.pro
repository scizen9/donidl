pro plot_lspec, galid, ps=ps, png=png, pdf=pdf, glgaout=glgaout, $
	specfile=specfile, parfile=parfile, verbose=verbose
;+
;	plot_lspec - plot LePhare *.spec file
;
; CALLING SEQUENCE:
;	plot_lspec,galid,/ps,/png,/glgaout,/verbose
;
; INPUTS:
;	galid	- galaxy id as specified in galdat struct
;
; KEYWORDS:
;	ps	- create postscript file of plot
;	png	- convert postscript file to png (implies /ps)
;	pdf	- convert postscript file to pdf (implies /ps)
;	glgaout	- put gif file in appropriate !GLGA_ROOT dir (implies /ps,/png)
;	specfile- LePhare *.spec file
;	parfile	- LePhare *.para file used to generate *.spec file
;	verbose	- extra output to screen
;
; HISTORY:
; $Id: plot_lspec.pro,v 1.14 2013/11/20 16:14:17 neill Exp $
;	29-jul-2011, jdn - Initial Version
;	30-jul-2011, jdn - Improved plot
;	31-jul-2011, jdn - refined output file
;	01-aug-2011, jdn - added glgaout keyword
;	08-JUN-2013, jdn - updated for GLGA
;-
; galdat common
common galdb_info
;
; get galaxy
if n_params(0) lt 1 then begin
	gal=''
	read,'Enter Galaxy id: ',gal
endif else gal = galid
;
; get galdat index
g = gfind(gal)
if g[0] lt 0 then begin
	print,'Not in gal db: ',gal
	in_db = (1 eq 0)
	glga_dir = './'
endif else begin
	in_db = (1 eq 1)
	; get GLGA directory
	glga_dir = !GLGA_ROOT + 'data/'+glga_degdir(galdat[g].ra)+'/'
endelse
;
; output file
psf = gal + '_hSEDlfit'
;
; get version string
fi = file_info(getenv('LEPHAREDIR')+'/source/zphota')
sta = strsplit(systime(0,fi.ctime),/extract)
version = sta[2]+'-'+strupcase(sta[1])+'-'+sta[4]
;
; check inputs
odir = ''
if keyword_set(ps) then begin
	do_ps = (1 eq 1)
endif else	do_ps = (1 eq 0)
;
if keyword_set(png) then begin
	do_ps = (1 eq 1)
	do_png= (1 eq 1)
endif else	do_png = (1 eq 0)
;
if keyword_set(pdf) then begin
	do_ps = (1 eq 1)
	do_pdf= (1 eq 1)
endif else	do_pdf = (1 eq 0)
;
if keyword_set(glgaout) then begin
	do_ps = (1 eq 1)
	do_png= (1 eq 1)
	do_pdf= (1 eq 1)
	odir = glga_dir + 'seds/lephare/'
	do_glga = (1 eq 1)
endif else	do_glga = (1 eq 0)
;
; filenames
if keyword_set(specfile) then begin
	lfile = specfile
	psf = strmid(specfile,0,strpos(specfile,'.spec'))
endif else	lfile = odir + gal + '.spec'
if keyword_set(parfile) then $
	pfile = parfile $
else	pfile = odir + 'gals_zphota.para'
;
; read in params
read_lpara,pfile,zphotlib,firlibs
;
; read in fit data
read_lspec,lfile,galwav,galmag, $
	firwav=firwav,firmag=firmag, $
	obmag=obmag,oberr=oberr,flwav=flwav,flwid=flwid, $
	gpmag=gpmag,fpmag=fpmag, $
	idno=idno,zspec=zspec,zphot=zphot,nfilt=nfilt,pdf=pdf, $
	npgal=npgal,mdgal=mdgal,ligal=ligal,nbgal=nbgal,chgal=chgal, $
	exgal=exgal,ebgal=ebgal, $
	aggal=aggal,msgal=msgal,sfgal=sfgal,ssgal=ssgal, $
	npfir=npfir,mdfir=mdfir,lifir=lifir,nbfir=nbfir,chfir=chfir, $
	lmfir=lmfir,verbose=verbose,status=stat
;
; check status
if stat lt 0 then begin
	print,'PLOT_LSPEC [ERROR]: No data read, returning.'
	return
endif
;
; log10 wave for interpolation
iwav = findgen(5000)/1000. - 1.5
;
; min,max y values
miny = 1.e9
maxy = -1.e9
plotgal = (1 eq 0)
plotfir = (1 eq 0)
;
; GAL-1 data
if npgal gt 0 then begin
	read_lmodel,zphotlib,mdgal,model
	zphotlib = gettok(zphotlib,'_')
	galwav = galwav / 1.e4			; convert ang to microns
	b = where(galmag ge 35, nb)
	g = where(galmag lt 35 and galmag gt 0.,ng1)
	if nb gt 0 then galmag[b] = -10.
	if ng1 gt 0 then begin
		galmag[g] = (-0.4*(galmag[g]-23.91))	; convert AB mag to Lg10 muJy
		gint = interpol(10.^galmag[g],alog10(galwav[g]),iwav)	; interpolate
	endif
	g = where(gpmag lt 35 and gpmag gt 0,ng2)
	b = where(gpmag ge 35, nb)
	if nb gt 0 then gpmag[b] = -10.
	if ng2 gt 0 then begin
		gpmag[g] = (-0.4*(gpmag[g]-23.91))
		miny = min([miny,gpmag[g]])
		maxy = max([maxy,gpmag[g]])
		plotgal = (1 eq 1)
	endif
endif else begin
	zphotlib = ''
	gint = -1
	plotgal = (1 eq 0)
	galwav=[-1]
	galmag=[-1]
endelse
;
; GAL-FIR data
if npfir gt 0 then begin
	temp = firlibs[lifir-1]
	firlib = gettok(temp,'_')
	firwav = firwav / 1.e4			; convert ang to microns
	g = where(firmag lt 35 and firmag gt 0.,ng1)
	b = where(firmag ge 35, nb)
	if nb gt 0 then firmag[b] = -10.
	if ng1 gt 0 then begin
		firmag[g] = (-0.4*(firmag[g]-23.91))	; convert AB mag to Lg10 muJy
		fint = interpol(10.^firmag[g],alog10(firwav[g]),iwav)	; interpolate
	endif else begin
		fint = -1
		plotfir = (1 eq 0)
	endelse
	g = where(fpmag lt 35 and fpmag gt 0.,ng2)
	b = where(fpmag ge 35, nb)
	if nb gt 0 then fpmag[b] = -10.
	if ng2 gt 0 then begin
		fpmag[g] = (-0.4*(fpmag[g]-23.91))
		miny = min([miny,fpmag[g]])
		maxy = max([maxy,fpmag[g]])
		plotfir = (1 eq 1)
	endif
endif else begin
	firlib = ''
	fint = -1
	plotfir = (1 eq 0)
endelse
;
; add interpolated fluxes
if npfir gt 0 and npgal gt 0 then begin
	iwav = 10.^iwav
	b = where(gint lt 0. or iwav gt max(galwav), nb)
	if nb gt 0 then gint[b] = 0.
	b = where(fint lt 0. or iwav lt 3., nb)
	if nb gt 0 then fint[b] = 0.
	tint = gint + fint
	b = where(tint le 0, nb)
	if nb gt 0 then tint[b] = 1.e-9
	tint = alog10(tint)
	plotsum = (1 eq 1)
endif else plotsum = (1 eq 0)
;
; convert waves to microns
flwid = (flwid / 1.e4) * 0.5		; half-width
flwav = flwav / 1.e4
;
; convert observed AB mags to log10 microJy
g = where(obmag gt 0. and obmag lt 35)
b = where(obmag lt 0., nb)
obmag[g] = (-0.4*(obmag[g]-23.91))
miny = min([miny,obmag[g]])
maxy = max([maxy,obmag[g]])
if nb gt 0 then obmag[b] = -10.
g = where(oberr ge 0.)
b = where(oberr lt 0.,nb)
oberr[g] = 0.8*oberr[g]
if nb gt 0 then oberr[b] = -10.
;
; plot
font_save = !p.font
if do_ps then begin
	psfile,psf
	!p.font = 0
	th=3
	si=1.5
	ls=1.08
	ss=1.25
endif else begin
	th=2
	si=2.25
	ls=1.75
	ss=2.0
endelse
deepcolor
ci   = [colordex('white'), colordex('black'), colordex('blue'), $
	colordex('red'), colordex('green'), colordex('turquoise')]
!p.background=ci[0]
!p.color=ci[1]
;
; xrange
xrng = [0.075,525]		; microns
xlab = textoidl('\lambda(\mum)')
;
; yrange
yrng = [(miny-0.5)>0,maxy+1.25]
ylab = textoidl('Log [ F_{\nu} \muJy ]')
;
; title
if do_glga then $
	tlab = gal+', z!Dspec!N = '+string(zspec,form='(f7.4)') $
else	tlab = 'ID: '+strn(idno)+', z!Dspec!N = '+string(zspec,form='(f7.4)')+ $
	', '+lfile
;
vsym,24,/fill
plot,galwav,galmag,yran=yrng,xran=xrng,/nodata,ysty=9,xsty=1,/xlog, $
	thick=th,charthi=th,xtitle=xlab,ytitle=ylab,charsi=si, $
	xthick=th,ythick=th,title=tlab
;
; AB mag axis
my_yminmax = 10.^[yrng-6.]
if (my_yminmax[0] le 0) then my_yminmax[0]=1e-2*my_yminmax[1]
mag_minmax=minmax(-2.5*alog10(my_yminmax*1e-23)-48.60)+[-1.,1]
dmposs=[0.01,0.02,0.05,0.1,0.2,0.5,1.,2.,5.,10.]
for l=0,n_elements(dmposs)-1 do begin
	if round(mag_minmax[1]-mag_minmax[0])/dmposs[l] ge 11 then $
		dm=dmposs[l]
endfor
mini=dm*fix(mag_minmax[0]/dm)
V = mini+indgen(round((mag_minmax[1]-mag_minmax[0])/dm+1))*dm
L = string(format='(f5.1)',V)
V=alog10(1d23*10.^(-0.4*(V+48.60)))+6.
n=n_elements(L) 
axis, yaxis=1, yticks=N-1, ytickv=V, ytickname=L, ytitle='AB mag', $
	charsize=si,ythick=th,charthi=th
;
oplot,flwav,obmag,psym=8,symsi=ss
oploterror,flwav,obmag,flwid,oberr,psym=3
vsym,24
if plotgal then $
	oplot,flwav,gpmag,psym=8,color=ci[5],symsi=ss/2.
if plotfir then $
	oplot,flwav,fpmag,psym=8,color=ci[3],symsi=ss/2.
if plotgal then $
	oplot,galwav,galmag,color=ci[2]
if plotfir then $
	oplot,firwav,firmag,color=ci[3]
if plotsum then $
	oplot,iwav,tint,color=ci[4],thick=th
;
xs = [0.18, 0.5, 1.1, 1.5, 4, 8, 16, 30, 60, 120, 220, 420]
ydel = (max(yrng) - min(yrng)) * 0.05
ys = [max(yrng) - ydel * 1.5, max(yrng) - ydel * 2.5, max(yrng) - ydel * 3.5]
ttag = ['SED','Lib','Model','N',textoidl('\chi^2'), 'z!Dphot!N', $
	'E(B-V)','Lir','Age','Mass','SFR','sSFR']
for i=0,n_elements(ttag)-1 do $
	xyouts,xs[i],ys[0],align=1.0,ttag[i],charthi=th,charsi=ls,color=ci[1]
if plotgal then begin
	gtag = ['GAL-1',zphotlib,model,strn(nbgal), $
		strtrim(string(chgal, form='(f11.2)'),2), $
		strtrim(string(zphot, form='(f7.4)'),2), $
		strtrim(string(ebgal, form='(f5.2)'),2), ' ', $
		strtrim(string(aggal, form='(f6.2)'),2), $
		strtrim(string(msgal, form='(f6.2)'),2), $
		strtrim(string(sfgal, form='(f6.2)'),2), $
		strtrim(string(ssgal, form='(f6.2)'),2) ]
	for i=0,n_elements(gtag)-1 do $
		xyouts,xs[i],ys[1],align=1.0,gtag[i],charthi=th,charsi=ls, $
		color=ci[2]
endif
if plotfir then begin
	ftag = ['GAL-FIR',firlib,strn(mdfir),strn(nbfir), $
		strtrim(string(chfir, form='(f11.2)'),2), ' ', ' ', $
		strtrim(string(lmfir, form='(f9.2)'),2) ]
	for i=0,n_elements(ftag)-1 do $
		xyouts,xs[i],ys[2],align=1.0,ftag[i],charthi=th,charsi=ls, $
		color=ci[3]
endif
xyouts,0.02,0.02,systime()+' LEPHARE v'+version,/norm
;
if do_ps then psclose
;
if do_glga then begin
	cmd = 'convert -rotate -90 ' + psf +'.ps PNG:' + odir+psf + '.png'
	if keyword_set(verbose) then print,cmd
	spawn,cmd
	cmd = 'ps2pdf ' + psf +'.ps ' + odir+psf + '.pdf'
	if keyword_set(verbose) then print,cmd
	spawn,cmd
	cmd = 'rm ' + psf + '.ps'
	if keyword_set(verbose) then print,cmd
	spawn,cmd
endif
;
if not do_glga and do_png then begin
	cmd = 'convert -rotate -90 ' + psf +'.ps PNG:' + odir+psf + '.png'
	if keyword_set(verbose) then print,cmd
	spawn,cmd
endif
;
if not do_glga and do_pdf then begin
	cmd = 'ps2pdf ' + psf +'.ps ' + odir+psf + '.pdf'
	if keyword_set(verbose) then print,cmd
	spawn,cmd
endif
;
!p.font = font_save
;
return
end
