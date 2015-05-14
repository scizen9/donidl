pro pltgalsed,galid,ps=ps,png=png,pdf=pdf,glgaout=glgaout,test=test, $
	yuan13=yuan13,update=update,verbose=verbose
;+
;	pltgalsed - plot SED of galid from galdat structure
;
; CALLING SEQUENCE:
;	pltgalsed,galid,/ps,/png,/glgaout,test=<int>,/verbose
;
; INPUTS:
;	galid	- galaxy id as specified in galdat struct
;
; KEYWORDS:
;	ps	- create postscript file of plot
;	png	- convert postscript file to png (implies /ps)
;	pdf	- convert postscript file to pdf (implies /ps)
;	glgaout	- put png,pdf file in !GLGA_ROOT degree dir (implies /ps,/png)
;	test	- integer minimum number of valid bandpasses required to plot
;	yuan13	- use extinction coeff's of Yuan et al. 2013
;	update	- only plot if GLGA photometry is newer than GLGA plot
;	verbose	- extra output to screen
;
; CALLS:
;	get_bestfluxes to get the fluxes to plot
;
; HISTORY:
; $Id: pltgalsed.pro,v 1.29 2013/09/03 15:36:23 neill Exp $
;	29-apr-2011, jdn - Initial Revision
;	24-may-2011, jdn - Removed site keyword
;	20-jul-2011, jdn - Now can test if any valid fluxes to plot
;	01-aug-2011, jdn - Rationalized keywords, outputs, removed status keyword
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
; get fluxes
get_bestfluxes,gal,flux,flxer,waves,bwid,srce,gphts, $
	galex_indices=gxi,sdss_indices=sdi,rc3_indices=rci, $
	twomass_indices=tmi,wise_indices=wii, iras_indices=iri, $
	status=stat, yuan13=yuan13, /silent
;
; check results
if stat ne 0 then begin
	print,'Not in gal and/or glga db: ',gal
	return
endif
;
; get galdat index
g = gfind(gal)
;
; output file
psf = gal + '_hSED'
;
; get GLGA directory
glga_dir = !GLGA_ROOT + 'data/'+glga_degdir(galdat[g].ra)+'/'
;
; get type
type = galdat[g].type
if strlen(type) le 0 then $
	type = '-'
;
if keyword_set(verbose) then $
	print,gal
;
; get version string
version = '$Revision: 1.29 $'
junk = gettok(version,' ')
junk = gettok(version,' ')
version = junk
;
; get output
if keyword_set(ps) then begin
	do_ps = (1 eq 1)
	odir = './'
endif else	do_ps = (1 eq 0)
;
if keyword_set(png) then begin
	do_ps = (1 eq 1)
	do_png= (1 eq 1)
	odir = './'
endif else	do_png = (1 eq 0)
;
if keyword_set(pdf) then begin
	do_ps = (1 eq 1)
	do_pdf= (1 eq 1)
	odir = './'
endif else	do_pdf = (1 eq 0)
;
if keyword_set(glgaout) then begin
	do_ps = (1 eq 1)
	do_png= (1 eq 1)
	do_pdf= (1 eq 1)
	odir = glga_dir + 'plots/'
	do_glga = (1 eq 1)
endif else	do_glga = (1 eq 0)
;
; GLGA phot path
ppath = glga_dir+'photometry/' 
;
; should we test for good fluxes?
if keyword_set(test) then $
	ntst = test $
else	ntst = 0
;
; prepare log fluxes
flxe_lo = flux - flxer
flxe_up = flux + flxer
lgflx = flux - flux
lgflxe_lo = lgflx
lgflxe_up = lgflx
;
; get lower limit
f = where(flxe_lo gt 0, nf)
if nf gt 0 then begin
	lgflxe_lo[f] = alog10(flxe_lo[f]) + 6.	; micro-Jy
	y0 = min(lgflxe_lo[f]) - 0.25
endif	else y0 = -1.
;
; get lower limits to good fluxes that go negative
f = where(flux gt 0. and flxe_lo lt 0, nf)
if nf gt 0 then $
	lgflxe_lo[f] = y0
;
; get upper limit
f = where(flxe_up gt 0., nf)
if nf gt 0 then begin
	lgflxe_up[f] = alog10(flxe_up[f]) + 6.	; micro-Jy
	y1 = max(lgflxe_up[f]) + 1.
endif	else y1 = 1.
;
; check for update only
if keyword_set(update) then begin
	;
	; plot timestamp
	fi = file_info(glga_dir+'plots/'+psf+'.pdf')
	;
	; if photometry is old, don't plot
	if gphts le fi.mtime then begin
		if keyword_set(verbose) then $
			print,'Plot up-to-date: ',gal
		return
	endif
endif
;
; anough valid fluxes?
f=where(flux gt 0., nf)
if keyword_set(test) and nf lt ntst then begin
	return
endif
if nf gt 0 then begin
	lgflx[f] = alog10(flux[f]) + 6.	; micro-Jy
endif
;
; get plottable fluxes
f=where(flux gt 0. and flxer gt 0., nf)
;
; plot
font_store=!p.font
if do_ps then begin
	th=5
	si=1.5
	psfile,psf
	print,'Plotting to: '+psf+'.ps'
	!p.font=0
endif else begin
	th=3
	si=2.0
endelse
;
; xrange
waves = waves / 1.e4		; convert Angstroms to microns
bwid = bwid / 1.e4
xrng = [0.075,525]
xlab = textoidl('\lambda(\mum)')
xl = 13
;
; yrange
yrng = [y0,y1]
ylab = textoidl('Log [ F_{\nu} \muJy ]')
;
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
plot,waves,flux,psym=8,thick=th,xthick=th,ythick=th,charthi=th,$
		charsi=si,title=gal+'  '+type, $
		xtitle=xlab,xran=xrng,xsty=1, $
		ytitle=ylab,yran=yrng,ysty=9,/xlog,/nodata
;
; AB mag axis
my_yminmax = 10.^[yrng-6.]
if finite(my_yminmax[1]) eq 0 then my_yminmax[1]=my_yminmax[0] + 2.
if finite(my_yminmax[0]) eq 0 then my_yminmax[0]=my_yminmax[1] - 2.
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
; plot error-bars
if nf gt 0 then begin
	errplot,waves[f],lgflxe_lo[f],lgflxe_up[f]
	oploterror,waves[f],lgflx[f],bwid[f]/2.,flxer[f]-flxer[f],psym=3
endif
;
; get limits
u=where(flux gt 0. and flxer lt 0., nu)
if nu gt 0 then $
	olimplot,waves[u],lgflx[u],scale=2.0
;
; flux level for missing data
yranl = !y.crange[1] - !y.crange[0]
fbad=!y.crange[0]+0.05*yranl
;
; colors
nwavs		= n_elements(waves)
ci		= lonarr(nwavs)
ci[gxi] 	= colordex('P')
ci[sdi]		= colordex('B')
ci[rci]		= colordex('Q')
ci[tmi]		= colordex('O')
ci[wii]		= colordex('N')
ci[iri]		= colordex('R')
cols = [gxi[0],sdi[0],rci[0],tmi[0],wii[0],iri[0]]
;
; loop over filters
for i=0,nwavs-1 do begin
	if srce[i] le 0 or srce[i] gt 2 then begin	; missing
		plots,waves[i],fbad,psym=1,thick=th,symsi=si,color=ci[i]
	endif else begin
		if srce[i] eq 1 then begin
			vsym,24,/fill 			; GLGA
			psi = 1.0
		endif else begin
			vsym,3,/fill
			psi = 1.5
		endelse
		plots,waves[i],lgflx[i],psym=8,thick=th,symsi=psi,color=ci[i]
	endelse
endfor
;
; legend
vsym,24,/fill
x0 = 0.37
x1 = 0.41
plots,x0,!y.crange[1]-yranl*0.07,psym=8,symsi=1.0,thick=th
xyouts,x1,!y.crange[1]-yranl*0.08,'GLGA',charthi=th,charsi=si
vsym,3,/fill
plots,x0,!y.crange[1]-yranl*0.12,psym=8,symsi=1.5,thick=th
xyouts,x1,!y.crange[1]-yranl*0.13,'Literature',charthi=th,charsi=si
plots,x0,!y.crange[1]-yranl*0.17,psym=1,symsi=1.3,thick=th
xyouts,x1,!y.crange[1]-yranl*0.18,'Missing',charthi=th,charsi=si
lem=strarr(3)
if galdat[g].cz lt -900. then $
	czstr = '-' $
else	czstr = strtrim(string(galdat[g].cz,format='(f9.1)'),2)
lem[0] = 'cz = '+czstr
lem[1] = 'E(B-V)!DMW!N = '+strtrim(string(galdat[g].mwebmv,format='(f5.3)'),2)
lem[2] = galdat[g].sample
legend,lem,charsi=si,charthi=th,box=0,pos=[xl,!y.crange[1]-0.01*yranl],spac=2.0
;
lem=strarr(6)
lem[0] = 'GALEX'
lem[1] = 'SDSS'
lem[2] = 'RC3'
lem[3] = '2MASS'
lem[4] = 'WISE'
lem[5] = 'IRAS'
legend,lem,textcolor=ci[cols],charthi=th,charsi=si,box=0,spac=1.7
;xyouts,0.995,0.30,systime()+' pltgalsed v'+version,/norm,ori=90.
xyouts,0.02,0.02,systime()+' pltgalsed v'+version,/norm
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
!p.font=font_store
;
return
end
