pro plot_zspec, galid, ps=ps, png=png, pdf=pdf, glgaout=glgaout, $
	specfile=specfile, verbose=verbose
;+
;	plot_zspec - plot ZPEG fit file
;
; CALLING SEQUENCE:
;	plot_zspec,galid,/ps,/png,/glgaout,/verbose
;
; INPUTS:
;	galid	- galaxy id as specified in galdat struct
;
; KEYWORDS:
;	ps	- create postscript file of plot
;	png	- convert postscript file to png (implies /ps)
;	pdf	- convert postscript file to pdf (implies /ps)
;	glgaout	- put gif file in appropriate !GLGA_ROOT dir (implies /ps,/png)
;	specfile- ZPEG output file
;	verbose	- extra output to screen
;
; HISTORY:
; $Id$
;	08-JUN-2013, jdn - updated for GLGA
;	09-JUN-2013, jdn - no longer calls plot_bestfit_zpeg_gals 
;			   and plots log scale for wavelength in microns
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
psf = gal + '_hSEDzfit'
;
; check inputs
odir = './'
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
	odir = glga_dir + 'seds/zpeg/'
	do_glga = (1 eq 1)
endif else	do_glga = (1 eq 0)
;
; filenames
if keyword_set(specfile) then begin
	rfile = specfile
	tmp = specfile
	psf = gettok(tmp,'.')
endif else	rfile = odir+gal
;
; read ZPEG run information
zpeg_read_infos,rfile,zpeginfo
version = zpeginfo.version
templtab = zpeginfo.templates
;
; read ZPEG Galaxy fit information
zpeg_get_fitinfo,gal,rfile,$
	mygal,stfilters,pho,pho_err,$
	models,lambda_mod,flux_mod,lambdalines_mod,flines_mod,$
	/fnu, /zpeg_scen

nfilters=n_elements(stfilters.abvega)

if do_ps then begin
	font_store=!p.font
	psfile,psf,/color
	!p.font=0
	si=1.5
	sl=1.25
	th=5
	ss=1.0
endif else begin
	si=2.0
	sl=1.5
	th=3
	ss=2.0
endelse

deepcolor
!p.background=colordex('white')
!p.color=colordex('black')

;*******************************************************************************
; plot

; X RANGE --------------------------------------------------
xminmax = minmax(stfilters.lambdamean)+[-1000.,1000.]

ilambdao = where(lambda_mod ge xminmax(0) and lambda_mod le xminmax(1) and $
		 flux_mod gt 0.)

if (ilambdao[0] ne -1) then begin
	modmin=min(flux_mod(ilambdao))
	modmax=max(flux_mod(ilambdao))
endif else begin
	modmin=0.
	modmax=0.
endelse
xminmax=[750.,32000.]
xlab = textoidl('\lambda(\mum)')
tomic = 1.e4		; convert angstroms to microns

iok=where(pho_err(1,*) gt 0. and stfilters.lambdamean ge xminmax(0) and $
	stfilters.lambdamean le xminmax(1))

; Y RANGE --------------------------------------------------
if (iok(0) ne -1) then begin
	yminmax=[min([pho(iok),modmin]),$
		 max([pho(iok),modmax])]    
endif else begin
	yminmax=[min(pho),max(pho)]
endelse

my_yminmax=yminmax
yminmax=alog10(yminmax*[0.5,10.]) + 6.	; converto micro-Jy

;myytit='Fnu (Jy)'       ;=1e-23*erg/s/cm^2/Hz)'
myytit=textoidl('Log [ F_{\nu} \muJy ]')

plot,[0,1],[0,1],/nodata,xr=xminmax/tomic,yr=yminmax, $
       xtit=xlab,ytit=myytit,xs=1,/xlog, ys=9,title=gal, $
       charsize=si,xthick=th,ythick=th,charthi=th,_extra=_extra
;
; AB mag axis
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
; Annotate redshift on plot
if (mygal.xpix gt 0.) then $
	legende,0.8,'zspec='+ $
		strtrim(string(format='(f6.3)',mygal.xpix),2), $
		charsize=sl,_extra=_extra

;-------------------------------------------------------------------------------
;oplot models

nsols=mygal.nsol
for i=0,nsols-1 do begin

	lambda=lambda_mod(*,i)
	lambdalines=lambdalines_mod(*,i)

	;; DATA SPECTRUM --------------------------------------------------
	mycolor=colordex('V')
	pcolor=colordex('G')
	;; plot spectrum
	oplot,lambda/tomic,alog10(flux_mod[*,i])+6.,color=mycolor,thick=2.

	ilok=where(lambda ge min(stfilters.lambdamean) and $
		   lambda le max(stfilters.lambdamean))

	for l=0,n_elements(lambdalines)-1 do begin
		iok=(where(lambda ge lambdalines(l)))(0)
		oplot,lambdalines[l]/tomic*[1,1],alog10(flux_mod[iok,i]+ $
			flines_mod[l,i]/200.*[0,1])+6.,$
			color=mycolor,thick=2.,linestyle=1
	endfor

	;; ---------------------------------------
	;; photometry
	for ifi=0,nfilters-1 do begin
		if pho_err(0,ifi) gt 0. then $
			oplot,[stfilters(ifi).lambdaeff/tomic], $
			      [alog10(models[ifi,i])+6.],$
			      psym=4,color=pcolor,symsize=ss*2.0
	endfor        
	;; LEGEND--------------------------------------------------
	;; legend
	x0=!x.crange(0)+0.1*(!x.crange(1)-!x.crange(0))
	iok=(where(lambda ge x0))(0)
	mycolor=colordex('V')

	mass=mygal.zphots(i).mass(0)
	massmin=mygal.zphots(i).mass(1)
	massmax=mygal.zphots(i).mass(2)

	di=2.7
	gmt=(1d0/10d0^(mygal.zphots(i).specsfr(0)+9.))<99. ; growth time in Gyr
	gmtmax=(1d0/10d0^(mygal.zphots(i).specsfr(1)+9.))<99.+gmt
	gmtmin=(1d0/10d0^(mygal.zphots(i).specsfr(2)+9.))<gmt
	legende,1.5+i*di+0,$
		strtrim(string(format='("#",i2,"; z=",f5.3,"!S!U+",f5.3,"!R!D-",f5.3,"!N; n!Dbands!N= ",i2,"; E(B-V)!Dadd!N=",f3.1)',$
			i+1,mygal.zphots(i).z(0),$
			mygal.zphots(i).z(2)-mygal.zphots(i).z(0),$
			mygal.zphots(i).z(0)-mygal.zphots(i).z(1),$
			mygal.nbands,$
			mygal.zphots(i).ebv),2),$
		linestylep1=1,color=mycolor,charsize=sl,chcolor=0, $
		linelen=0.7,thick=2.,_extra=_extra

	legende,1.5+i*di+0.9,'   '+$
		strtrim(string(format='(a7," @",f6.3," Gyr; M*=",f5.2,"!S!U+",f5.2,"!R!D-",f5.2,"!N")',$
			templtab(mygal.zphots(i).itemplate-1),$
			mygal.zphots(i).age,$
			mass,massmax-mass,mass-massmin),2),$
		charsize=sl,thick=1., $
		chcolor=colordex('C'),_extra=_extra

	legende,1.5+i*di+1.8,'   '+$
		string(format='("<age>!DL!N=",f6.3,"!S!U+",f6.3,"!R!D-",f6.3,"!N Gyr; DMT=",f6.3,"!S!U+",f6.3,"!R!D-",f6.3,"!N Gyr")',$
			mygal.zphots(i).agestars(0),$
			mygal.zphots(i).agestars(2)-$
			mygal.zphots(i).agestars(0),$
			mygal.zphots(i).agestars(0)-$
			mygal.zphots(i).agestars(1),$
			gmt,gmtmax-gmt,gmt-gmtmin),$
		charsize=sl,thick=1.,chcolor=colordex('C'),_extra=_extra
endfor

;--------------------------------------------------------------------------------
;oplot photometry
vsym,8,/fill
for i=0,nfilters-1 do begin
	if (pho_err(0,i) gt 0) then begin
		flx = alog10(pho[i])+6.
		if (pho[i]-pho_err[0,i]) gt 0. then begin
			flx_lo = alog10(pho[i]-pho_err[0,i])+6.
			lo_err = flx - flx_lo
		endif else lo_err = 0.
		flx_hi = alog10(pho[i]+pho_err[1,i])+6.
		hi_err = flx_hi - flx
		oploterror,[stfilters[i].lambdaeff/tomic], $
			   [flx],[lo_err],psym=8, $
			   symsize=ss,/lobar,color=colordex('R')
		oploterror,[stfilters(i).lambdaeff/tomic], $
			   [flx],[hi_err],psym=8, $
			   symsize=ss,/hibar,color=colordex('R')
	endif
endfor

xyouts,0.02,0.02,systime()+' ZPEG v'+version,/norm

if do_ps then begin
	!p.font = font_store
	psclose
endif
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
return
end
