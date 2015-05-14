pro galex_plotradprof, id, pathtoprofile=pathtoprofile, $
    fintfile=fintfile, nintfile=nintfile, $
    maskimagefile=maskimagefile, masklistfile=masklistfile, $
    uvjpgpath=uvjpgpath, dssfile=dssfile ,outpath=outpath

;
; Warning this procedure spawns convert and ps2pdf14
; (without any checking) to convert postscript output 
; to pdf and jpeg files. Can just comment out if postscript
; is OK. 
;

nofuv=0
nonuv=0
nintdata=0
fintdata=0
xdpic=0
nfpic=0
fdpic=0
dssdata=0

if not keyword_set(pathtoprofile) then pathtoprofile='./'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; files names from galex_radprof

ntotfile =pathtoprofile+'/'+id+'_NUV_totprofile.dat'
nannfile =pathtoprofile+'/'+id+'_NUV_annprofile.dat'
nbgfile  =pathtoprofile+'/'+id+'_NUV_background.dat'
nellfile =pathtoprofile+'/'+id+'_NUV_ellipsepar.dat'
ntotflxfile =pathtoprofile+'/'+id+'_NUV_total.dat'
naperflxfile =pathtoprofile+'/'+id+'_NUV_aperture.dat'

ftotfile =pathtoprofile+'/'+id+'_FUV_totprofile.dat'
fannfile =pathtoprofile+'/'+id+'_FUV_annprofile.dat'
fbgfile  =pathtoprofile+'/'+id+'_FUV_background.dat'
fellfile =pathtoprofile+'/'+id+'_FUV_ellipsepar.dat'
ftotflxfile =pathtoprofile+'/'+id+'_FUV_total.dat'
faperflxfile =pathtoprofile+'/'+id+'_FUV_aperture.dat'

;;;;;;;;;;;;;;;;;;;;;;;
; read in profile data

;nuv

if file_exist(ntotfile) and file_exist(nannfile)and $
   file_exist(nbgfile) and file_exist(nellfile) then begin

 readcol, ntotfile, ntot_a, ntot_mag, ntot_mag_e, ntot_int, ntot_int_e, $
          ntot_int_e_cnt, ntot_int_e_bg, ntot_bg, ntot_npixels,/silent

 readcol, ntotflxfile, ntf_a, ntf_mag, ntf_mag_e, ntf_int, ntf_int_e, $
          ntf_int_e_cnt, ntf_int_e_bg, ntf_bg, ntf_npixels,/silent

 readcol, naperflxfile, naf_a, naf_mag, naf_mag_e, naf_int, naf_int_e, $
          naf_int_e_cnt, naf_int_e_bg, naf_bg, naf_npixels,/silent

 readcol, nannfile, nann_a, nann_mu, nann_mu_e, nann_int, nann_int_e, $
          nann_int_e_cnt, nann_int_e_bg, nann_bg, nann_npixels,/silent

 readcol, nellfile, nra_cen, ndec_cen, nsemimajor, nsemiminor, npa,/silent

 readcol, nbgfile, nbg, nbg_e, nmu_bg, nmu_bg_e, nscale, $
          nskyradius_in, nskyradius_out,/silent

 ;;;;;;;;;;;;;;;;;;;
 ; do sympototic mag

 setplotcolors
 dm=shift(ntot_mag,1) - ntot_mag
 dr=shift(ntot_a,1) - ntot_a

 ;good = where(ntot_int gt 0); and nann_mu_e le 0.8)
 ;good = where(good ge 1)
 ;meanclip,dm/dr,meanm,sig,subs=subs
 ;good=good[subs]

 n=n_elements(dm)
 good=indgen(n)
 good=where(good ge floor(0.25*n) and nann_int gt 0) 

 x=dm[good]/dr[good]
 y=ntot_mag[good]
 erry=ntot_mag_e[good]
 errx=erry/x
 plot,x,y,/ynozero,psym=sym(1),$ 
  yr=[min(y)-3,max(y)+3],/ys
 fit=linfit(x,y,measure_errors=erry,$
            sigma=sigma,yfit=yfit)
 oplot, x,yfit
 print,fit,sigma 

 ;FITEXY, x, y, A, B, x_sigma=errx ,Y_SIG=erry , sigma_A_B
 ;print,a,b
 ;print,sigma_A_B


endif else nonuv=1

;fuv

if file_exist(ftotfile) and file_exist(fannfile)and $
   file_exist(fbgfile) and file_exist(fellfile) then begin

 readcol, ftotfile, ftot_a, ftot_mag, ftot_mag_e, ftot_int, ftot_int_e, $
          ftot_int_e_cnt, ftot_int_e_bg, ftot_bg, ftot_npixels,/silent

 readcol, ftotflxfile, ftf_a, ftf_mag, ftf_mag_e, ftf_int, ftf_int_e, $
          ftf_int_e_cnt, ftf_int_e_bg, ftf_bg, ftf_npixels,/silent

 readcol, faperflxfile, faf_a, faf_mag, faf_mag_e, faf_int, faf_int_e, $
          faf_int_e_cnt, faf_int_e_bg, faf_bg, faf_npixels,/silent

 readcol, fannfile, fann_a, fann_mu, fann_mu_e, fann_int, fann_int_e, $
          fann_int_e_cnt, fann_int_e_bg, fann_bg, fann_npixels,/silent

 readcol, fellfile, fra_cen, fdec_cen, fsemimajor, fsemiminor, fpa,/silent

 readcol, fbgfile, fbg, fbg_e, fmu_bg, fmu_bg_e, fscale, $
          fskyradius_in, fskyradius_out,/silent

endif else nofuv=1

if nofuv and nonuv then begin
 print,pre+'Can not find any profile data.'
 return
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; set some useful values to
; set up plots regardless of band

if ~nonuv then begin
 a=ntot_a
 tot_mag=ntot_mag
 ann_mu=nann_mu 
 ra_cen=nra_cen[0]
 dec_cen=ndec_cen[0]
 semimajor=nsemimajor[0]
 semiminor=nsemiminor[0]
 pa=npa[0]
 b=a*(semiminor/semimajor)
 r=sqrt(a*b)
 r_ap=sqrt(semimajor*semiminor)
endif else begin
 a=ftot_a
 tot_mag=ftot_mag
 ann_mu=fann_mu
 ra_cen=fra_cen[0]
 dec_cen=fdec_cen[0]
 semimajor=fsemimajor[0]
 semiminor=fsemiminor[0]
 pa=fpa[0]
 b=a*(semiminor/semimajor)
 r=sqrt(a*b)
 r_ap=sqrt(semimajor*semiminor)
endelse

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; read in uv int headers

if keyword_set(nintfile) and file_exist(nintfile) then begin
 ;nint=mrdfits(nintfile,0,nhdr,/silent)
 nhdr=headfits(nintfile,ext=0)
 extast,nhdr,nastr
 AD2XY, nra_cen[0] ,ndec_cen[0], nastr, nx0, ny0
 nintdata=1
endif

if keyword_set(fintfile) and file_exist(fintfile) then begin
 ;fint=mrdfits(fintfile,0,fhdr,/silent)
 fhdr=headfits(fintfile,ext=0)
 extast,fhdr,fastr
 AD2XY, fra_cen[0] ,fdec_cen[0], fastr, fx0, fy0
 fintdata=1
endif

if keyword_set(dssfile) and file_exist(dssfile) then begin
 dss = mrdfits(dssfile, 0, dhdr)
 extast, dhdr, dssastr
 AD2XY, ra_cen,dec_cen, dssastr, dssx0, dssy0
 getrot,dhdr,rot_dss,cdelt_dss
 dssscale = abs(cdelt_dss[0])*3600.
 dssdata=1
endif
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;
; read in jpg images

xdjpgfile =uvjpgpath+'/'+id+'_xd_2color.jpg'
ndjpgfile =uvjpgpath+'/'+id+'_nd_2color.jpg'
fdjpgfile =uvjpgpath+'/'+id+'_fd_2color.jpg'

if keyword_set(xdjpgfile) then begin
 read_jpeg,xdjpgfile,xdjpg,/true
 xdpic=1

 ;mask the 2color image

 if keyword_set(maskimagefile) and file_exist(maskimagefile) then begin
  tmask=mrdfits(maskimagefile,0,mhdr)
  maskidx=where(tmask eq 1)
  xdjpgr = xdjpg[0, *, *]
  xdjpgr[maskidx] = 0
  xdjpgg = xdjpg[1, *, *]
  xdjpgg[maskidx] = 0
  xdjpgb = xdjpg[2, *, *]
  xdjpgb[maskidx] = 0
  xdjpg[0, *, *] = xdjpgr
  xdjpg[1, *, *] = xdjpgg 
  xdjpg[2, *, *] = xdjpgb
 endif

 if keyword_set(masklistfile) and file_exist(masklistfile) then begin
  readcol,masklistfile,maskidx,x,y,format='l,i,i'
  xdjpgr = xdjpg[0, *, *]
  xdjpgr[maskidx] = 0
  xdjpgg = xdjpg[1, *, *]
  xdjpgg[maskidx] = 0
  xdjpgb = xdjpg[2, *, *]
  xdjpgb[maskidx] = 0
  xdjpg[0, *, *] = xdjpgr
  xdjpg[1, *, *] = xdjpgg 
  xdjpg[2, *, *] = xdjpgb
 endif

endif

if keyword_set(ndjpgfile) then begin
 read_jpeg,ndjpgfile,ndjpg,/true
 sz=size(ndjpg,/dim)
 ndimg=bytarr(sz[1],sz[2])
 ndimg[*,*]=ndjpg[0,*,*]
 ndpic=1
endif

if keyword_set(fdjpgfile) then begin
 read_jpeg,fdjpgfile,fdjpg,/true
 sz=size(fdjpg,/dim)
 fdimg=bytarr(sz[1],sz[2])
 fdimg[*,*]=fdjpg[0,*,*]
 fdpic=1
endif

;;;;;;;;;;;;;;;;;;;;;;
; start profile plots


filename=outpath+'/'+id+'_profile.ps'
ps_start, filename=filename, xsize=10.5,ysize=8.,/inch,$
          /color,bit=8, xoffset=0.15, yoffset = 10.75,/landscape

!p.multi=[0,2,3]

setplotcolors
psize=0.75
chrsz=2

;growth curve

if ~nofuv then begin
 fminmax=minmax(ftot_mag, /nan)
 min=fminmax[0]
 max=fminmax[1]
endif
if ~nonuv then begin
 nminmax=minmax(ntot_mag, /nan)
 min=nminmax[0]
 max=nminmax[1]
endif
if ~nofuv and ~nonuv then begin
 min=min([fminmax[0],nminmax[0]])
 max=max([fminmax[1],nminmax[1]])
endif

plot,[r/60],[tot_mag],yr=[max+0.5, min-0.5],/ys,/nodata,$
    xr = [0, max(r/60.)+0.2], /xs,$
    xtit='R!de!n [arc minutes]',$
    ytit='Growth Curve [AB mag]',charsize=chrsz

if ~noNUV then $
 oploterror, [r/60.],[ntot_mag],[ntot_mag_e*0],[ntot_mag_e],$
  color=!red,errcolor=!red,psym=-sym(1, psize = psize), /nohat

if ~nofUV then $
 oploterror, [r/60.],[ftot_mag],[ftot_mag_e*0],[ftot_mag_e],$
  color=!blue,errcolor=!blue,psym=-sym(1, psize = psize), /nohat

legend,['NUV','FUV'],textcolors=[!red,!blue],/bot,/right,box=0,charsize=0.8

;annular surface brightness

if ~nofuv then begin
 fminmax=minmax(fann_mu, /nan)
 min=fminmax[0]
 max=fminmax[1]
endif
if ~nonuv then begin
 nminmax=minmax(nann_mu, /nan)
 min=nminmax[0]
 max=nminmax[1]
endif
if ~nofuv and ~nonuv then begin
 min=min([fminmax[0],nminmax[0]])
 max=max([fminmax[1],nminmax[1]])
endif

plot,[r/60],[ann_mu],yr=[max+0.5, min-0.5],/ys,/nodata,$
    xr = [0, max(r/60.)+0.2], /xs,$
    xtit='R!de!n [arc minutes]',$
    ytit=greek('mu',/append)+' [AB mag/arcsec!u2!n]',charsize=chrsz

if ~noNUV then begin
 oploterror, [r/60.],[nann_mu],[nann_mu_e*0],[nann_mu_e],$
  color=!red,errcolor=!red,psym=-sym(1, psize = psize), /nohat
 ;oplot,[ntf_a/60,ntf_a/60],[max+0.5,min-0.5],line=2,color=!red,thick=4
endif

if ~noFUV then begin
 oploterror, [r/60.],[fann_mu],[fann_mu_e*0],[fann_mu_e],$
  color=!blue,errcolor=!blue,psym=-sym(1, psize = psize), /nohat
 ;oplot,[ftf_a/60,ftf_a/60],[max+0.5,min-0.5],line=2,color=!blue,thick=4
endif

 oplot,[r_ap/60,r_ap/60],[max+0.5,min-0.5],line=2,thick=4

legend,['NUV','FUV'],textcolors=[!red,!blue],/bot,/left,box=0,charsize=0.8

; total color

fn=where(finite(ftot_mag),cntffinite)
nn=where(finite(ntot_mag),cntnfinite)

if ~nofuv and ~nonuv and cntffinite gt 0 and cntnfinite gt 0 then begin
 min=min(ftot_mag - ntot_mag)
 max=max(ftot_mag - ntot_mag)
 plot, [r/60.], [ftot_mag - ntot_mag], psym=-sym(1, psize = psize),$
     charsize=chrsz, ytit = 'Total FUV-NUV', xtit='R!de!n [arc minutes]',$
     xr = [0, max(r/60.)+0.2], /xs, $
     yr = [min-0.05, max+0.05],/ys,/nodata
 err=sqrt(ftot_mag_e^2 + ntot_mag_e^2)
 oploterror, [r/60.],[ftot_mag - ntot_mag],[err*0],[err],$
  psym=-sym(1, psize = psize), /nohat
endif else begin
 plot, [r/60.], [r/60.], psym=-sym(1, psize = psize),$
     charsize=chrsz, ytit = 'Total FUV-NUV', xtit='R!de!n [arc minutes]',$
     xr = [0, max(r/60.)+0.1], /xs, $
     yr = [-1, 1],/ys,/nodata
endelse
oplot, [0,1e4],[0,0], color=!black

; annular color

fn=where(finite(fann_mu),cntffinite)
nn=where(finite(nann_mu),cntnfinite)

if ~nofuv and ~nonuv and cntffinite gt 0 and cntnfinite gt 0 then begin
 min=min(fann_mu - nann_mu)
 max=max(fann_mu - nann_mu)
 plot, [r/60.], [fann_mu - nann_mu], psym=-sym(1, psize = psize),$
     charsize=chrsz, ytit = 'Annular FUV-NUV', xtit='R!de!n [arc minutes]',$
     xr = [0, max(r/60.)+0.2], /xs, $
     yr = [min-0.05, max+0.05],/ys,/nodata
 err=sqrt(fann_mu_e^2 + nann_mu_e^2)
 oploterror, [r/60.],[fann_mu - nann_mu],[err*0],[err],$
  psym=-sym(1, psize = psize), /nohat
endif else begin
 plot, [r/60.], [r/60.], psym=-sym(1, psize = psize),$
     charsize=chrsz, ytit = 'Annular FUV-NUV', xtit='R!de!n [arc minutes]',$
     xr = [0, max(r/60.)+0.1], /xs, $
     yr = [-1, 1],/ys,/nodata
endelse
oplot, [0,1e4],[0,0], color=!black

; text

chrsz=1
plot,[0,100],[0,100],xs=4,ys=4,/nodata, pos = [0.085, 0.01, 0.98, 0.33]

xyouts,0,85,'ID:  '+id,charsize=chrsz
if nintdata then ntime=SXPAR(nhdr,'EXPTIME')>0 else ntime=-999
if fintdata then ftime=SXPAR(fhdr,'EXPTIME')>0 else ftime=-999
xyouts,0,70,'FUV EXPTIME [s]:  '+strn(ftime),charsize=chrsz
xyouts,0,60,'NUV EXPTIME [s]:  '+strn(ntime),charsize=chrsz
xyouts,0,50,'R.A.  [J2K]:  '+strn(ra_cen[0],format='(f12.6)'),charsize=chrsz
xyouts,0,40,'DEC [J2K]:  '+strn(dec_cen[0],format='(f12.6)'),charsize=chrsz
xyouts,0,30,'SEMIMAJOR [arcmin]:  '+strn(semimajor/60,format='(f6.2)')$
 ,charsize=chrsz
xyouts,0,20,'RATIO (a/b):  '+strn(semimajor/semiminor,format='(f6.2)'),$
 charsize=chrsz  
xyouts,0,10,'P.A.:  '+strn(pa[0],format='(f6.2)'),charsize=chrsz

euler, ra_cen, dec_cen, l, b, 1
ebv=dust_getval(l,b,/interp)
A_NUV = 8.24*ebv - 0.67*(ebv^2)
A_FUV = 8.24*ebv 

xyouts,28,70,'TOT:  m(FUV!do!n)='$
 +strn(ftf_mag,format='(f5.2)')+'   m(FUV!ddr!n)='$
 +strn(ftf_mag-a_fuv,format='(f5.2)')$
 +'  ('+greek("plus_minus", /append)+$
 strn(ftf_mag_e,format='(F4.2)')+')',charsize=chrsz

xyouts,28,60,'TOT:  m(NUV!do!n)='$
 +strn(ntf_mag,format='(f5.2)')+'   m(NUV!ddr!n)='$
 +strn(ntf_mag-a_nuv,format='(f5.2)')$
 +'  ('+greek("plus_minus", /append)+$
 strn(ntf_mag_e,format='(F4.2)')+')',charsize=chrsz

uvcolor=ftf_mag-ntf_mag
err=sqrt(ftf_mag_e^2 + ntf_mag_e^2)
xyouts,28,50,'TOT:  FUV-NUV='+$
 strn(uvcolor,format='(f5.2)')$
 +'  ('+greek("plus_minus", /append)+strn(err,$
 format='(F4.2)')+')',charsize=chrsz

xyouts,28,40,'APR:  m(FUV!do!n)='$
 +strn(faf_mag,format='(f5.2)')+'   m(FUV!ddr!n)='$
 +strn(faf_mag-a_fuv,format='(f5.2)')$
 +'  ('+greek("plus_minus", /append)+$
 strn(faf_mag_e,format='(F4.2)')+')',charsize=chrsz

xyouts,28,30,'APR:  m(NUV!do!n)='$
 +strn(naf_mag,format='(f5.2)')+'   m(NUV!ddr!n)='$
 +strn(naf_mag-a_nuv,format='(f5.2)')$
 +'  ('+greek("plus_minus", /append)+$
 strn(naf_mag_e,format='(F4.2)')+')',charsize=chrsz

uvcolor=faf_mag-naf_mag
err=sqrt(faf_mag_e^2 + naf_mag_e^2)
xyouts,28,20,'APR:  FUV-NUV='+$
 strn(uvcolor,format='(f5.2)')$
 +'  ('+greek("plus_minus", /append)+strn(err,$
 format='(F4.2)')+')',charsize=chrsz

if nofuv then fmu_bg=!values.f_nan 
if nofuv then fmu_bg_e=0

xyouts,75,70,greek('mu',/append)+' BG FUV:  '+strn(fmu_bg,format='(f5.2)')$
 +'  ('+greek("plus_minus", /append)+strn(fmu_bg_e,format='(F4.2)')+')'$
 ,charsize=chrsz

if nonuv then nmu_bg=!values.f_nan 
if nonuv then nmu_bg_e=0

xyouts,75,60,greek('mu',/append)+' BG NUV:  '+strn(nmu_bg,format='(f5.2)')$
 +'  ('+greek("plus_minus", /append)+strn(nmu_bg_e,format='(F4.2)')+')'$
 ,charsize=chrsz

xyouts,75,50,'GAL E(B-V):  '+strn(ebv,format='(f5.3)'),charsize=chrsz
xyouts,75,40,'GAL A!dFUV!n:  '+strn(A_FUV,format='(f5.3)'),charsize=chrsz
xyouts,75,30,'GAL A!dNUV!n:  '+strn(A_NUV,format='(f5.3)'),charsize=chrsz


xyouts,85,3,systime(),charsize=.7

ps_end

;;;;;;;;;;;;;;;;;;;;;;
; convert to pdf & jpg

p=outpath+'/'
name=id+'_profile'
spawn,'convert -rotate -90 '+p+name+'.ps '+p+name+'.jpg'
spawn,'ps2pdf14 '+p+name+'.ps '+p+name+'.pdf'
spawn,'rm '+p+name+'.ps'

;;;;;;;;;;;;;;;;;;;;;;
; start image plots


filename=outpath+'/'+id+'_images.ps'
ps_start, filename=filename, xsize=10.5,ysize=8.,/inch,$
          /color,bit=8, xoffset=0.15, yoffset = 11,/landscape

!p.charsize=1.0
chrsz=0.75

!p.multi=[0,2,2]


; FUV 

if fdpic then begin

 loadct,0 & gamma_ct, 2 & rct

 sz=size(fdimg,/dim)
 delta=(fskyradius_out/fscale * 1.01) > 60 
 x1=fx0-delta > 0
 x2=fx0+delta < sz[0]-1
 y1=fy0-delta > 0
 y2=fy0+delta < sz[1]-1
 fdimg=fdimg[x1:x2,y1:y2]
 sz=size(fdimg,/dim)
 scale=fscale[0]/60.0 ; arcmins
 plotimage,fdimg,/preserve,charsize=chrsz,color=max(!d.n_colors),$
 imgxrange=[0-sz[0]/2,0+sz[0]/2]*scale,imgyrange=[0-sz[1]/2,0+sz[1]/2]*scale,$
 ytitle='Arc Minutes',title='FUV'

 setplotcolors

 ela=[fsemimajor/60.0,fsemiminor/60.0,0,0,float(90.+fpa,0)]
 tvellipse,ela[0],ela[1],ela[2],ela[3],ela[4],/data,$
   linestyle=1,color=!red, thick=3

 ratio=fsemiminor/fsemimajor

 eli=[fskyradius_out/60.0,ratio*fskyradius_out/60.0,0,0,float(90.+fpa,0)]
 tvellipse,eli[0],eli[1],eli[2],eli[3],eli[4],/data,$
   linestyle=2,color=!black, thick=1

 elo=[fskyradius_in/60.0,ratio*fskyradius_in/60.0,0,0,float(90.+fpa,0)]
 tvellipse,elo[0],elo[1],elo[2],elo[3],elo[4],/data,$
   linestyle=2,color=!black, thick=1

endif else begin

 plot,[0,1],[0,1],/nodata,xs=4,ys=4
 xyouts,[0.32],[0.5],'No FUV Image',charsize=1

endelse


; NUV 

if ndpic then begin

 loadct,0 & gamma_ct, 2 & rct

 sz=size(ndimg,/dim)
 delta=(nskyradius_out/nscale * 1.01) > 60 
 x1=fx0-delta > 0
 x2=fx0+delta < sz[0]-1
 y1=fy0-delta > 0
 y2=fy0+delta < sz[1]-1
 ndimg=ndimg[x1:x2,y1:y2]
 sz=size(ndimg,/dim)
 scale=nscale[0]/60.0
 plotimage,ndimg,/preserve,charsize=chrsz,color=max(!d.n_colors),$
 imgxrange=[0-sz[0]/2,0+sz[0]/2]*scale,imgyrange=[0-sz[1]/2,0+sz[1]/2]*scale,$
 ytitle='Arc Minutes',title='NUV'

 setplotcolors

 ela=[nsemimajor/60.0,nsemiminor/60.0,0,0,float(90.+npa,0)]
 tvellipse,ela[0],ela[1],ela[2],ela[3],ela[4],/data,$
   linestyle=1,color=!red, thick=3

 ratio=nsemiminor/nsemimajor

 eli=[nskyradius_out/60.0,ratio*nskyradius_out/60.0,0,0,float(90.+npa,0)]
 tvellipse,eli[0],eli[1],eli[2],eli[3],eli[4],/data,$
   linestyle=2,color=!black, thick=1

 elo=[nskyradius_in/60.0,ratio*nskyradius_in/60.0,0,0,float(90.+npa,0)]
 tvellipse,elo[0],elo[1],elo[2],elo[3],elo[4],/data,$
   linestyle=2,color=!black, thick=1

endif else begin

 plot,[0,1],[0,1],/nodata,xs=4,ys=4
 xyouts,[0.32],[0.5],'No NUV Image',charsize=1

endelse


; COLOR (make use of last derived values)

if xdpic then begin

 loadct,0

 xdjpg=xdjpg[*,x1:x2,y1:y2]
 plotimage,xdjpg,/preserve,charsize=chrsz,$
 imgxrange=[0-sz[0]/2,0+sz[0]/2]*scale,imgyrange=[0-sz[1]/2,0+sz[1]/2]*scale,$
 ytitle='Arc Minutes',title='Masked Composite'

 setplotcolors

 tvellipse,ela[0],ela[1],ela[2],ela[3],ela[4],/data,$
   linestyle=1,color=!red, thick=3

 tvellipse,eli[0],eli[1],eli[2],eli[3],eli[4],/data,$
   linestyle=2,color=!white, thick=1

 tvellipse,elo[0],elo[1],elo[2],elo[3],elo[4],/data,$
   linestyle=2,color=!white, thick=1

endif else begin

 plot,[0,1],[0,1],/nodata,xs=4,ys=4
 xyouts,[0.3],[0.5],'No Composite Image',charsize=1

endelse


; DSS

if dssdata then begin

 loadct,0 & gamma_ct, 2 & rct

 delta_dss = (delta*nscale)/dssscale
 sz_dss=size(dss,/dim)
 x1_dss=dssx0-delta_dss[0] > 0
 x2_dss=dssx0+delta_dss[0] < sz_dss[0]-1
 y1_dss=dssy0-delta_dss[0] > 0
 y2_dss=dssy0+delta_dss[0] < sz_dss[1]-1
 hextract, dss, dhdr, x1_dss,x2_dss,y1_dss,y2_dss
 sz=size(dss,/dim)
 dss = asinh(dss*1.)
 meanclip, dss, dss_sky, dss_sigma
 min = dss_sky*0.98
 max=max(dss) < (dss_sky+(20*dss_sigma))
 scale=dssscale/60.0
 plotimage,dss,range=[min,max],/preserve,charsize=chrsz,$
  color=max(!d.n_colors),$
  imgxrange=[0-sz[0]/2,0+sz[0]/2]*scale,imgyrange=[0-sz[1]/2,0+sz[1]/2]*scale,$
  ytitle='Arc Minutes',title='POSS2-R'

 setplotcolors

 tvellipse,ela[0],ela[1],ela[2],ela[3],ela[4],/data,$
   linestyle=1,color=!red, thick=3

endif else begin

 plot,[0,1],[0,1],/nodata,xs=4,ys=4
 xyouts,[0.31],[0.5],'No POSS2 Image',charsize=1

endelse

!p.multi=0

ps_end

;;;;;;;;;;;;;;;;;;;;;;
; convert to pdf & jpg

p=outpath+'/'
name=id+'_images'
spawn,'convert -rotate -90 '+p+name+'.ps '+p+name+'.jpg'
spawn,'ps2pdf14 '+p+name+'.ps '+p+name+'.pdf'
spawn,'rm '+p+name+'.ps'

;;;;;;;;;;;;
; all done

return
end
