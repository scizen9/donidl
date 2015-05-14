pro galex_como, id, ra, dec, diam,  base, path, $
                 outdir = outdir, nuv=nuv, fuv=fuv, $
                 trim=trim, status=status, pixscl_factor = pixscl_factor, $
                 stitch=stitch, visitext=visitext, silent = silent
;+
; NAME:
;      GALEX_COMO v1.5
; PURPOSE:
;      Coadd and mosaic builder
;
;
; id = output prefix
; ra,dec = center decimal degrees (scalar) 
; diam = arcmin width & height (scalar or array, e.g. [30,20])
; base = GALEX base (scalar or array)
; path = path to GALEX data (scalar or array)
; outdir = directory to place coadded products
; nuv,fuv = band to build
; trim = FOV distance to trim beyond (arcmins default=36, scalar or array)
; pixscl_factor = increase pixel scale by factor  
;       default=1 --> 1.5' pixels (normal)
;       ex: pixscl_factor=4 --> 6'pixels
; stitch = stitch fields in order instead of coadding them
;      (can make a nicer mosaic when there are lots of reflections)
; visitext = preserve visits as extensions          
; status = 1 (success) or 0 (fail)

version='1.5'

;location of galex_como related file template_hdr.txt 
location='/Users/neill/idl/seibert/galex/galex_como'

;;;;;;;;;;
;checks

if ( N_params() lt 6 ) then begin
 print,'Error: Syntax'
 print,'build_coadd, id, ra, dec, diam,  base, path, $'
 print,'             [outdir=,nuv=,fuv=,trim=,status=]'
 status=0
 return
endif

if n_elements(base) ne n_elements(path) then begin
 print, 'Error: base and path must have same number of elements'
 status=0
 return
endif

;;;;;;;;;;
;defaults

if not keyword_set(trim) then trim=fltarr(n_elements(base))*0+36.0
if n_elements(trim) ne n_elements(base) then begin
 if n_elements(trim) eq 1 then begin
   ttrim=fltarr(n_elements(base))*0+trim
   trim=ttrim
 endif else begin
   print, 'Error: trim must must have same number of elements as base or 1'
   status=0 
   return
 endelse
endif 


if not keyword_set(pixscl_factor) then pixscl_factor = 1.0
pixscl_factor = pixscl_factor*1.0
if pixscl_factor ne 1.0 and $
   pixscl_factor ne 2.0 and $
   pixscl_factor ne 4.0 and $
   pixscl_factor ne 6.0 and $
   pixscl_factor ne 8.0 and $
   pixscl_factor ne 10.0 and $
   pixscl_factor ne 12.0 and $
   pixscl_factor ne 14.0 and $
   pixscl_factor ne 16.0 then begin
 print, 'Error: pixel scale factor must be 1, 2, 4, 6, 8, 10, 12, 14 or 16'
 status=0
 return
endif
if not keyword_set(outdir) then outdir='.'
outdir=outdir+'/'
band='n'
bnd=1
if keyword_set(fuv) then begin 
 band='f'
 bnd=2
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;define count and relative response input map names

cntfile=path+'/'+base+'-'+band+'d-cnt.fit*'
rrhrfile=path+'/'+base+'-'+band+'d-rrhr.fit*'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;define count, relative response. and intensity output map names

outcntfile=outdir+id+'-'+band+'d-cnt.fits'
outrrfile=outdir+id+'-'+band+'d-rrhr.fits'
outintfile=outdir+id+'-'+band+'d-int.fits'

;;;;;;;;;;;;;;;;;;;;;;
;define size of coadd
xnpix=ceil(diam[0]*60./(1.5*pixscl_factor))
if n_elements(diam) eq 1 then ynpix=xnpix else $
 ynpix=ceil(diam[1]*60./(1.5*pixscl_factor))

;;;;;;;;;;;;;;;;;;;
;establish header
readfmt,location+'/template_hdr.txt','a80',refhdr,/silent

sxaddpar,refhdr,'NAXIS1',xnpix
sxaddpar,refhdr,'NAXIS2',ynpix
sxaddpar,refhdr,'CRPIX1',xnpix/2.
sxaddpar,refhdr,'CRPIX2',ynpix/2.
sxaddpar,refhdr,'CRVAL1',ra
sxaddpar,refhdr,'CRVAL2',dec
sxaddpar,refhdr,'BAND',bnd
sxaddpar,refhdr,'RA_CEN',ra
sxaddpar,refhdr,'DEC_CEN',dec
sxaddpar,refhdr,'OBJECT',id

if pixscl_factor ne 1.0 then begin
 cdelt1=sxpar(refhdr,'CDELT1')
 cdelt2=sxpar(refhdr,'CDELT2')
 sxaddpar,refhdr,'CDELT1',cdelt1*pixscl_factor
 sxaddpar,refhdr,'CDELT2',cdelt2*pixscl_factor
endif

int=fltarr(xnpix,ynpix)*0
cnt=int
rrhr=int

exptime=0.
ii=0 ; coadd counter 

for i=0,n_elements(base)-1 do begin

 if (not file_exist(cntfile[i])) or (not file_exist(rrhrfile[i])) then goto,skipout
 
 ii=ii+1

 if not keyword_set(silent) then print, 'como: '+$
  strcompress(i+1,/rem)+' of '+strcompress(n_elements(base),/rem)+$
  ': '+base[i]+'-'+band

 hdr=headfits(cntfile[i])
 racen=sxpar(hdr,'AVASPRA')
 deccen=sxpar(hdr,'AVASPDEC')
 naxis1=sxpar(hdr,'NAXIS1')
 naxis2=sxpar(hdr,'NAXIS2')
 sz=[2,naxis1,naxis2]
 adxy,hdr,racen,deccen,x,y  
 DIST_CIRCLE, d, sz[1],x,y 
 crop=where(d gt trim[i]*60./1.5)

 srrhr=mrdfits(rrhrfile[i],0,rrhrhdr,/silent) > 0 
 srrhr[crop]=0 ;trim edge

 srrhr = frebin(srrhr, sz[1]/pixscl_factor, sz[2]/pixscl_factor)
 hrebin, rrhrhdr, outsize = [sz[1]/pixscl_factor, sz[2]/pixscl_factor]

 hastrom_err,srrhr,rrhrhdr,refhdr,missing=0, $
    errmsg = errmsg, interp = 0, degree=3, ngrid=20

 if errmsg ne '' then begin
  if not keyword_set(silent) then print, errmsg
  if not keyword_set(silent) then print, 'skipping '+base[i]
  ii = ii-1
  goto, skipout
 endif
 if keyword_set(stitch) then begin
  good=where(srrhr gt 0 and rrhr le 0, countgood)
  if countgood eq 0 then begin
   ii = ii-1
   if not keyword_set(silent) then print, 'skipping '+base[i]
   goto, skipout
  endif
  rrhr[good]=srrhr[good]    
 endif else rrhr=rrhr+srrhr
 ;delvarx,srrhr,/free


 scnt=mrdfits(cntfile[i],0,chdr,/silent) > 0
 scnt[crop]=0 ;trim edge

 scnt = frebin(scnt, sz[1]/pixscl_factor, sz[2]/pixscl_factor, /total)
 hrebin, chdr, outsize = [sz[1]/pixscl_factor, sz[2]/pixscl_factor]

 hastrom_err,scnt,chdr,refhdr,missing=0, $
  errmsg = errmsg, interp = 0, degree=3, ngrid=20

 if errmsg ne '' then begin
  if not keyword_set(silent) then print, errmsg
  if not keyword_set(silent) then print, 'skipping '+base[i]
  ii = ii-1
  goto, skipout
 endif
 if keyword_set(stitch) then begin
  cnt[good]=scnt[good]    
 endif else cnt=cnt+scnt
 ;delvarx,scnt,d,/free

 sexptime=sxpar(chdr,'EXPTIME')
 exptime=exptime+sexptime
 grelease=sxpar(chdr,'GRELEASE')
 obsdate=sxpar(chdr,'OBS-DATE')
 eclipse=sxpar(chdr,'ECLIPSE')

 if keyword_set(visitext) then begin
  a=where(srrhr gt 0,counta)
  if counta gt 0 then begin
   sint=scnt*0.
   sint[a]=scnt[a]/srrhr[a]
   if ii eq 1 then begin 
    mwrfits,sint,outintfile,chdr,/create ;primary
    mwrfits,scnt,outcntfile,chdr,/create ;primary
    mwrfits,srrhr,outrrfile,rrhrhdr,/create ;primary
   endif
   mwrfits,sint,outintfile,chdr
   mwrfits,scnt,outcntfile,chdr
   mwrfits,srrhr,outrrfile,rrhrhdr
  endif
 endif

 delvarx,sint,srrhr,scnt,good,/free

 sxaddpar,refhdr,'DATE'+string(ii,format='(i4.4)'),obsdate
 sxaddpar,refhdr,'ECL'+string(ii,format='(i4.4)'),eclipse
 sxaddpar,refhdr,'EXPT'+string(ii,format='(i4.4)'),sexptime
 sxaddpar,refhdr,'GREL'+string(ii,format='(i4.4)'),grelease
 sxaddpar,refhdr,'BASE'+string(ii,format='(i4.4)'),base[i]
     
 skipout:

endfor

sxaddpar,refhdr,'EXPTIME',exptime
sxaddpar,refhdr,'MAXRR',max(rrhr)>0
sxaddpar,refhdr,'MINRR',min(rrhr)>0
sxaddpar,refhdr,'MEDRR',median(rrhr[where(rrhr ge 0)])
sxaddpar,refhdr,'NADDED',ii
sxaddpar,refhdr,'COMOVER',version

a=where(rrhr gt 0,counta, complement=b, ncomplement=countb)
if counta eq 0 then begin
 status=0
 return
endif
int[a]=cnt[a]/rrhr[a]
if countb gt 0 then int[b]=!values.f_nan




if keyword_set(visitext) then begin ; overwrite primary
 modfits, outintfile, int, refhdr, EXTEN_NO = 0;, ERRMSG=err
 modfits, outcntfile, cnt, refhdr, EXTEN_NO = 0;, ERRMSG=err 
 modfits, outrrfile, rrhr, refhdr, EXTEN_NO = 0;, ERRMSG=err 
endif else begin
 mwrfits,cnt,outcntfile,refhdr,/create 
 mwrfits,rrhr,outrrfile,refhdr,/create 
 mwrfits,int,outintfile,refhdr,/create 
endelse

spawn,'gzip -f '+outdir+id+'*.fits',err
status=1

end
