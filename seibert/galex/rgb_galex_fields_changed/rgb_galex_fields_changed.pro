pro rgb_galex_fields, fuvfile, nuvfile, target, $
                      full=full, half=half, fourth=fourth, eighth=eighth, $
                      thumb=thumb, nsigma=nsigma, verbose=verbose, $
                       no_annotation=no_annotation
;+
; NAME:
;  rgb_galex_fields
;
;
; PURPOSE:
;  Generate RGB composite jpeg images from GALEX FUV and NUV intensity maps.
;  RED=NUV, BLUE=FUV, GREEN=average of FUV & NUV.
;
;  Images are energy and square root scaled and the color stretch is
;  nsigma_min to nsigma_max above sky level.
;
;  Defaults to full resolution images.
;  Optionally creates 1/2, 1/4 and 1/8 resolution images.
;  Will process single files or lists of files as arrays.
;
; CATEGORY:
;  image processing
;
;
; CALLING SEQUENCE:
;  rgb_galex_fields, fuv, nuv, target [,/full, /half, /fourth,
;                   /eighth, /thumb, nsigma=nsigma]
;
; INPUTS:
;  fuvfile=FUV intensity fits file(s) (...fd-int.fits)
;  nuvfile=NUV intensity fits file(s) (...nd-int.fits)
;  target =name(s) of jpeg files to create and title of image
;          Field name is recommended.
;          
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;  One of these must be specified otherwise defaults to full
;  full  = full resolution image
;  half  = 1/2  scale
;  fourth= 1/4  scale
;  eighth= 1/8  scale
;  thumb = 1/32 scale
;  nsigma= [fuvmin,fuvmax,nuvmin,nuvmax] 
;          - sky sigma clipping for image color stretch 
;          (default value = [2,12])
;          [3,8,3,8] recommended for coadded deep fields
;          [1,6,1,6] if one want maximum faint details from coadded deep fields
;  verbose=set keyword to complain if an error occurs
;          and give status of jpeg file creation
;
; OUTPUTS:
;  full scale jpeg -> target_2color.jpg (3840 x 3840)       
;
; OPTIONAL OUTPUTS:
;  1/2 scale jpeg  -> target_2color_large.jpg (1920 x 1920)
;  1/4 scale jpeg  -> target_2color_medium.jpg (960 x 960)
;  1/8 scale jpeg  -> target_2color_small.jpg  (480 x 480)
;  1/32 scale jpeg -> target_2color_thumb.jpg  (120 x 120)
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;  uses file_exist.pro and astrolib routines
;  assumes Unix platform
;  
;
; EXAMPLE:
;
;  This will generate three images called:
;    ELAISS1_0001_2color.jpg, 
;    ELAISS1_0001_2color_medium.jpg  
;    ELAISS1_0001_2color_small.jpg
;
;  >fuvfile='ELAISS1_0001-fd-int.fits'
;  >nuvfile='ELAISS1_0001-fd-int.fits'
;  >target ='ELAISS1_0001'
;
;  >rgb_galex_fields, fuvfile, nuvfile, target, $
;  >                  /full, /fourth, /eighth
;   
;
;
; MODIFICATION HISTORY:
;  M. Seibert 9/19/2003
;  update 12/5/2003 MHS
;
;  Tweaked by Tim Conrow at various times. See CVS history.
;
;-

;-----------------------------------------------------
; some quick checks

if n_params() lt 3 then begin
 print,'*** ERROR: Requires at least 3 parameters.'
 return
endif

if n_elements(nuvfile) ne n_elements(fuvfile) or $
   n_elements(target) ne n_elements(fuvfile) then begin $
  if keyword_set(verbose) then $
    print,'*** ERROR: Image and target lists must have same number of elements.'
  return
endif

if not keyword_set(full) and not keyword_set(fourth) $
  and not keyword_set(eighth)and not keyword_set(thumb) then full=1

if not keyword_set(nsigma) then nsigma=[2,12,2,12]
if n_elements(nsigma) ne 4 then begin $
 if keyword_set(verbose) then $
  print,'*** ERROR: nsigma must be a 4 element array: [fuvmin,fuvmax,nuvmin,nuvmax].'
 return
endif

;-----------------------------------------------------

for i=0,n_elements(fuvfile)-1 do begin 

 fltpos = strpos( fuvfile[i], 'fd-int')

 be=file_exist(fuvfile[i])
 re=file_exist(nuvfile[i])

 if be or re then begin  ; if fits files exists

  if be     then  begin
    blue = mrdfits(fuvfile[i],0,bh)
    blue = (blue/1500.)  ; scale ~energy
    blue = sqrt(smooth(blue,2) >0) ; lightly smooth the FUV image
  endif
  if re     then  begin
    red  = mrdfits(nuvfile[i],0,rh)
    red  = (red/2400.)
    red  = sqrt(red > 0)
  endif
  if re and be then begin
    hastrom,red,rh,bh               ; <--- probably not needed
    green = (0.5*blue + 0.5*red)
    ;green = (0.01*blue + 0.99*red)
    hdr   = rh
  endif
  if not be then  begin
    blue = red
    green= red
    hdr  = rh
  endif
  if not re then  begin
    red  = blue
    green= blue
    hdr  = bh
  endif


  ;object=sxpar(hdr,'OBJECT')
  exptime = sxpar(hdr,'EXPTIME')
  ra_cen  = sxpar(hdr,'RA_CENT')
  dec_cen = sxpar(hdr,'DEC_CENT')
  obs_date= sxpar(hdr,'OBS-DATE')

  sky,red,   rsky, rskysigma, /silent   ; quick method for sky & sigma
  sky,blue,  bsky, bskysigma, /silent
  sky,green, gsky, gskysigma, /silent

  rnsig = nsigma[2:3] ; lower,upper nsigma clip
  gnsig = [avg([nsigma[0],nsigma[2]]),avg([nsigma[1],nsigma[3]])]
  bnsig = nsigma[0:1]

  r = bytscl(red,  min=rsky+rnsig[0]*rskysigma,max=rsky+rnsig[1]*rskysigma)
  g = bytscl(green,min=gsky+gnsig[0]*gskysigma,max=gsky+gnsig[1]*gskysigma)
  b = bytscl(blue, min=bsky+bnsig[0]*bskysigma,max=bsky+bnsig[1]*bskysigma)

  sz=size(red)

  set_plot,'z' ; generate as virtual image because I write to image
               ; can be avoided if no text in border needed
  device,set_resolution=[sz[1],sz[2]]

  erase

  targ = target[i]
  if not be then targ = targ + " (NUV)"
  if not re then targ = targ + " (FUV)"

  if not keyword_set(no_annotation) then begin

  xyouts,200,3650,targ,charsize=8,charthick=10,/dev
  xyouts,200,3500,'!4a!x: '+strn(ra_cen,format='(f9.5)'),$
         charsize=8,charthick=10,/dev
  xyouts,200,3350,'!4d!x: '+strn(dec_cen,format='(f10.6)'),$
         charsize=8,charthick=10,/dev
  xyouts,200,3200,'Exp: '+strn(exptime,format='(f9.2)'),$
         charsize=8,charthick=10,/dev
  xyouts,200,200,obs_date,charsize=8,charthick=10,/dev
 
  txtpln=tvrd()
  t=where(txtpln gt 1)
  r[t]=max(r)
  g[t]=max(g)
  b[t]=max(b)

  endif  

  image=bytarr(3,sz[1],sz[2])
  image[0,*,*]=r
  image[1,*,*]=g
  image[2,*,*]=b

  if keyword_set(full) then begin 
   jname=target[i]+'_2color.jpg' & quality=75
   write_jpeg,jname,image,true=1,quality=quality
   if keyword_set(verbose) then print, '| Wrote '+jname
  endif

  if keyword_set(half) then begin
   img_med=rebin(image,3,1920,1920)
   jname=target[i]+'_2color_large.jpg'
   write_jpeg,jname,img_med,true=1,quality=75
   if keyword_set(verbose) then print, '| Wrote '+jname
  endif

  if keyword_set(fourth) then begin
   img_med=rebin(image,3,960,960)
   jname=target[i]+'_2color_medium.jpg'
   write_jpeg,jname,img_med,true=1,quality=100
   if keyword_set(verbose) then print, '| Wrote '+jname
  endif

  if keyword_set(eighth) then begin
   img_sm=rebin(image,3,480,480)
   jname=target[i]+'_2color_small.jpg'
   write_jpeg,jname,img_sm,true=1,quality=100
   if keyword_set(verbose) then print, '| Wrote '+jname
  endif

  if keyword_set(thumb) then begin
   img_sm=rebin(image,3,120,120)
   jname=target[i]+'_2color_thumb.jpg'
   write_jpeg,jname,img_sm,true=1,quality=100
   if keyword_set(verbose) then print, '| Wrote '+jname
  endif

  set_plot,'x'

endif else if keyword_set(verbose) then $
  print,'Cannot find either file:',fuvfile[i],nuvfile[i] 

endfor 

end
