pro rgb_glossy, fuvfile, nuvfile, target, $
                full=full, half=half, fourth=fourth, eighth=eighth, $
                nsigma=nsigma, text=text, nocrop=nocrop
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
;  Pptionally creates 1/4 and 1/8 resolution images.
;  Will process single files or lists of files as arrays.
;
; CATEGORY:
;  image processing
;
;
; CALLING SEQUENCE:
;  rgb_galex_fields, fuv, nuv, target [,/full, /half, /fourth, /eighth]
;
;
; INPUTS:
;  fuvfile=FUV intensity fits file(s) (...fd-int.fits)
;  nuvfile=NUV intensity fits file(s) (...nd-int.fits)
;  target =name(s) of jpeg files to create
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;  One of these must be specified otherwise defaults to full
;  full  = full resolution image
;  half  = 1/2 scale
;  fourth= 1/4 scale
;  eighth= 1/8 scale
;  nsigma=[min,max] - sigma clipping for image color stretch 
;                     (default value = [2,12])
;
;
; OUTPUTS:
;  full scale jpeg -> target_2color.jpg (3840 x 3840)       
;
; OPTIONAL OUTPUTS:
;  1/2 scale jpeg  -> target_2color_large.jpg (1920 x 1920)
;  1/4 scale jpeg  -> target_2color_medium.jpg (960 x 960)
;  1/8 scale jpeg  -> target_2color_small.jpg  (480 x 480)
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;  uses file_exist.pro and astrolib routines
;  assumes Unix platform
;  
;
; EXAMPLE:
;
;  This will generate three images called:
;  pretty_2color.jpg , pretty_2color_medium.jpg and pretty_2color_small.jpg
;
;  fuvfile='fuv-int.fits'
;  nuvfile='nuv-int.fits'
;  target ='pretty'
;
;  rgb_galex_fields, fuvfile, nuvfile, target, $
;                    /full, /fourth, /eighth
;   
;
;
; MODIFICATION HISTORY:
;  M. Seibert 9/19/2003
;
;-

;-----------------------------------------------------
; some quick checks

if n_params() lt 3 then begin
 print,'ERROR: Requires at least 3 parameters.'
 return
endif

if n_elements(nuvfile) ne n_elements(fuvfile) or $
   n_elements(target) ne n_elements(fuvfile) then begin $
  print,'ERROR: Image and target lists must have same number of elements.'
  return
endif

if not keyword_set(full) and not keyword_set(fourth) $
  and not keyword_set(eighth) then full=1

if not keyword_set(nsigma) then nsigma=[2,12,2,12]
if n_elements(nsigma) ne 4 then begin $
 print,'ERROR: nsigma must be 4 element array: [fuvmin,fuvmax,nuvmin,nuvmax].'
 retun
endif

;-----------------------------------------------------

for i=0,n_elements(fuvfile)-1 do begin 

 fltpos = strpos( fuvfile[i], 'fd-int')

 a=file_exist(fuvfile[i])
 b=file_exist(nuvfile[i])

 if a and b then begin  ; if fits files exists

  blue=mrdfits(fuvfile[i],0,bh)
  red=mrdfits(nuvfile[i],0,rh)
  ;hextract,blue,bh,200,3639,200,3639
  hastrom,red,rh,bh               

  sz=size(red)
  ;crop the circles
  
  if not keyword_Set(nocrop) then begin
   DIST_CIRCLE, d, sz[1] 
   a=where(d gt 1450)
   blue[a]=0
   red[a]=0
  endif

  exptime=sxpar(bh,'EXPTIME')
  ra_cen=sxpar(bh,'RA_CENT')
  dec_cen=sxpar(bh,'DEC_CENT')
 
  blue=(blue/1500.)  ; scale ~energy
  red=(red/2400.)

  blue = sqrt(smooth(blue,2) >0) ; lightly smooth the FUV image
  red  = sqrt(red > 0)
  green = (0.5*blue + 0.5*red)

  sky,red, rsky, rskysigma,/silent   ; quick method for sky & sigma
  sky,blue, bsky, bskysigma,/silent
  sky,green, gsky, gskysigma,/silent

  rnsig=nsigma[2:3] ; lower,upper nsigma clip
  gnsig=[avg([nsigma[0],nsigma[2]]),avg([nsigma[1],nsigma[3]])]
  bnsig=nsigma[0:1]

  r=bytscl(red,min=rsky+rnsig[0]*rskysigma,max=rsky+rnsig[1]*rskysigma)
  g=bytscl(green,min=gsky+gnsig[0]*gskysigma,max=gsky+gnsig[1]*gskysigma)
  b=bytscl(blue,min=bsky+bnsig[0]*bskysigma,max=bsky+bnsig[1]*bskysigma)
  ;g=(0.5*b + 0.5*r)

  if keyword_Set(text) then begin
   set_plot,'z' ; generate as virtual image because I write to image
               ; can be avoided if no text in border needed
   device,set_resolution=[sz[1],sz[2]]
   erase

  y=3550
  if target eq 'M31' then y=4450
  xyouts,200,y,'GALEX',charsize=8,charthick=8,/dev
  xyouts,200,y-125,'!3'+text[i],charsize=6,charthick=6,/dev
  ;xyouts,200,3500,'!4a!x: '+strn(ra_cen,format='(f9.5)'),$
  ;       charsize=8,charthick=10,/dev
  ;xyouts,200,3350,'!4d!x: '+strn(dec_cen,format='(f10.6)'),$
  ;       charsize=8,charthick=10,/dev
  ;xyouts,200,3200,'Exp: '+strn(exptime,format='(f9.2)'),$
  ;       charsize=8,charthick=10,/dev 

   txtpln=tvrd()
   t=where(txtpln gt 1)
   r[t]=max(r);74
   g[t]=max(g);114
   b[t]=max(b);173

 endif 

  image=bytarr(3,sz[1],sz[2])
  image[0,*,*]=r
  image[1,*,*]=g
  image[2,*,*]=b


  ;read_jpeg,'../logo_large.jpg',logo
  ;lsz=size(logo)
  ;y=3550
  ;if target eq 'M31' then y=4450
  ;image[0,200:200+lsz[2]-1,y:y+lsz[3]-1]=logo[0,*,*]
  ;image[1,200:200+lsz[2]-1,y:y+lsz[3]-1]=logo[1,*,*]
  ;image[2,200:200+lsz[2]-1,y:y+lsz[3]-1]=logo[2,*,*]


  if keyword_set(full) then begin 
   jname=target[i]+'_2color_full.jpg' & quality=100
   write_jpeg,jname,image,true=1,quality=quality
   print, 'wrote '+jname
  endif

  if keyword_set(half) then begin
   img_med=rebin(image,3,sz[1]/2,sz[2]/2)
   jname=target[i]+'_2color_half.jpg'
   write_jpeg,jname,img_med,true=1,quality=100
   print, 'wrote '+jname
  endif

  if keyword_set(fourth) then begin
   img_med=rebin(image,3,sz[1]/4,sz[2]/4)
   jname=target[i]+'_2color_fourth.jpg'
   write_jpeg,jname,img_med,true=1,quality=100
   print, 'wrote '+jname
  endif

  if keyword_set(eighth) then begin
   img_sm=rebin(image,3,sz[1]/8,sz[2]/8)
   jname=target[i]+'_2color_eighth.jpg'
   write_jpeg,jname,img_sm,true=1,quality=100
   print, 'wrote '+jname
  endif

  set_plot,'x'

endif else print,'Can not find files:',fuvfile[i],nuvfile[i] 

endfor 

print,'finished'

end
