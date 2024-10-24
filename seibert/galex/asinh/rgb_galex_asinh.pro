pro rgb_galex_asinh, fuvfile, nuvfile, target, $
                     bgsub=bgsub,bgvals=bgvals,$
                     greenmix=greenmix, scales=scales,brite=brite, $
                     nonlinearity=nonlinearity, origin=origin, $
                     full=full, half=half, fourth=fourth, eighth=eighth, $
                     thumb=thumb, verbose=verbose, $
                     no_annotations=no_annotations, refhdr=refhdr,$
                     quality=quality,tif=tif,fuvshift=fuvshift, $
                     nofuvsmooth = nofuvsmooth, nuvsmooth = nuvsmooth, $
                     bgouter = bgouter

;+
; NAME:
;  rgb_galex_asinh
;
;
; PURPOSE:
;  Generate RGB composite jpeg images from GALEX FUV and NUV intensity maps.
;  RED=NUV, BLUE=FUV, GREEN=combination of FUV & NUV.
;
;  Images are lambda*flamda and asinh scaled/fit using Wherry,
;  Blanton, Hogg IDL routines of the the Lupton, et al. algorithm.
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
;                   /eighth, /thumb, greenmix=greenmix, scales=scales,
;                   brite=brite, nonlinearity=nonlinearity, origin=origin
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
;  bgsub = subtract a 'global' background level
;  greenmix = blue and red combination level for green where red and
;             blue are the lambda*flamda scaled
;             values. Default=[.2,.8]
;  brite = scale factor applied to all three RGB planes. Default=1
;          if bgsub then brite=(1+(sxpar(hdr,'EXPTIME')/1e4))<3,
;          setting brite will override this.
;  verbose = set keyword to complain if an error occurs
;          and give status of jpeg file creation
;  no_annotations = image only with no markups.
;  refhdr = align image to a reference header. Useful for time-sliced data.
;  quality = jpeg compression quality for full and half res images.
;            Default = 75. Other resolutions are always quality=100.
;
;  The following keywords are associated with the Wherry et al routines.
;  ----------------------------------------------------------------------
;  scales = scaling applied to red green and blue. Default=[.085,.09,.085]
;  nonlinearity = nonlinearity factor used in asinh scaling. Default=2.5
;  origin = Limits the pixel values of the image to a 'box', so that
;           the colors do not saturate to white but to a specific color. 
;           Default=[0,0,0]
;
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
;  M. Seibert 12/24/2004
;  M. Seibert 12/06/2005
;
;
;-

;-----------------------------------------------------
; some quick checks

if keyword_set(verbose) then print, '| Starting asinh image creation' 

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
  and not keyword_set(eighth) and not keyword_set(thumb) $
  and not keyword_set(tif) then full=1

if not keyword_set(greenmix) then greenmix=[.2,.8];[.5,.5]
if n_elements(greenmix) ne 2 then begin $
 if keyword_set(verbose) then $
  print,'*** ERROR: greenmix must be a 2 element array: [fraction_FUV,fraction_NUV].'
 return
endif


if not keyword_set(scales) then scales=[.085,.09,.085]
if n_elements(scales) ne 3 then begin $
 if keyword_set(verbose) then $
  print,'*** ERROR: scales must be a 3 element array: [red,green,blue].'
  print,'*** ERROR: default value is [.085,.09,.085].'
 return
endif

if not keyword_set(quality) then quality=75

fexptime=0
nexptime=0

;-----------------------------------------------------

for i=0,n_elements(fuvfile)-1 do begin 

 fltpos = strpos( fuvfile[i], 'fd-int')

 be=file_exist(fuvfile[i])
 re=file_exist(nuvfile[i])

 if be or re then begin  ; if fits files exists

  if be     then  begin
    blue = mrdfits(fuvfile[i],0,bh)
    fexptime = sxpar(bh,'EXPTIME')
    if keyword_set(refhdr) then hastrom,blue,bh,refhdr 
    blue= blue*1.4*1525  ; scale as lambda*flambda
    if not keyword_set(nofuvsmooth) then $
     blue = smooth(blue,2)>0 ; lightly smooth the FUV image
    if keyword_set(fuvshift) then blue=shift(blue,fuvshift) 
  endif
  if re     then  begin
    red  = mrdfits(nuvfile[i],0,rh)
    nexptime = sxpar(rh,'EXPTIME')
    if keyword_set(refhdr) then hastrom,red,rh,refhdr
    red=  red*0.206*2297
    if keyword_set(nuvsmooth) then red = smooth(red,2)>0
  endif
  if re and be then hdr = rh
  if not be then begin 
    blue = red
    hdr  = rh
    greenmix=[0.5,0.5]
    scales=[.085,.085,.085]    
  endif
  if not re then  begin
    red  = blue
    hdr  = bh
    greenmix=[0.5,0.5]
    scales=[.085,.085,.085]
  endif


  if keyword_set(bgsub) then begin

   sz = size(red)
   mindim = sz[1] < sz[2]
   skycirc = fix(0.73*(mindim/2.))

   if not keyword_set(bgvals) and not keyword_set(bgouter) then begin
    if keyword_set(verbose) then print, '| Background subtracting' 
    sky,red,   rsky, rskysigma,circ=skycirc, /silent; quick sky & sigma
    sky,blue,  bsky, bskysigma,circ=skycirc, /silent
   
    if rsky le 0 or bsky le 0 then begin
     if keyword_set(verbose) then print, '| Computing alternative background' 
     dist_circle, circle, mindim, mindim/2, mindim/2
     area=where(circle le skycirc)
     delvarx,circle
     meanclip,red[area],rsky,clip,clipsig=3
     meanclip,blue[area],bsky,clip,clipsig=3
     if keyword_set(verbose) then print, '| ',bsky, rsky
    endif
   endif

   if keyword_set(bgouter) then begin
     dist_circle, circle, mindim, mindim/2, mindim/2
     area=where(circle gt skycirc)
     delvarx,circle
     meanclip,red[area],rsky,clip,clipsig=3
     meanclip,blue[area],bsky,clip,clipsig=3
     if keyword_set(verbose) then print, '| ',bsky, rsky
   endif

   if keyword_set(bgvals) then begin
    bsky=bgvals[0]
    rsky=bgvals[1]
    if keyword_set(verbose) then print, '| ',bsky, rsky
   endif

   rsky = rsky>0
   bsky = bsky>0

   if keyword_set(verbose) then print, '| ',bsky, rsky

   red=(temporary(red)-rsky)>0
   blue=(temporary(blue)-bsky)>0

  endif

  ;----------------------------------
  ; define green color

  green=(greenmix[0]*blue + greenmix[1]*red)


  ;----------------------------------
  ; build RGB composite

  sz=size(red)


  image=fltarr(sz[1],sz[2],3)
  image[*,*,0]=red
  image[*,*,1]=green
  image[*,*,2]=blue



  if not keyword_set(brite) then begin
    brite=1
    if keyword_set(bgsub) then brite=(1+(sxpar(hdr,'EXPTIME')/1e4))<3
  endif 
  if not keyword_set(nonlinearity) then nonlinearity=2.5
  if not keyword_Set(origin) then origin=[0,0,0]

  image = nw_scale_rgb(image,scales=scales*brite)
  image = nw_arcsinh_fit(image,nonlinearity=nonlinearity)
  image = nw_fit_to_box(image,origin=origin)
  image = nw_float_to_byte(image)
  
  ;-------------------------------------
  ; Make and attach top text annotations

  if not keyword_set(no_annotations) then begin

  exptime = sxpar(hdr,'EXPTIME')
  ra_cen  = sxpar(hdr,'RA_CENT')
  dec_cen = sxpar(hdr,'DEC_CENT')
  obs_date= sxpar(hdr,'OBS-DATE')
  grelease = strcompress(string(sxpar(hdr,'GRELEASE')),/rem)

  red=image[*,*,0]
  green=image[*,*,1]
  blue=image[*,*,2]

  set_plot,'z' ; generate as virtual image because I write to image
               ; can be avoided if no text in border needed
  device,set_resolution=[sz[1],sz[2]]

  erase

  targ = target[i]
  if not be then targ = targ + " (NUV)"
  if not re then targ = targ + " (FUV)"

  xyouts,300,3710,'!6'+targ,charsize=7,charthick=4,/dev
  xyouts,300,3610,'!6Date: '+obs_date+'  '+$
   'NUVexp: '+strn(nexptime,format='(f9.2)')+'s  '+$
   'FUVexp: '+strn(fexptime,format='(f9.2)')+'s  '+$
   '!7a!6: '+strn(ra_cen,format='(f9.5)')+'  '+$
   '!7d!6: '+strn(dec_cen,format='(f10.6)'),$
    charsize=4,charthick=4,/dev
  

  bgsubtxt='N'
  if keyword_set(bgsub) then bgsubtxt='Y'

  
  stats='GREL:'+grelease+'/'+$
      strcompress('SCL:['+strn(scales[0],format='(f6.4)')+$
      ','+strn(scales[1],format='(f6.4)')+','+$
      strn(scales[2],format='(f6.4)')+$
      '] / BRT:'+strn(brite,format='(f7.3)')+$
      ' / NL:'+strn(nonlinearity,format='(f7.3)')+$
      ' / ORG:['+strn(origin[0])+','+strn(origin[1])+','+strn(origin[2])+$
      '] / BGSUB:'+bgsubtxt,/rem)

  xyouts,270,50,stats,charsize=3,charthick=3,/dev

  pos=[0.07,0.07,0.93,0.93]
  cropx1=fix(sz[1]*pos[0])
  cropx2=fix(sz[1]*pos[2])
  cropy1=fix(sz[2]*pos[1])
  cropy2=fix(sz[2]*pos[3])
  hextract,red,hdr,redcrop,hdrcrop,cropx1,cropx2,cropy1,cropy2
  !x.thick=2
  !y.thick=2
  type=2
  if keyword_set(bgsub) then type=1
  curvimcontour,redcrop,hdrcrop,type=type,/nodata,$
      /noerase,charsize=3,charthick=2.5,pos=[.07,.07,.93,.93],thick=2.5
  delvarx,redcrop,hdrcrop

  txtpln=tvrd()
  t=where(txtpln gt 0 and (smooth(red,10)+smooth(blue,10)) eq 0)

  device,/close ; close z device

  red[t]=200;255
  green[t]=200;255
  blue[t]=200;255

  image[*,*,0]=red
  image[*,*,1]=green
  image[*,*,2]=blue 


  ;-------------------------------
  ; add logo
 
  findpro,'rgb_galex_asinh',/noprint,dirlist=dirlist
  read_jpeg, dirlist[0]+'/logosimple.jpg', logo
  lsize=size(logo,/dim)
  image[2970:2970+599,3610:3610+161,0]=logo[0,*,*]
  image[2970:2970+599,3610:3610+161,1]=logo[1,*,*]
  image[2970:2970+599,3610:3610+161,2]=logo[2,*,*]

  ;--------------------------------
  ; make color bar for monitor test

  cb=intarr(21,256*10)
  for cc=0,20 do cb[cc,*]=reverse(indgen(256*10)/10)

  image[3700:3700+20, 3570-2560+1:3570, 0]=cb
  image[3721:3721+20, 3570-2560+1:3570, 1]=cb
  image[3742:3742+20, 3570-2560+1:3570, 2]=cb
  
  image[3690:3695, 3570-2560+1:3570, *]=100
  image[3767:3772, 3570-2560+1:3570, *]=100
  image[3690:3772, 3570:3575, *]=100
  image[3690:3772, 3570-2565+1:3570-2560+1, *]=100

  image[3660:3690, 3320:3330, 0]=200
  image[3772:3802, 3320:3330, 0]=200

  image[3660:3690, 3470:3480, 1]=200
  image[3772:3802, 3470:3480, 1]=200

  endif ; for no_annotations


  set_plot,'x'

  ;------------------------------------
  ; write files


  if keyword_set(full) then begin 
   jname=target[i]+'_2color.jpg'
   write_jpeg,jname,image,true=3,quality=quality
   if keyword_set(verbose) then print, '| Wrote '+jname
  endif

  if keyword_set(half) then begin
   img_med=rebin(image,1920,1920,3)
   jname=target[i]+'_2color_large.jpg'
   write_jpeg,jname,img_med,true=3,quality=quality
   if keyword_set(verbose) then print, '| Wrote '+jname
  endif

  if keyword_set(fourth) then begin
   img_med=rebin(image,960,960,3)
   jname=target[i]+'_2color_medium.jpg'
   write_jpeg,jname,img_med,true=3,quality=100
   if keyword_set(verbose) then print, '| Wrote '+jname
  endif

  if keyword_set(eighth) then begin
   img_sm=rebin(image,480,480,3)
   ;img_sm=rebin(image,900,400,3)
   ;img_sm=congrid(image,fix(sz[1]/8.),fix(sz[2]/8.),3)
   jname=target[i]+'_2color_small.jpg'
   write_jpeg,jname,img_sm,true=3,quality=100
   if keyword_set(verbose) then print, '| Wrote '+jname
  endif

  if keyword_set(thumb) then begin
   img_sm=rebin(image,120,120,3)
   jname=target[i]+'_2color_thumb.jpg'
   write_jpeg,jname,img_sm,true=3,quality=100
   if keyword_set(verbose) then print, '| Wrote '+jname
  endif

  if keyword_set(tif) then begin 
   timage=fltarr(3,sz[1],sz[2])
   timage[0,*,*]=image[*,*,0]
   timage[1,*,*]=image[*,*,1]
   timage[2,*,*]=image[*,*,2]
   jname=target[i]+'_2color.tif'
   timage=REVERSE(timage, 3)
   write_tiff,jname,timage,orientation=1
   if keyword_set(verbose) then print, '| Wrote '+jname
  endif  


  ;set_plot,'x'

endif else if keyword_set(verbose) then $
  print,'Cannot find either file:',fuvfile[i],nuvfile[i] 

endfor 

end
