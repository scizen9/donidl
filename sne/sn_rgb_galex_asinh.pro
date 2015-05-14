pro sn_rgb_galex_asinh, fspec=fspec, bgsub=bgsub,bgvals=bgvals, $
                     greenmix=greenmix, scales=scales,brite=brite, $
                     nonlinearity=nonlinearity, origin=origin, $
                     verbose=verbose, refhdr=refhdr, $
                     quality=quality,tif=tif,fuvshift=fuvshift, $
                     nofuvsmooth=nofuvsmooth, plotaps=plotaps, $
		     outdir=outdir,update=update
;+
; NAME:
;  sn_rgb_galex_asinh
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
;  bgsub = subtract a 'global' background level
;  greenmix = blue and red combination level for green where red and
;             blue are the lambda*flamda scaled
;             values. Default=[.2,.8]
;  brite = scale factor applied to all three RGB planes. Default=1
;          if bgsub then brite=(1+(sxpar(hdr,'EXPTIME')/1e4))<3,
;          setting brite will override this.
;  verbose = set keyword to complain if an error occurs
;          and give status of jpeg file creation
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
;  full scale jpeg -> target_2color.jpg
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
;  D. Neill, kludged for SN survey 13/07/07
;
;
;-
common sndb_info
common glgadb_info
;
; get file list
if keyword_set(fspec) then $
	fspc = fspec $
else	fspc = '*NUV.fit*'
nuvfile = file_search(fspc, count=nnuvf)
if nnuvf le 0 then begin
	print,'Cannot find ',fspc
	return
endif
;
;-----------------------------------------------------
; some quick checks

if keyword_set(verbose) then print, '| Starting asinh image creation' 

if keyword_set(outdir) then $
	odir = outdir + '/' $
else	odir = ''

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
;-----------------------------------------------------

;
; loop over files
for i=0,nnuvf-1 do begin

 fexptime=0
 nexptime=0

 fuvfile=nuvfile[i]
 p = strpos(fuvfile,'NUV')
 strput,fuvfile,'F',p

 re=file_exist(nuvfile[i])
 be=file_exist(fuvfile)

 if be or re then begin  ; if fits files exists

  if be     then  begin
    blue = mrdfits(fuvfile,0,bh,/silent)
    fexptime = sxpar(bh,'EXPTIME')
    if keyword_set(refhdr) then hastrom,blue,bh,refhdr 
    blue= blue*1.4*1525  ; scale as lambda*flambda
    if not keyword_set(nofuvsmooth) then $
         blue = smooth(blue,2)>0 ; lightly smooth the FUV image
    if keyword_set(fuvshift) then blue=shift(blue,fuvshift) 
  endif
  if re     then  begin
    red  = mrdfits(nuvfile[i],0,rh,/silent)
    nexptime = sxpar(rh,'EXPTIME')
    if keyword_set(refhdr) then hastrom,red,rh,refhdr
    red=  red*0.206*2297
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

  nx=sxpar(hdr,'NAXIS1')
  ny=sxpar(hdr,'NAXIS2')
  extast,hdr,astr
  getrot,hdr,rot,cdelt
  as_pix = abs(cdelt[0]*3600.)


  if keyword_set(bgsub) then begin

   if not keyword_set(bgvals) then begin
    if keyword_set(verbose) then print, '| Background subtracting' 
    sky,red,   rsky, rskysigma,circ=1400, /silent; quick method for sky & sigma
    sky,blue,  bsky, bskysigma,circ=1400, /silent
   
    if rsky le 0 or bsky le 0 then begin
     if keyword_set(verbose) then print, '| Computing alternative background' 
     dist_circle, circle, 3840, 1920, 1920
     area=where(circle le 1400)
     delvarx,circle
     meanclip,red[area],rsky,clip,clipsig=3
     meanclip,blue[area],bsky,clip,clipsig=3
     if keyword_set(verbose) then print, '| ',bsky, rsky
    endif
   endif else begin
    bsky=bgvals[0]
    rsky=bgvals[1]
   endelse
   rsky = rsky>0
   bsky = bsky>0

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
  mimage = nw_float_to_byte(image)	; final master image
  
  ;-------------------------------------
  ; Make and attach annotations
  
  ;
  ; get host name
  fdecomp,nuvfile[i],jnk1,ipath,root,ext
  host=strmid(root,0,strpos(root,'_NUV'))
  h=snhfind(host,nsn)
  ;
  ; loop over sne
  if nsn gt 0 then for j=0,nsn-1 do begin
    
	  ra = sndat[h[j]].ra
	  dec= sndat[h[j]].dec
	  snnm = strtrim(sndat[h[j]].id,2)
	  if strpos(snnm,'?') gt 0 then $
		  snnm = strmid(snnm,0,strpos(snnm,'?'))
	  galaxy = strtrim(sndat[h[j]].host,2)
	  galtyp = strtrim(sndat[h[j]].htype,2)
  ;
  ; get x,y of sn
  ad2xy,ra,dec,astr,snx,sny
  if keyword_set(verbose) then print,'SN x,y: ',snx,sny,format='(a,2f9.2)'

  red=mimage[*,*,0]
  green=mimage[*,*,1]
  blue=mimage[*,*,2]

;
; plot galaxy name, type, SN name, type, survey
;
  set_plot,'z' ; generate as virtual image because I write to image
               ; can be avoided if no text in border needed
  device,set_resolution=[sz[1],sz[2]]
  !p.font=1
  erase

;
; get redshift
  p = snfind(snnm)
  if sndat(p).cz gt -1000. and sndat(p).cz lt 1.e5 then $
	  czlab = string(sndat(p).cz,format='(f7.1)')+' km/s' $
  else    if sndat(p).cz le -1000. then czlab = '-' $
  else	  czlab = string((sndat(p).cz/!phys_c),format='(f7.3)')
  lab1 = string(snnm,sndat(p).type,czlab,format='(a-8,1x,a-6,1x,a12)')
  lab2 = string(galaxy,galtyp,fexptime,'/',nexptime,' s', $
	  format='(a-12,2x,a-5,2x,f6.0,a,f6.0,a)')
  xyouts,15,ny-30,lab1,charsize=2,charthick=4,/dev
  xyouts,15,15,lab2,charsize=2,charthick=4,/dev
  txtpln=tvrd()
  galt=where(txtpln gt 0)

  device,/close ; close z device
;
; insert into image
;
  red[galt]=200;255	; make Galaxy name white
  green[galt]=200;255
  blue[galt]=200;255

;
; plot apertures
;
  if keyword_set(plotaps) then begin
	  sfx = '_aps'
;
; get aperture sizes
	ap0 = sndat(p).ap_res	; radius of one NUV resolution element
	ap1 = sndat(p).ap_500pc	; 500pc diameter aperture
	ap2 = sndat(p).ap_1kpc	; 1 kpc diameter aperture
	ap3 = sndat(p).ap_2kpc	; 2 kpc diameter aperture
	if keyword_set(verbose) then print,'Aps: ',ap0,ap1,ap2,ap3
;
; set up aperture plots
	npoints  = 90.
	interval = (2. * !pi) / npoints
	t = findgen(npoints + 1) * interval
;
; plot ap1
;
    if finite(ap1) eq 1 and ap1 gt ap0 then begin
  	set_plot,'z' ; generate as virtual image because I write to image
               	; can be avoided if no text in border needed
  	device,set_resolution=[sz[1],sz[2]]
  	erase

	rap = ap1/2./as_pix	; radius in pixels
	x = snx + rap * cos(t)
	y = sny + rap * sin(t)
	plots,x(0),y(0),/dev
	for k=1,npoints do plots,x(k),y(k),/dev,/continue

  	txtpln=tvrd()
  	ap1t=where(txtpln ne 0,nind)

  	device,/close ; close z device

	if nind gt 0 then begin
		red[ap1t]=0
		green[ap1t]=240
		blue[ap1t]=0
	endif else print,'Ap 1 produced no indices'
    endif
;
; plot ap2
;
    if finite(ap2) eq 1 and ap2 gt ap0 then begin
  	set_plot,'z' ; generate as virtual image because I write to image
               	; can be avoided if no text in border needed
  	device,set_resolution=[sz[1],sz[2]]
  	erase

	rap = ap2/2./as_pix	; radius in pixels
	x = snx + rap * cos(t)
	y = sny + rap * sin(t)
	plots,x(0),y(0),/dev
	for k=1,npoints do plots,x(k),y(k),/dev,/continue

  	txtpln=tvrd()
  	ap2t=where(txtpln ne 0,nind)

  	device,/close ; close z device

	if nind gt 0 then begin
		red[ap2t]=0
		green[ap2t]=240
		blue[ap2t]=0
	endif else print,'Ap 2 produced no indices'
    endif
;
; plot ap3
;
    if finite(ap3) eq 1 and ap3 gt ap0 then begin
  	set_plot,'z' ; generate as virtual image because I write to image
               	; can be avoided if no text in border needed
  	device,set_resolution=[sz[1],sz[2]]
  	erase

	rap = ap3/2./as_pix	; radius in pixels
	x = snx + rap * cos(t)
	y = sny + rap * sin(t)
	plots,x(0),y(0),/dev
	for k=1,npoints do plots,x(k),y(k),/dev,/continue

  	txtpln=tvrd()
  	ap3t=where(txtpln ne 0,nind)

  	device,/close ; close z device

	if nind gt 0 then begin
		red[ap3t]=0
		green[ap3t]=240
		blue[ap3t]=0
	endif else print,'Ap 3 produced no indices'
    endif
;
; plot ap0
  	set_plot,'z' ; generate as virtual image because I write to image
               	; can be avoided if no text in border needed
  	device,set_resolution=[sz[1],sz[2]]
  	erase

	rap = ap0/2./as_pix	; radius in pixels
	x = snx + rap * cos(t)
	y = sny + rap * sin(t)
	plots,x(0),y(0),/dev
	for k=1,npoints do plots,x(k),y(k),/dev,/continue

  	txtpln=tvrd()
  	ap0t=where(txtpln ne 0,nind)

  	device,/close ; close z device

	if nind gt 0 then begin
		red[ap0t]=240
		green[ap0t]=0
		blue[ap0t]=0
	endif else print,'Ap 0 produced no indices'
  endif else begin
	  sfx = ''
;
; plot supernova position
;
    if snx gt 1. and sny gt 1. and snx lt sz[1] and sny lt sz[2] then begin
	set_plot,'z' ; generate as virtual image because I write to image
               ; can be avoided if no text in border needed
	device,set_resolution=[sz[1],sz[2]]
	erase

	plots,snx,sny,psym=1,symsize=2.0,thick=1,/dev
	txtpln=tvrd()
	snt=where(txtpln gt 0)

	device,/close ; close z device

	red[snt]=240		; SN plotted as red
	green[snt]=0
	blue[snt]=0
    endif else print,snnm+' off image'
  endelse
;
; reconstruct image
;
  image[*,*,0]=red
  image[*,*,1]=green
  image[*,*,2]=blue 


  ;------------------------------------
  ; write jpg file


  jname=odir+host+'_'+snnm+sfx+'_galex.jpg'
  if not file_test(jname) or keyword_set(update) then begin
  	write_jpeg,jname,image,true=3,quality=quality
  	if keyword_set(verbose) then print, '| Wrote '+jname
  endif

  endfor	; loop over sne

endif else if keyword_set(verbose) then $
  print,'Cannot find either file:',fuvfile,nuvfile[i] 

endfor	; loop over files

end
