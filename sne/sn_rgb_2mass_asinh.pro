pro sn_rgb_2mass_asinh, fspec=fspec,scales=scales,brite=brite, $
                     nonlinearity=nonlinearity, origin=origin, $
                     verbose=verbose, quality=quality, outdir=outdir, $
		     plotaps=plotaps, update=update
;+
; NAME:
;  sn_rgb_2mass_asinh
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
;  sn_rgb_2mass_asinh, fspec=fspec,scales=scales,brite=brite, $
;                     nonlinearity=nonlinearity, origin=origin, $
;                     verbose=verbose, quality=quality
;
; INPUTS:
;          
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;  brite = scale factor applied to all three RGB planes. Default=1
;  verbose = set keyword to complain if an error occurs
;          and give status of jpeg file creation
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
; MODIFICATION HISTORY:
;  M. Seibert 12/24/2004
;  M. Seibert 12/06/2005
;  D. Neill, kludged for SN survey 13/07/2007
;  D. Neill, further kludge for 2mass LGA data 16/08/2007
;
;
;-
;
common lowz_sne_info
restore,!SNE_DATA+'glga_info.sav'
;
; get file list
if not keyword_set(fspec) then fspec='*_k.fits.gz'
pfile = file_search(fspec)
;-----------------------------------------------------
; some quick checks

if keyword_set(verbose) then print, '| Starting asinh image creation' 

if keyword_set(scales) then $
	scls=scales $
else	scls=[0.085, 0.055, 0.085]
if n_elements(scls) ne 3 then begin $
 if keyword_set(verbose) then $
  print,'*** ERROR: scales must be a 3 element array: [red,green,blue].'
  print,'*** ERROR: default value is [1.,1.,1.]'
 return
endif

if not keyword_set(quality) then quality=75

if not keyword_set(brite) then brite=1.

if not keyword_set(nonlinearity) then nonlinearity=2.5

if not keyword_set(origin) then origin=[0,0,0]

if keyword_set(outdir) then $
	odir = outdir + '/' $
else	odir = ''

;-----------------------------------------------------

for i=0,n_elements(pfile)-1 do begin 

 jfile=pfile[i]
 strput,jfile,'j',strpos(pfile[i],'k')
 hfile=pfile[i]
 strput,hfile,'h',strpos(pfile[i],'k')
 kfile=pfile[i]

 re=file_exist(kfile)
 gg=file_exist(hfile)
 be=file_exist(jfile)

 if be or re or gg then begin  ; if fits files exists

  if be     then  begin
    blue = mrdfits(jfile,0,bh,/fscale,/silent)
    bsky = sxpar(bh,'skyval')
    blue = blue - bsky
    hdr = bh
  endif
  if gg     then  begin
    green  = mrdfits(hfile,0,gh,/fscale,/silent)
    gsky = sxpar(gh,'skyval')
    green = green - gsky
    hdr = gh
  endif
  if re     then  begin
    red  = mrdfits(kfile,0,rh,/fscale,/silent)
    rsky = sxpar(rh,'skyval')
    red = red - rsky
    hdr = rh
  endif
  if not be and re then begin 
    blue = red
  endif else if not be and gg then begin
    blue = green
  endif
  if not re and gg then begin 
    red = green
  endif else if not re and be then begin
    red = blue
  endif
  if not gg and re then begin 
    green = red
  endif else if not gg and be then begin
    green = blue
  endif

  nx=sxpar(hdr,'NAXIS1')
  ny=sxpar(hdr,'NAXIS2')
  extast,hdr,astr
  getrot,hdr,rot,cdelt
  as_pix = abs(cdelt[0]*3600.)


  ;----------------------------------
  ; build RGB composite

  sz=size(red)

  image=fltarr(sz[1],sz[2],3)
  image[*,*,0]=red
  image[*,*,1]=green
  image[*,*,2]=blue

  image = nw_scale_rgb(image,scales=scls*brite)
  image = nw_arcsinh_fit(image,nonlinearity=nonlinearity)
  image = nw_fit_to_box(image,origin=origin)
  mimage = nw_float_to_byte(image)
  
  ;-------------------------------------
  ; Make and attach annotations
  
  ;
  ; get host name
  fdecomp,pfile[i],jnk1,ipath,root,ext
  host=strmid(root,0,strpos(root,'_k'))
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
  lab2 = string(galaxy,galtyp,format='(a-12,2x,a-5)')
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


  jname=odir+host+'_'+snnm+sfx+'_2mass.jpg'
  if not file_test(jname) or keyword_set(update) then begin
  	write_jpeg,jname,image,true=3,quality=quality
  	if keyword_set(verbose) then print, '| Wrote '+jname
  endif

  endfor	; loop over sne

endif else if keyword_set(verbose) then $
  print,'Cannot find any file:',hfile,jfile,kfile

endfor	; loop over files

end
