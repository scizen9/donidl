pro sn_rgb_xsc_asinh, file, snname, scales=scales,brite=brite, $
                     nonlinearity=nonlinearity, origin=origin, $
                     verbose=verbose, quality=quality,tif=tif, $
		     plotaps=plotaps
;+
; NAME:
;  sn_rgb_xsc_asinh
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
;  sn_rgb_xsc_asinh, scales=scales,brite=brite, $
;                     nonlinearity=nonlinearity, origin=origin, $
;                     verbose=verbose, quality=quality,tif=tif, $
;		     plotaps=plotaps
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
;  D. Neill, further kludge for 2mass XSC data 16/08/2007
;
;
;-
;
common lowz_sne_info
;
;-----------------------------------------------------
; some quick checks

if keyword_set(verbose) then print, '| Starting asinh image creation' 

if not keyword_set(scales) then scales=[.085,.055,.085]
if n_elements(scales) ne 3 then begin $
 if keyword_set(verbose) then $
  print,'*** ERROR: scales must be a 3 element array: [red,green,blue].'
  print,'*** ERROR: default value is [.085,.09,.085].'
 return
endif

if not keyword_set(quality) then quality=75

if not keyword_set(brite) then brite=1.

if not keyword_set(nonlinearity) then nonlinearity=2.5

if not keyword_set(origin) then origin=[0,0,0]

;-----------------------------------------------------

if file_test(file) then begin

  fdecomp,file,disk,dir,name,ext
  host=strupcase(name)

  img = mrdfits(file,0,hdr,/silent)

  blue = img(*,*,0)
  green = img(*,*,1)
  red = img(*,*,2)

  nx=sxpar(hdr,'NAXIS1')
  ny=sxpar(hdr,'NAXIS2')


  ;----------------------------------
  ; build RGB composite

  sz=size(red)

  image=fltarr(sz[1],sz[2],3)
  image[*,*,0]=red
  image[*,*,1]=green
  image[*,*,2]=blue

  image = nw_scale_rgb(image,scales=scales*brite)
  image = nw_arcsinh_fit(image,nonlinearity=nonlinearity)
  image = nw_fit_to_box(image,origin=origin)
  image = nw_float_to_byte(image)
  
  ;-------------------------------------
  ; Make and attach annotations
  snnm = strtrim(snname,2)
  t=snfind(snnm)
  rad=sndat(t).ra
  decd=sndat(t).dec
  coostr=adstrn(rad,decd,1,delim=':')
  rastr=gettok(coostr,' ')
  decstr=strtrim(coostr,2)
  sntyp=sndat(t).type
  htype=sndat(t).htype
  cz=sndat(t).cz
  
  ;
  ; get x,y of sn
  cmd='sky2xy '+file+' '+rastr+' '+decstr
  spawn,cmd,res
  if strpos(res,'ff') lt 0 then begin
	  sta=strsplit(res,/extract)
	  snx = float(sta(4))
	  sny = float(sta(5))
  endif else begin
	  snx = -1.
	  sny = -1.
  endelse
  print,'SN x,y: ',snx,sny,format='(a,2f9.2)'

  red=image[*,*,0]
  green=image[*,*,1]
  blue=image[*,*,2]

;
; plot galaxy name, type, SN name, type, survey
;
  set_plot,'z' ; generate as virtual image because I write to image
               ; can be avoided if no text in border needed
  device,set_resolution=[sz[1],sz[2]]
  !p.font=1
  erase

;
; make labels
  lab1 = string(snnm,sntyp,cz,format='(a-8,1x,a-6,1x,f7.1)')
  lab2 = string(host,htype,format='(a-10,1x,a-5)')
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
;
; get galaxy distance
	sscl=sndat(t).linear_scale
;
; get aperture sizes (scale for 2mass XSC is 1 arcsec per pixel)
	ap0 = (5.6)*0.5		; radius of one NUV resolution element
	ap1 = (250./sscl)	; 500pc diameter aperture
	ap2 = (500./sscl)	; 1 kpc diameter aperture
	ap3 = (1.d3/sscl)	; 2 kpc diameter aperture
	print,'Aps: ',ap0,ap1,ap2,ap3
;
; plot ap1
;
  	set_plot,'z' ; generate as virtual image because I write to image
               	; can be avoided if no text in border needed
  	device,set_resolution=[sz[1],sz[2]]
  	erase

	npoints  = 90.
	interval = (2. * !pi) / npoints
	t = findgen(npoints + 1) * interval
	x = snx + ap1 * cos(t)
	y = sny + ap1 * sin(t)
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
;
; plot ap2
;
  	set_plot,'z' ; generate as virtual image because I write to image
               	; can be avoided if no text in border needed
  	device,set_resolution=[sz[1],sz[2]]
  	erase

	x = snx + ap2 * cos(t)
	y = sny + ap2 * sin(t)
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
;
; plot ap3
;
  	set_plot,'z' ; generate as virtual image because I write to image
               	; can be avoided if no text in border needed
  	device,set_resolution=[sz[1],sz[2]]
  	erase

	x = snx + ap3 * cos(t)
	y = sny + ap3 * sin(t)
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
;
; plot ap0
  	set_plot,'z' ; generate as virtual image because I write to image
               	; can be avoided if no text in border needed
  	device,set_resolution=[sz[1],sz[2]]
  	erase

	x = snx + ap0 * cos(t)
	y = sny + ap0 * sin(t)
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
;
; plot supernova position
;
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
  endelse
;
; reconstruct image
;
  image[*,*,0]=red
  image[*,*,1]=green
  image[*,*,2]=blue 


  ;------------------------------------
  ; write files


  jname=dir+name+'_'+snnm+'.jpg'
  write_jpeg,jname,image,true=3,quality=quality
  if keyword_set(verbose) then print, '| Wrote '+jname

  set_plot,'x'

endif else if keyword_set(verbose) then $
  print,'Cannot find file:',file

end
