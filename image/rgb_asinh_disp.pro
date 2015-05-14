pro rgb_asinh_disp, redi, greeni, bluei, greenmix=greenmix, scales=scales, $
		brite=brite, nonlinearity=nonlinearity, origin=origin, $
		verbose=verbose, nobsmooth=nobsmooth, $
		aprad=aprad, appos=appos, skyrad=skyrad
;+
; NAME:
;  rgb_asinh_disp
;
; CATEGORY:
;  image processing
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;  greenmix = blue and red combination level for green where red and
;             blue are the lambda*flamda scaled
;             values. Default=[.2,.8]
;  brite = scale factor applied to all three RGB planes. Default=1
;          if bgsub then brite=(1+(sxpar(hdr,'EXPTIME')/1e4))<3,
;          setting brite will override this.
;  verbose = set keyword to complain if an error occurs
;          and give status of jpeg file creation
;
;  The following keywords are associated with the Wherry et al routines.
;  ----------------------------------------------------------------------
;  scales = scaling applied to red green and blue. Default=[.085,.09,.085]
;  nonlinearity = nonlinearity factor used in asinh scaling. Default=2.5
;  origin = Limits the pixel values of the image to a 'box', so that
;           the colors do not saturate to white but to a specific color. 
;           Default=[0,0,0]
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  M. Seibert 12/24/2004
;  M. Seibert 12/06/2005
;  D. Neill, kludged for SN survey 13/07/07
;
;
;-
;-----------------------------------------------------
; some quick checks

if keyword_set(verbose) then print, '| Starting asinh image creation' 

if n_params() lt 1 then begin
 print,'Usage: rgb_asinh_disp, red, green [, blue]'
 return
endif

if not keyword_set(greenmix) then greenmix=[.2,.8]
if n_elements(greenmix) ne 2 then begin $
 if keyword_set(verbose) then $
  print,'*** ERROR: greenmix must be a 2 element array: [fraction_r,fraction_b].'
 return
endif

if not keyword_set(scales) then scales=[1.,1.,1.]
if n_elements(scales) ne 3 then begin $
 if keyword_set(verbose) then $
  print,'*** ERROR: scales must be a 3 element array: [red,green,blue].'
  print,'*** ERROR: default value is [1.,1.,1.].'
 return
endif

;-----------------------------------------------------
  ;----------------------------------
  ; define green color

  if n_params(0) lt 3 then begin
	if n_params(0) lt 2 or n_elements(greeni) le 0 then begin
		red=redi
		blue=redi
		green=redi
		scales=[scales(0),scales(0),scales(0)]
	endif else begin
		red=redi
		blue=greeni
		green=(greenmix[0]*blue + greenmix[1]*red)
	endelse
  endif else begin
	  red=redi
	  green=greeni
	  blue=bluei
  endelse

  if not keyword_set(nobsmooth) then $
	blue = smooth(blue,2)>0 ; lightly smooth the FUV image

  ;----------------------------------
  ; build RGB composite

  sz=size(red)
  nx=sz[1]
  ny=sz[2]

  image=fltarr(sz[1],sz[2],3)
  image[*,*,0]=red
  image[*,*,1]=green
  image[*,*,2]=blue

  if not keyword_set(brite) then begin
    brite=1
  endif 
  if not keyword_set(nonlinearity) then nonlinearity=2.5
  if not keyword_Set(origin) then origin=[0,0,0]

  image = nw_scale_rgb(image,scales=scales*brite)
  image = nw_arcsinh_fit(image,nonlinearity=nonlinearity)
  image = nw_fit_to_box(image,origin=origin)
  image = nw_float_to_byte(image)
  
  ;-------------------------------------
;
; plot 
;
  set_plot,'x' ; generate as virtual image because I write to image
               ; can be avoided if no text in border needed
  !p.font=1
;
; display
;
  tvscl,image,true=3

;
; plot apertures
;
  if keyword_set(aprad) then begin
	if keyword_set(appos) then begin
		x0 = appos(0)
		y0 = appos(1)
	endif else begin
		x0 = float(nx)/2.
		y0 = float(ny)/2.
	endelse
	npoints = 90.
	interval = (2.*!pi) / npoints
	t = findgen(npoints + 1) * interval
	x = x0 + aprad * cos(t)
	y = y0 + aprad * sin(t)
	plots,x(0),y(0),/dev
	for k=1,npoints do plots,x(k),y(k),/dev,/continue
  endif
  if keyword_set(skyrad) then begin
	if keyword_set(appos) then begin
		x0 = appos(0)
		y0 = appos(1)
	endif else begin
		x0 = float(nx)/2.
		y0 = float(ny)/2.
	endelse
	npoints = 90.
	interval = (2.*!pi) / npoints
	t = findgen(npoints + 1) * interval
	x = x0 + skyrad(0) * cos(t)
	y = y0 + skyrad(0) * sin(t)
	plots,x(0),y(0),/dev
	for k=1,npoints do plots,x(k),y(k),/dev,/continue
	x = x0 + skyrad(1) * cos(t)
	y = y0 + skyrad(1) * sin(t)
	plots,x(0),y(0),/dev
	for k=1,npoints do plots,x(k),y(k),/dev,/continue
  endif

return
end
