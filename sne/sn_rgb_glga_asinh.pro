pro sn_rgb_glga_asinh, snid, galex=galex,sdss=sdss,twomass=twomass,wise=wise, $
	scales=scales,brite=brite, nonlinearity=nonlinearity, origin=origin, $
	quality=quality, outdir=outdir, update=update, $
	silent=silent, verbose=verbose
;+
; NAME:
;  sn_rgb_glga_asinh
;
;
; PURPOSE:
;  Generate RGB composite jpeg images from GLGA intensity maps.
;
;  Images are lambda*flamda and asinh scaled/fit using Wherry,
;  Blanton, Hogg IDL routines of the the Lupton, et al. algorithm.
;
; CATEGORY:
;  image processing
;
;
; CALLING SEQUENCE:
;  sn_rgb_glga_asinh, snid, scales=scales,brite=brite, $
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
;  jpeg sn finder chart
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
;  D. Neill, further kludge for SDSS data 08/06/2009
;
;-
; database access
common sndb_info
common galdb_info
common glgadb_info
;
; check inputs
verb = (1 eq 1)
if keyword_set(silent) then verb = ( 1 eq 0)
if keyword_set(verbose) then verb = (1 eq 1)
;
; find sn
s = snfind(snid,/silent)
if s[0] lt 0 then begin
	if verb then print,'Not found: ',snid
	return
endif
;
; get its name
snnm = strtrim(sndat[s].id,2)
if strpos(snnm,'?') gt 0 then $
	snnm = strmid(snnm,0,strpos(snnm,'?'))
;
; get host id
g = gfind(sndat[s].host)
if g[0] lt 0 then begin
	if verb then print,'Host not found: ',sndat[s].host
	return
endif
;
; is host in GLGA?
l = glfind(galdat[g].id)
if l[0] lt 0 then begin
	if verb then print,'Host not in GLGA: ',galdat[g].id
	return
endif
;
; get host id, type
host = strtrim(glgadat[l].id,2)
galaxy = host
galtyp = strtrim(galdat[g].type,2)
;
; which survey?
bands = ['FUV', 'NUV']
srvy='galex'		; data type
dscls = [0.085, 0.09, 0.085]
greenmix = [0.2,0.8]
offset = 0.
if keyword_set(sdss) then begin
	bands=['g','r','i']
	srvy='sdss'
	dscls = [300., 390., 600.]
	offset = 0.0002
endif
if keyword_set(twomass) then begin
	bands = ['j','h','k']
	srvy='2mass'
	dscls = [12.5, 9.5, 10.5]
	offset = 0.
endif
if keyword_set(wise) then begin
	bands = ['w1','w2','w3']
	srvy='wise'
	dscls = [0.07,0.18,0.07]
	offset = 0.
endif
nbands = n_elements(bands)
;
; get directories
dd = !GLGA_ROOT + 'data/' + glga_degdir(galdat[g].ra)
plotpath = dd + '/plots/'
ddir = dd + '/'+srvy+'/fits/'
;-----------------------------------------------------
; some quick checks

if verb then begin
	print,' '
	print, '| Starting asinh image creation for SN: ',snid
endif

if keyword_set(scales) then $
	scls=scales $
else	scls=dscls
if n_elements(scls) ne 3 then begin $
 if verb then $
  print,'*** ERROR: scales must be a 3 element array: [red,green,blue].'
 return
endif

if not keyword_set(quality) then quality=75

if not keyword_set(brite) then brite=1.

if not keyword_set(nonlinearity) then nonlinearity=2.5

if not keyword_set(origin) then origin=[0,0,0]

if keyword_set(outdir) then $
	odir = outdir + '/' $
else	odir = plotpath

;-----------------------------------------------------

bfile = ddir + host + '_' + bands[0] + '.fit*'
rfile = ddir + host + '_' + bands[nbands-1] + '.fit*'
gfile = 'xxxxx'
if srvy ne 'galex' then $
	gfile = ddir + host + '_' + bands[1] + '.fit*'

re=file_exist(rfile)
gg=file_exist(gfile)
be=file_exist(bfile)

if be or re or gg then begin  ; if fits files exists

  if be then  begin
    blue = mrdfits(bfile,0,bh,/fscale,/silent)
    mmm,blue[where(blue gt 0)],sky,sig
    if sig le 0. then $
	    sky,blue[where(blue gt 0)],sky,sig
    blue = (blue - sky) + offset
    hdr = bh
  endif
  if gg then  begin
    green  = mrdfits(gfile,0,gh,/fscale,/silent)
    mmm,green[where(green gt 0)],sky,sig
    if sig le 0. then $
	    sky,green[where(green gt 0)],sky,sig
    green = (green - sky) + offset
    hdr = gh
  endif
  if re then  begin
    red  = mrdfits(rfile,0,rh,/fscale,/silent)
    mmm,red[where(red gt 0)],sky,sig
    if sig le 0. then $
	    sky,red[where(red gt 0)],sky,sig
    red = (red - sky) + offset
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
  ;
  ; handle galex
  if srvy eq 'galex' then begin
	  blue= blue*1.4*1525.
	  blue = smooth(blue,2)>0.	; lightly smooth FUV image
	  red = red*0.206*2297.
	  green = (greenmix[0]*blue + greenmix[1]*red)
  endif

  nx=sxpar(hdr,'NAXIS1')
  ny=sxpar(hdr,'NAXIS2')
  extast,hdr,astr
  getrot,hdr,rot,cdelt
  as_pix = abs(cdelt[0]*3600.)
  print,'| pixel scale (as/px) : ',as_pix
  ;
  ; linear scale
  lscl = galdat[g].linear_scale	; pc / arcsecond
  linpix = 2000./lscl/as_pix>0	; 2kpc line on image
  print,'| lin. scale has 2kpc : ',linpix,' raw pixels'
  ;
  ; max size
  mxpx = fix(600./as_pix+0.5)-1	; 600 arcsec is image size
  print,'| input image size    : ',nx,' x ',ny,' px'


  ;-------------------------------------
  ; Get SN position

  ra = sndat[s].ra
  dec= sndat[s].dec
  ;
  ; get x,y of sn
  ad2xy,ra,dec,astr,snx,sny
  if verb then print,'| SN x,y in input img : ',snx,sny,format='(a,2f13.2)'
  ;
  ; check if we are on the image
  if snx lt 0 or snx ge nx or sny lt 0 or sny ge ny then begin
	  print,'| SN off image nx,ny  : ',nx,ny
	  return
  endif
  ;
  ; get sub image limits
  x0 = (fix(snx+0.5) - (mxpx/2)) > 0
  x1 = (fix(snx+0.5) + (mxpx/2)) < (nx-1)
  y0 = (fix(sny+0.5) - (mxpx/2)) > 0
  y1 = (fix(sny+0.5) + (mxpx/2)) < (ny-1)

  ;
  ; extract subims
  red	= red[x0:x1,y0:y1]
  green	= green[x0:x1,y0:y1]
  blue	= blue[x0:x1,y0:y1]
  sz=size(red)

  ;
  ; get sn pixel in subimg
  snxpx = fix(snx+0.5) - x0
  snypx = fix(sny+0.5) - y0

  ;
  ; make full arrays
  fred  = dblarr(mxpx,mxpx)
  fgreen= fred
  fblue = fred

  ;
  ; get offsets
  xoff = mxpx/2 - snxpx
  yoff = mxpx/2 - snypx
  print,'| x,y offsets         : ',xoff,yoff

  ;
  ; set image limits
  x0o = xoff > 0
  x1o = x0o + (sz[1]-1) < (mxpx-1)
  y0o = yoff > 0
  y1o = y0o + (sz[2]-1) < (mxpx-1)

  ;
  ; insert into full arrays
  fred[x0o:x1o,y0o:y1o] = red
  fgreen[x0o:x1o,y0o:y1o] = green
  fblue[x0o:x1o,y0o:y1o] = blue

  ;
  ; resample if needed
  resam = as_pix / 0.5
  if resam gt 1.01 or resam lt 0.99 then begin
	  outsz = 1200
	  red	= congrid(fred,outsz,outsz)
	  green	= congrid(fgreen,outsz,outsz)
	  blue	= congrid(fblue,outsz,outsz)
	  snx = snx * resam
	  sny = snx * resam
  	  print,'| resampling to size  : 1200 x 1200 px'
  endif else begin
	  red   = fred
	  green = fgreen
	  blue  = fblue
  	  print,'| output image size   : 1200 x 1200 px'
  endelse
  snx = 600.
  sny = 600.

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
  image = nw_float_to_byte(image)
  
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
; get redshift
  p = snfind(snnm)
  if sndat[p].cz gt -1000. and sndat[p].cz lt 1.e5 then $
	  czlab = string(sndat[p].cz,format='(f7.1)')+' km/s' $
  else    if sndat[p].cz le -1000. then czlab = '-' $
  else	  czlab = string((sndat[p].cz/!phys_c),format='(f7.3)')
;
; make name, cz/redshit label
  lab1 = string(snnm,sndat[p].type,czlab,format='(a-8,1x,a-6,1x,a12)')
;
; make galaxy and type label
  lab2 = string(galaxy,galtyp,format='(a-12,2x,a-5)')
;
; plot them on the device
  xyouts,15,sz[2]-30,lab1,charsize=3,charthick=2,/dev
  xyouts,15,15,lab2,charsize=3,charthick=2,/dev
  if lscl gt 0. then begin
  	xyouts,sz[1]-100,sz[2]-30,'2Kpc',charsize=3,charthick=2,/dev
  	plots,(sz[1]-120.)-linpix*resam,sz[2]-18,/dev
  	plots,(sz[1]-120.),sz[2]-18,/dev,/continue,thick=4
  endif
;
; plot a large circle at SN position
  npoints = 90.
  interval = (2. * !pi) / npoints
  t = findgen(npoints + 1) * interval
  rap = (50.*resam)/2./as_pix
  x = snx + rap * cos(t)
  y = sny + rap * sin(t)
  plots,x[0],y[0],/dev
  for k=1,npoints do plots,x[k],y[k],/dev,/continue
  txtpln=tvrd()
  galt=where(txtpln gt 0)

  device,/close ; close z device
;
; insert into image
;
  red[galt]=255		; make annotations white
  green[galt]=255
  blue[galt]=255

;
; plot black circle in case we are on a bright galaxy
;
  set_plot,'z' ; generate as virtual image because I write to image
	       ; can be avoided if no text in border needed
  device,set_resolution=[sz[1],sz[2]]
  erase

  rap = (48.*resam)/2./as_pix
  x = snx + rap * cos(t)
  y = sny + rap * sin(t)
  plots,x[0],y[0],/dev
  for k=1,npoints do plots,x[k],y[k],/dev,/continue
  txtpln=tvrd()
  snt=where(txtpln gt 0)

  device,/close ; close z device

  red[snt]=0		; SN circle plotted as black
  green[snt]=0
  blue[snt]=0
;
; plot smaller red diamond at supernova position
;
  set_plot,'z' ; generate as virtual image because I write to image
	       ; can be avoided if no text in border needed
  device,set_resolution=[sz[1],sz[2]]
  erase

  plots,snx,sny,psym=4,symsize=2.0,thick=1,/dev
  txtpln=tvrd()
  snt=where(txtpln gt 0)

  device,/close ; close z device

  red[snt]=255		; SN plotted as red
  green[snt]=0
  blue[snt]=0
;
; reconstruct image
;
  image[*,*,0]=red
  image[*,*,1]=green
  image[*,*,2]=blue 


  ;------------------------------------
  ; write jpg file

  jname=odir+host+'_'+snnm+'_'+srvy+'.jpg'
  if not file_test(jname) or keyword_set(update) then begin
  	write_jpeg,jname,image,true=3,quality=quality
  	if verb then print, '| Wrote : '+jname
  endif else $
	if verb then print,'| Already exists : '+jname

endif else if verb then begin
  print,'| Cannot find files :'
  print,'| ',bfile
  if nbands ge 3 then print,'| ',gfile
  print,'| ',rfile
endif

end
