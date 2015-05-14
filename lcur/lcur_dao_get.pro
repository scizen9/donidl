pro lcur_dao_get
;
COMMON dao_imdata	; dao_im, dao_imhdr, dao_imgno

inp = ''
read,'dao file root (no ext): ',inp
if strtrim(inp,2) eq '' then return
root = gettok(inp,'.')
mfile = root+'.mch'
dfile = root+'.dat'
;
ch=strmid(root,strpos(root,'c')+1,1)
if stregex(ch,'[0-9]') lt 0 then begin
	ch=strmid(root,strpos(root,'f')+1,1)
	ich=-1
endif else ich=fix(ch)
;
;check input
list=findfile(mfile,count=nf)
if nf ne 1 then begin
	print,'Error finding .mch file: ',mfile,', no data read'
	return
endif
list=findfile(dfile,count=nf)
if nf ne 1 then begin
	print,'Error finding .dat file: ',dfile,', no data read'
	return
endif
;
;
imgno = 0L
;read,'display image number (-1 to skip): ',imgno
imgno = -1L

rddaomch, mfile, ifilt, imfiles

openr,dlun,dfile,/get_lun

ndates = 0L
nstars = 0L
readu,dlun,ndates,nstars

jd = dblarr(ndates)
readu,dlun,jd

id	= lonarr(nstars)
x	= fltarr(nstars)
y	= fltarr(nstars)
avmags	= fltarr(nstars)
avmerrs = fltarr(nstars)
nframes = intarr(nstars)
chi	= fltarr(nstars)
sharp	= fltarr(nstars)
var	= fltarr(nstars)
blunder	= fltarr(nstars)
mags	= fltarr(ndates,nstars)
merrs	= fltarr(ndates,nstars)
readu,dlun,id,x,y,avmags,avmerrs,nframes,chi,sharp,var,blunder,mags,merrs

free_lun,dlun

;
; get mask file
flist=file_search('refmask.fits')
mskfil=flist(0)
mskim=mrdfits(mskfil,0,mhdr)
nmskx=sxpar(mhdr,'naxis1')
nmsky=sxpar(mhdr,'naxis2')
;
; mask saturated stars
mask=intarr(nstars) + 1
for i=0,nstars-1 do begin
	ix = fix(x(i)+0.5)
	iy = fix(y(i)+0.5)
	if ix ge 0 and ix lt nmskx and iy ge 0 and iy lt nmsky then $
		mask(i) = mskim(ix,iy)
endfor
good = where(mask le 0, ngood)
id		= id(good)
x		= x(good)
y		= y(good)
avmags		= avmags(good)
avmerrs		= avmerrs(good)
nframes		= nframes(good)
chi		= chi(good)
sharp		= sharp(good)
var		= var(good)
blunder		= blunder(good)
mags		= mags(*,good)
merrs		= merrs(*,good)
print,'Good/Total stars: ',ngood,' / ',nstars
nstars=ngood
;
; set any other limits?

choice = 0
read,'Enter: 0 - no limits, 1 - auto limits, 2 - manual limits: ',choice

do_plot = (choice eq 2)
do_limits = (choice eq 1 or choice eq 2)

if do_plot then begin
	window,5,xsize=1200,ysize=500,title=root,xpos=50,ypos=200
	!p.multi=[0,3,1]
endif

h = histogram(chi,omin=omin,max=8.0,bins=0.01)
xx = findgen(n_elements(h)) * 0.01 + omin

t = where(h eq max(h))		; find peak of histogram
hmax = h(t(0))          	; get peak value
htlow = h
htlow(0:t(0)) = 1000000L	; set lower half of histogram to high value
t =where(htlow lt hmax*0.005,n)	; get 0.5% of peak value
if n le 0 then t = [0]
chilim = xx(t(0))       	; set chi limit
;chilim = 0.1
if do_limits then print,'Chi upper limit: ',chilim

if do_plot then begin
    plot,xx,h,xtitle='CHI',ytitle='N',xran=[omin,1.0],psym=10,charsize=3,xsty=1
    oplot,[chilim,chilim],[0,1000000L],linesty=3
endif

;
h = histogram(sharp, min=-50, max=50, omin=omin)
xx = findgen(n_elements(h)) + omin

t = where(h eq max(h))  	; find peak of histogram
hmax = h(t(0))          	; get peak value
htlow = h
htlow(t(0):*) = 1000000L	; set upper half to high value
hthi = h
hthi(0:t(0)) = 1000000L 	; set lower half to high value

tl = where(htlow lt hmax*0.1)	; get 10% of peak value
th = where(hthi  lt hmax*0.1)	; get 10% of peak value

sharpran = [xx(tl(n_elements(tl)-1)),xx(th(0))]
;sharpran = [-1., 1.]
if do_limits then print,'Sharp range [lo,hi]: ',sharpran

if do_plot then begin
    plot,xx,h,xtitle='SHARP',ytitle='N',xran=[-20,20],psym=10,charsize=3
    oplot,[sharpran(0),sharpran(0)],[0,1000000L],linesty=3
    oplot,[sharpran(1),sharpran(1)],[0,1000000L],linesty=3
endif

;
h = histogram(blunder, min=0., max=0.9, omin=omin,bins=0.01)
xx = findgen(n_elements(h)) * 0.01 + omin

t = where(h eq max(h))  	; find peak of histogram
hmax = h(t(0))          	; get peak value
htlow = h
htlow(t(0):*) = 1000000L	; set upper half to high value
hthi = h
hthi(0:t(0)) = 1000000L 	; set lower half to high value

tl = where(htlow lt hmax*0.01)	; get 1% of peak value
th = where(hthi  lt hmax*0.01)	; get 1% of peak value

blundran = [xx(tl(n_elements(tl)-1)),xx(th(0))]
;blundran = [0.,1.]
if do_limits then print,'Blunder range [lo,hi]: ',blundran

if do_plot then begin
    plot,xx,h,xtitle='BLUNDER',ytitle='N',xran=[0,1],psym=10,charsize=3
    oplot,[blundran(0),blundran(0)],[0,1000000L],linesty=3
    oplot,[blundran(1),blundran(1)],[0,1000000L],linesty=3
endif

;
; other limits
;
if do_limits then begin
	magran= [20.0,14.0]
	nflim = max(nframes)/2
	print,'Magnitude range: ',magran
	print,'Nframes range, limit: ',min(nframes),max(nframes),nflim
endif

if do_plot then begin
	read,'Chi upper limit               : ',chilim
	read,'Sharp range [lo,hi]           : ',sharpran
	read,'Bluder range [lo,hi]          : ',blundran
	read,'Magnitude range [faint,bright]: ',magran
	read,'nframes lower limit           : ',nflim
	!p.multi=0
	wdelete,5
endif
;
; apply limits

if do_limits then begin
  good = where(chi lt chilim and sharp gt sharpran(0) and sharp lt sharpran(1) $
	and blunder gt blundran(0) and blunder lt blundran(1) $
	and avmags lt magran(0) and avmags gt magran(1) $
	and nframes gt nflim, ngood)

  if ngood lt 5 then begin
	print,'LCUR_DAO_GET - ERROR: limits too strict, not enough points: ', $
		ngood
	return
  endif

  id		= id(good)
  x		= x(good)
  y		= y(good)
  nframes	= nframes(good)
  chi		= chi(good)
  sharp		= sharp(good)
  var		= var(good)
  blunder	= blunder(good)
  mags		= mags(*,good)
  merrs		= merrs(*,good)
endif else ngood = n_elements(id)
print,'Stars read in: ',ngood

;
; calculate enframes
if do_limits then $
	merrlim = 0.05 $
else	merrlim = 0.5
enframes = intarr(ngood)
for i=0,ngood-1 do begin
	t=where(merrs(*,i) le merrlim, n)
	enframes(i) = fix(n)
endfor
;rjd = -2L
;read,'Reference RJD (long) or -1 for *.cnd file, -2 to skip: ',rjd
;
;fake,mags,id
;newvar4,var,mags,merrs,jd,rjd,root,id,nvar
nvar=var	; set nvar equal to DAO var for now
;
; get nearest neighbor
magthr=5.0	; 1%
nearneighb,x,y,mags,merrs,magthr,rnei
;
; check number of points
if ( lcur_com_put('jd',jd) and lcur_com_put('dao_id',id) ) then begin

;
; get epoch number
	epnum = fix( long(jd) - long(min(jd)) )
	ret = lcur_com_put('epnum', epnum)
;
; get initial jdran, jdzer, and twin
	ret = lcur_com_put('jdran', [min(jd),max(jd)])
	ret = lcur_com_put('jdzero', (fix(min(jd)/1000) * 1000.d0) )
	ret = lcur_com_put('twin', [min(jd),max(jd)])
	ret = lcur_com_put('t0', 0)
	ret = lcur_com_put('t1', (ndates - 1) )

;
; set initial pran to default
	ret = lcur_com_put('def_pran', (1 eq 1) )
;
; store in common variables

	ret = lcur_com_put('pfile', dfile)
	ret = lcur_com_put('filt', ifilt)
;
; set dao variables
	ret = lcur_com_put('dao_filt',ifilt)
	ret = lcur_com_put('dao_x',x)
	ret = lcur_com_put('dao_y',y)
	ret = lcur_com_put('dao_rnei',rnei)
	ret = lcur_com_put('dao_nframes',nframes)
	ret = lcur_com_put('dao_chi',chi)
	ret = lcur_com_put('dao_sharp',sharp)
	ret = lcur_com_put('dao_varo',var)
	ret = lcur_com_put('dao_var',nvar)
	ret = lcur_com_put('dao_varth', [0.,0.])
	ret = lcur_com_put('dao_blunder',blunder)
	ret = lcur_com_put('dao_mags',mags)
	ret = lcur_com_put('dao_merrs',merrs)
	ret = lcur_com_put('dao_files',imfiles)
	ret = lcur_com_put('merrlim', merrlim)
	ret = lcur_com_put('dao_nflim',nflim)
	ret = lcur_com_put('dao_enframes',enframes)

	print,'lcur_dao_get: ',strn(nstars)+' stars, '+strn(ndates)+ $
		' points read in'
	print,'lcur_dao_get: ',strn(ngood)+' stars passed limits'
;
; read reference image
	dao_im=readfits('ref'+ch+'.fits',dao_imhdr)
	dao_imgno=-1
	switch ich of
		0:
		1:
		2:
		3: begin
			dao_im=transpose(dao_im)
			break
		end
		4:
		5:
		6:
		7: begin
			dao_im=rotate(dao_im,3)
			break
		end
		else: print,'Error - unknown chip: ',ich
	endswitch
;
; create display window
	window,2,xsize=512,ysize=512,title=imfile,retain=0
	wset,0
;
; set first star to examine
	lcur_dao_next,1

endif	
;
return
end	; pro lcur_dao_get
