PRO mimap_swarp_config,ra=ra,dec=dec,ps=ps,size=size,uims=uims,wtype=wtype, $
	ctype=ctype,rtype=rtype,osample=osample,fstype=fsctype, $
	outimname=outimname,weightname=weightname,default=default,interp=interp

if keyword_set(default) EQ 0 then default='~/idl/mosaic/mimap_default.swarp'

;
; read in default file
rec=''
par=[rec]
val=[rec]
openr,il,default,/get_lun
while not eof(il) do begin
	readf,il,rec
	rec = gettok(rec,'#')
	if strlen(rec) gt 0 then begin
		p = gettok(rec,' ')
		v = strtrim(rec,2)
		par=[par,p]
		val=[val,v]
	endif
endwhile
free_lun,il
par=par[1:*]
val=val[1:*]
npar=n_elements(par)

if keyword_set(outimname) then begin
  a=where(par EQ 'IMAGEOUT_NAME')
  val[a]=outimname
endif
if keyword_set(weightname) then $
   val[where(par EQ 'WEIGHTOUT_NAME')]=weightname 

;setup output image
if keyword_set(ra) then begin
  a=where(par EQ 'CENTER_TYPE')
  val[a]='MANUAL'
  rastring=string(ra,format='(F10.5)')
  decstring=string(dec,format='(F10.5)')
  a=where(par EQ 'CENTER')
  val[a]=rastring+', '+decstring
endif

if keyword_set(ps) then begin
  sz=ceil(size/ps)
  a=where(par EQ 'PIXELSCALE_TYPE')
  val[a]='MANUAL'
  a=where(par EQ 'PIXEL_SCALE')
  val[a]=string(ps[0],format='(F7.4)')+', '+string(ps[1],format='(F7.4)')
  a=where(par EQ 'IMAGE_SIZE')
  val[a]=string(sz[0])+', '+string(sz[1])
endif


if keyword_set(ctype) then begin
  a=where(par EQ 'COMBINE')
  val[a]='Y'
  val[a+1]=ctype
endif else val[a]='N'

if keyword_set(rtype) then begin
  a=where(par EQ 'RESAMPLING_TYPE')
  val[a]=rtype
endif

if keyword_set(osample) then val[where(par EQ 'OVERSAMPLING')]=osample

if keyword_set(interp) then val[where(par EQ 'INTERPOLATE')]='Y'
if keyword_set(wtype) then begin
  val[where(par EQ 'WEIGHT_TYPE')]=wtype
  ustring=''
  for i=0,n_elements(uims)-1 do $
    ustring=ustring+uims[i]+ ' '
  a=where(par EQ 'WEIGHT_IMAGE')
  val[a]=ustring
endif
  
close,1
openw,1,'mimap.swarp',width=10000
for i=0,npar-1 do begin
  printf,1,par[i],'    ',val[i]
endfor

close,1

end

