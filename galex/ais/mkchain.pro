pro mkchain,skygrid_start,sgdat,grid,bslim,aisexp,ocdat,nchain,nutile

;pro mkchain,skygrid_start,grid,aisflg,sgdat

; updated for replanning AIS to 30k limit
; uses plotchains.pro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


label='SKYGRID:'+string(skygrid_start,format='(I3.3)')

; read in sky grid positions

;### remember that grid number is target id - 1

ra_center=sgdat[skygrid_start].ra
dec_center=sgdat[skygrid_start].dec

device,decomposed=1,retain=2
window,1,xsize=500,ysize=400,xpos=1,ypos=1600

plot,[0,0],[0,0],xrange=[180,-180],yrange=[-90,90],xsty=5,ysty=5,/nodata
aitoff,sgdat.ra,sgdat.dec,ax,ay
aitoff_grid,/label
plots,ax,ay,psym=6,symsize=0.7

; outline position on sky plot
plots,ax(skygrid_start),ay(skygrid_start),psym=6,color=getcolor('red',10),thick=3
if skygrid_start ne 491 then plots,ax(skygrid_start+1),ay(skygrid_start+1),psym=6,color=getcolor('blue',20),thick=3
if skygrid_start ne 0 then plots,ax(skygrid_start-1),ay(skygrid_start-1),psym=6,color=getcolor('green',30),thick=3


xyouts,190,85,label,color=getcolor('red',10),charsize=1.2
xyouts,190,75,'(last)',color=getcolor('green',30)
xyouts,190,65,'(next)',color=getcolor('blue',20)

xyouts,190,-75,strcompress('RA = '+string(ra_center))
xyouts,190,-85,strcompress('Dec ='+string(dec_center))

euler,ra_center,dec_center,gl,gb,1

xyouts,-120,-75,strcompress('l = '+string(gl))
xyouts,-100,-85,strcompress('b ='+string(gb))

;------------
; read in existing chains file

gridnum=replicate(-1,1L)
readcol,'chains.dat',chnum,gridnum,/silent

; mark as already planned
aisflg=replicate(0,47612)
if gridnum(0) ne -1 then aisflg[gridnum]=1 ; skips if chains.dat is header only

; read in recently done chains
flist=file_search('chains-*.dat', count=nchf)
if nchf gt 0 then begin
	for i=0,nchf-1 do begin
		readcol,flist[i],chnum5,gridnum5,/silent
		aisflg[gridnum5]=5
	endfor
endif

galex_radius=0.62
fieldsize=20.0
;fieldsize=90.0
npixel=1024
scale=fieldsize/double(npixel) ;;scale of plot in degrees per pixel
i100limit=15.0    ;;;upper limit for 100 micron flux


; define conversion parameters before calling ad2xy
astr= { CD: [ [1.d,0],[0.,1.d] ], $
        CDELT: [scale,scale], $
        CRPIX: [npixel/2,npixel/2], $
        CRVAL: [ra_center,dec_center], $
        CTYPE: ['RA---TAN','DEC--TAN'], $
	PV2: [0.,0.], $
	longpole: 180.d0, latpole: 0.d0 }


gcirc,2,ra_center,dec_center,grid.ra,grid.dec,dist
dist=dist/3600.d
w=where(dist lt (0.5*fieldsize))
ais=grid[w]
aisf=aisflg[w]
aislim=bslim[w]
gexp=aisexp[w]

gcirc,2,ra_center,dec_center,ocdat.ra,ocdat.dec,dist
dist=dist/3600.d
w=where(dist lt (0.5*fieldsize),nw)
ocgrid={ECLIPSE:0,TARGET_ID:'',RA:0.0d,DEC:0.0d,OWPOS:'',SURVEY:'',COMMENTS:''}
if nw gt 0 then ocgrid=ocdat[w]

!p.multi=[0,1,1]
window,0,xsize=600,ysize=700,xpos=700,ypos=550

if (!d.name eq 'PS') then begin	; If Postscript output mode
    !p.font=0					; select hardware fonts
    device,/helv,/isolatin1			; Helvetica ISOLatin fontset
    thk=4					; thick borders are nice
    graph_color=getcolor('black',10)
endif else begin			; If screen or other output mode
    !p.font=-1					; select Hershey fonts
    xyouts,0,0,/norm,'!6'			; Set to Triplex Roman font
    thk=1					; use regular thickness
    graph_color=getcolor('white',10)
endelse


;;project field centers into image coordinates
ad2xy,ais.ra,ais.dec,astr,xgal,ygal
xgal=(xgal-npixel/2)*scale
ygal=(ygal-npixel/2)*scale
n=50
theta=2*!pi/(1.*n)*findgen(n+1)
xcirc=galex_radius*cos(theta)
ycirc=galex_radius*sin(theta)
;;first plot circles with no bright stars
starflag=''

; bright star limit set to nuv > 5000
btmp=1

;;;now create blank chains structure
maxnchain=100         ; maximum 100 chains generated in any grid position
;maxntiles=12          ; maximum of 12 fields in a chain
maxntiles=15          ; maximum of 12 fields in a chain
idc={chainstuff,id:lonarr(maxntiles),ntile:0,x:fltarr(maxntiles),y:fltarr(maxntiles)}
idc=replicate(idc,maxnchain)

nchain=0
nutile = intarr(maxnchain)

;#### plots from here
plotchains,ais,aisf,astr,fieldsize,npixel,galex_radius,idc,label,i100limit, $
	sgdat,aislim,gexp,ocgrid

quitflg=0
while (quitflg eq 0) and (nchain lt maxnchain) do begin
	id=lonarr(maxntiles)
	xc=dblarr(maxntiles)
	yc=dblarr(maxntiles)

	print,'Click on the starting tile with the left mouse button'
	dmin=9999.0
	while (dmin gt 0.6) do begin
		cursor,x0,y0,/data,/up
		if y0 gt fieldsize*0.5 and $
		   abs(x0) lt fieldsize*0.1 then begin
			quitflg=1
			goto,quitit
		endif
		dist=sqrt((x0-xgal)^2+(y0-ygal)^2)
		dmin=min(dist,p)
		if (aislim(p).bs eq 1 or aislim(p).fld eq 1) then dmin=9999.0
	endwhile
	id(0)=long(strmid(ais(p).TARGET_ID,5,5))
	xc(0)=xgal(p)
	yc(0)=ygal(p)
	print,'Starting tile: ',long(strmid(ais(p).TARGET_ID,5,5)),ais(p).ra,ais(p).dec

	; keep ra,dec info
	cra=ais(p).ra & cdec=ais(p).dec

	plots,xgal(p),ygal(p),psym=4,color=getcolor('red',13),symsize=1.5,$
		thick=4
	ntile=1
	last=0
	print,'Use left button to add tiles to the chain'
	print,'Use the right button to mark the end of the chain'
	distmax=1.5
	repeat begin
            ;;;now overplot the chains
	    dmin=9999.0
            while(dmin gt 0.6) do begin
	    	cursor,xtmp,ytmp,/data,/up
		dist=sqrt((xtmp-xgal)^2+(ytmp-ygal)^2)
		dmin=min(dist,p)
		if sqrt((xtmp-xc[ntile-1])^2+(ytmp-yc[ntile-1])^2) LT distmax then begin
			distflg=0
		endif else begin
			distflg=1
		endelse
		if (aislim(p).bs eq 1 or aislim(p).fld eq 1) then dmin=9999.0
	    endwhile
	    id(ntile)=long(strmid(ais(p).TARGET_ID,5,5))
	    xc(ntile)=xgal(p)
	    yc(ntile)=ygal(p)
	    print,ntile+1,long(strmid(ais(p).TARGET_ID,5,5)),ais(p).RA,ais(p).DEC
	    cra=[cra,ais(p).RA] & cdec=[cdec,ais(p).DEC]

	    oplot,[xc(ntile-1),xc(ntile)],[yc(ntile-1),yc(ntile)],$
		color=getcolor('green',13)
	    if (!mouse.button eq 4) or (ntile eq maxntiles-1) then begin
		last=1
		plots,xc(ntile),yc(ntile),psym=4,$
			color=getcolor('blue',13),symsize=1.5,thick=4
	    endif else begin
		last=0
		plots,xc(ntile),yc(ntile),psym=4,$
			color=getcolor('green',13),thick=4
	    endelse
	    ntile=ntile+1
        endrep until last eq 1

	; run checks here
	; first save to temporary dat file
	print, 'Checking chain...'
	get_lun,lun
	openw,lun,'tmp.dat'
	for i=0,ntile-1 do printf,lun,i,id(i),format='(2i7)'
	close,lun
	free_lun,lun
	; then run addpatt to get the resulting ra,dec & twist
	addpatt,file='tmp.dat'
	; then run part of aischk
	readcol,'tmp.tdb',tgrid,toffset,tmode,tra,tdec,ttwist,tscanx,tscany, $
		format='A,F,A,D,D,D,F,F',skipline=1,/silent
	tmk=0
	for j=0,n_elements(tgrid)-2 do begin
		mk=0
		q1=qnorm(rdt2q([tra[j],tdec[j],ttwist[j]]))
		q2=qnorm(rdt2q([tra[j+1],tdec[j+1],ttwist[j+1]]))
		qdelt=qnorm(qmult(qconj(q1),q2))
		euler=acos(qdelt[3])*2.0*!RADEG
		if (euler gt 1.3) then mk=1
		if ((j ne 0) and (euler lt .6)) then mk=1
		if mk eq 1 then $
			plots,[xc(j),xc(j+1)],[yc(j),yc(j+1)],psym=-3,thick=4, $
				color=getcolor('red',13)
		if mk eq 1 then tmk=tmk+1
	endfor

	; aischk says it is good
	if tmk eq 0 then begin
  		print, 'Chain PASSES euler angle check'
        	print,'Accept/reject this chain?'
		ytmp=fieldsize*(-0.5)

		; read until click above plot
		while (ytmp lt fieldsize*(0.5)) do begin
			cursor,xtmp,ytmp,/data,/up
			; are we above plot?
			if (ytmp gt fieldsize*0.5) then begin
				; accept chain
				if (xtmp LT fieldsize*(-0.1)) then begin
					idc[nchain].id=id
					idc[nchain].x=xc
					idc[nchain].y=yc
					idc[nchain].ntile=ntile
					; get unique tiles
					id = id[where(id ne 0L)]
					uq = uniq(id[sort(id)])
					nutile[nchain] = n_elements(uq)
					nchain=nchain+1
				; quit
       				endif else $
       					if (abs(xtmp) lt fieldsize*(0.1)) then $
        					quitflg=1
			endif
			; if above plot and not quit or accept, reject chain
		endwhile
	; aischk says it is bad
	endif else begin
		print, '### Chain rejected (bad links in red)'
		print, '...replotting in 5 secs...'
		wait,5
		quitflg=0
	endelse

	if quitflg eq 0 then begin
	;;;replot the chains if we aren't quitting
		plotchains,ais,aisf,astr,fieldsize,npixel,galex_radius,idc, $
			label,i100limit,sgdat,aislim,gexp,ocgrid
	endif
endwhile

;; here, we quit to
quitit:

if nchain gt 0 then begin
	nutile = nutile[0:nchain-1]
	print,'Writing to chains.dat file.'
	get_lun,lun
	openw,lun,'chains.dat',/append
	for i=0,nchain-1 do begin
            for j=0,idc(i).ntile-1 do printf,lun,j,idc(i).id(j),format='(2i7)'
	endfor
	close,lun
	free_lun,lun
	print,'Writing to chains-'+string(skygrid_start,format='(I3.3)')+'.dat file.'
	get_lun,lun
	openw,lun,strcompress('chains-'+string(skygrid_start,format='(I3.3)')+'.dat',/remove_all)
	for i=0,nchain-1 do begin
            for j=0,idc(i).ntile-1 do printf,lun,j,idc(i).id(j),format='(2i7)'
	endfor
	close,lun
	free_lun,lun
endif

return
end

;===== STARTS HERE

path='/Users/neill/galex/ais_chains/data/'

; sky grid centers
sgdat=mrdfits(path+'sky_grid-tdb.fits',1)

; read in AIS grid positions
grid=mrdfits(path+'ais-grid-tdb.fits',1)
p=where(grid.ra lt 0.0,np)
if np gt 0 then grid[p].ra=grid[p].ra+ 360.0d

; read in flux limit data
; based on run of chkbstar with limits
; NUV/FUV point source <= 30k/7k
; NUV/FUV field brightness <= 80k/15k
; run using ais-grid-chkbs30-80k-td1.sc
; output in 
;bslim=mrdfits(path+'ais-grid-limits30-80k-td1-tab.fits',1)

;new file (Apr 2011) based on 10^7 cts/s FUV and global limits
;bslim=mrdfits(path+'ais-grid-limits.fits',1)
bslim=mrdfits(path+'ais-grid-nolimits.fits',1)

; read in exposure time data (Apr 2011 based on GR6 exposure)
;aisexp=mrdfits(path+'ais-grid-exp-dph-dat.fits',1)
aisexp=mrdfits(path+'ais-grid-exp-dat.fits',1)

; read in overcountrate shutdown data
ocdat=mrdfits(path+'overcountrate-dat.fits',1)

stgrid=0
read,stgrid,prompt='Enter starting grid: '
for i=stgrid,491 do begin
  if i ne stgrid then begin
    yn='n'
    read,yn,prompt='Next (y/n)? '
    if strupcase(yn) ne 'Y' then break
  endif
  mkchain,i,sgdat,grid,bslim,aisexp,ocdat,nchain,nutile
  if nchain gt 0 then begin
  	jname='sg'+string(i,form='(i03)')+'.jpg'
  	mk_jpeg,jname,window=0,qual=100
  	print,'Created: ',jname
	openw,ol,'nlegs.list',/append,/get_lun
	ostr = string(i,form='(i03)')+"	"+strn(1)+"	"+strn(nutile[0])
	print,ostr
	printf,ol,ostr
	if nchain gt 1 then $
		for j=1,nchain-1 do begin
			ostr = "	"+strn(j+1)+"	"+strn(nutile[j])
			print,ostr
			printf,ol,ostr
		endfor
	free_lun,ol
  endif
  print,'Working on next field.'
endfor
end

