pro gx_sncor,imfil,z
;+
;	gx_sncor - analyze intensity correlation with SN position
;
; INPUTS
;	imfil - input image filename
;	z     - redshift of SN
;
; KEYWORDS:
;	NONE
;
; HISTORY:
;	v1.0, neill@srl.caltech.edu 18-dec-06
;
;-
; check inputs
nn=1
p=0
if n_params(0) lt 1 then begin
	flist=file_search('*-nd-intbgsub.fits',count=nn)
	if nn le 0 then begin
		print,'Error no NUV ims found'
		return
	endif else if nn gt 1 then begin
		for i=0,nn-1 do print,i,' ',flist(i)
		p=-1
		while p lt 0 or p ge nn do read,'im #: ',p
		imfil=flist(p)
	endif else imfil=flist(0)
	z=0.
	read,'Enter redshift: ',z
endif
;
; get physical scale in pc/"
dz=1.d-5
pscl = comd(z,dz,71.,0.27,0.73) * 1.d6 / ( (1.+z) * 206265.d0 )
print,'Physical scale in pc/": ',pscl,format='(a,f9.2)'
print,'NUV resolution corresponds to ',pscl*5.6,' pc',format='(a,f9.2,a)'
;
; photometry setup
fzo = 6.18		; FUV zeropoint offset (25 - 18.82)
nzo = 4.92		; NUV zeropoint offset (25 - 20.08)
apr=[5.6/1.5]/2.0	; radius of one NUV resolution element
;
; setups
q='g'
th=3
si=1.25
!p.background=colordex('white')
!p.color=colordex('black')
if !d.name ne 'X' then !p.font=1 else !p.font=-1
;
; set up usersym (circle)
a=[findgen(16)*(!pi*2/16.),0.]
usersym,cos(a),sin(a)
;
; get directory info
cd,'./',current=cwd
dirs=strsplit(cwd,'/',/extract,count=nd)
snnm=dirs(nd-1)
srvy=dirs(nd-2)
type=dirs(nd-3)
tlab=snnm+': '+type+', '+srvy+', '+imfil
;
; get file info
tmp=imfil
rute=gettok(tmp,'.')
;
; get fuv image
i=strpos(imfil,'-nd-')
if i ge 0 then begin
	fuvfil=imfil
	strput,fuvfil,'-fd',i
	list=file_search(fuvfil,count=nf)
	if nf eq 1 then begin
		fuvim=readfits(fuvfil,hfuv)
		fxdim=sxpar(hfuv,'NAXIS1')
		fydim=sxpar(hfuv,'NAXIS2')
		if fxdim ne 512 or fydim ne 512 then begin
			print,'Warning: FUV image of wrong dims: ',fxdim,fydim
		endif
		aper,fuvim,255.5,255.5,fmags,fmerr,fsky,fskerr,1., $
			apr,[0.,0.],[0.,0.],setskyval=0.,/silent,/exact
		fmags = fmags - fzo
	endif else begin
		print,'Warning: no FUV image found: ',fuvfil
		fmags=99.999
		fmerr=9.999
	endelse
endif else begin
	print,'Error: NUV image not found: ',imfil
	return
endelse
;
; get image info
iim = readfits(imfil,h1)
xdim = sxpar(h1,'NAXIS1')
ydim = sxpar(h1,'NAXIS2')
if xdim ne 512 or ydim ne 512 then begin
	print,'Warning: NUV image of wrong dims: ',xdim,ydim
	if xdim lt 259 or ydim lt 259 then begin
		print,'Returning...'
		return
	endif
endif
aper,iim,255.5,255.5,nmags,nmerr,nsky,nskerr,1.,apr,[0.,0.],[0.,0.], $
	setskyval=0.,/silent,/exact,readnoise=1.
nmags = nmags - nzo
print,'FUV resolve: ',fmags(0),fmerr(0)
print,'FUV surface: ',fmags(0)+3.4787
print,'NUV resolve: ',nmags(0),nmerr(0)
print,'NUV surface: ',nmags(0)+3.4787
;
window,0,xpos=0,ypos=600,xsize=xdim*2,ysize=ydim*2,title=tlab
;
mn=mean(iim)
sg=stddev(iim)
lo=mn-(3.*sg)
hi=mn+(5.*sg)
dim=rebin(iim,xdim*2,ydim*2,/sample)
dim(0)=lo
dim(1)=hi
;
tvscl,dim>lo<hi
plot,[511,511],[511,511],psym=8,symsi=4,thick=5,/device,/noerase, $
	xsty=1,ysty=1,xran=[0,xdim*2-1],yran=[0,ydim*2-1], $
	pos=[0,0,xdim*2-1,ydim*2-1],color=128
pc=2000./pscl/1.5
oplot,[100,100+pc],[100,100],color=128,thick=th
xyouts,100,110,'2 kpc',color=128,charsi=1.5
;
fl='/Users/neill/snsites/Ia/analysis/archive.list'
readcol,fl,sn,ty,u,host,hty,ra,dec,z,del,a0,nexp,fexp,tile, /silent, $
	form='a,a,a,a,a,d,d,f,f,f,f,f,a'
;
t=where(strpos(sn,snnm) ge 0, n)
if n eq 1 then begin
	print,'del: ',del(t(0))
endif
;
return
end
