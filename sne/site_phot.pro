pro site_phot,snlist
;
; Perform site photometry and analysis on sn host images
;
common sndb_info	; sndat structure
;
; SN types
tylist=[ $
	'I', $
	'Ia', $
	'Ib', 'Ib/c', 'Ic', $
	'IIn', 'IIb', $
	'IIL', 'IIP', $
	'II' ]
nty = n_elements(tylist)
;
; open analysis log file
;lfile='sitean.log'
;filestamp,lfile
;openw,ol,lfile,/get_lun
;printf,ol,'# SITE_PHOT run on '+systime(0)
;printf,ol,'# SN          mF   mFerr   mF.5  mFerr    mF1  mFerr    mF2  mFerr   sbF     mN   mNerr   mN.5  mNerr    mN1  mNerr    mN2  mNerr   sbN   host           expt       snx      sny  file'
;
; loop over snlist
nl = n_elements(snlist)
for k=0,nl-1 do begin
    p=snfind(snlist(k))
    tyn=sndat(p).tyn
;
; get GALEX images
    flist = $
    file_search(!SNE_HOSTS+tylist(tyn)+'/'+snlist(k)+'/*intbgsub.fits',count=nf)
;
; loop over images
    q=''
    for i=0,nf-1 do begin
;
; header
	hdr = headfits(flist(i))
	ftile = sxpar(hdr,'tile')
	while strpos(ftile,'_') ge 0 do strput,ftile,'-',strpos(ftile,'_')
	band = sxpar(hdr,'band')
	srv = sxpar(hdr,'mpstype')
;
; sn name
	sn=snlist(k)
;
; is sn in sndat?
	host = sndat(p).host
	snty = sndat(p).type
;
; analyze images
	print,flist(i)
	if sndat(p).off_ew gt -9990. and sndat(p).off_ns gt -9990. then begin
		adoffset,sndat(p).hra,sndat(p).hdec, $
			sndat(p).off_ew,sndat(p).off_ns,0,raoff,decoff
		delra = sndat(p).ra - raoff
		deldec= sndat(p).dec - decoff
	endif else begin
		raoff = -99.0
		decoff = -99.0
		delra = -9999.0
		deldec = -9999.0
	endelse
	fmt = '(a,2f13.8)'
	print,'SN ra, dec: ',sndat(p).ra,sndat(p).dec,form=fmt
	print,'Deltas    : ',delra,deldec,form=fmt
	im=mrdfits(flist(i),0,hdr,/silent)
	expt=sxpar(hdr,'EXPTIME')
	ima = im*expt+1000.	; create count image (with 1000 count bias)
	nx = sxpar(hdr,'NAXIS1')
	ny = sxpar(hdr,'NAXIS2')
	nx2 = nx*2
	ny2 = ny*2
	window,0,xsize=nx2,ysize=ny2,title=snlist(k),xpos=1100,ypos=1200
;
; display image
	sig = stddev(ima)
	lo = 1000. - 2.*sig
	hi = 1000. + 5.*sig
	imd = rebin(smooth(ima,2)>0,nx2,ny2,/sample)
	imd(0) = lo
	imd(1) = hi
	tvscl, imd>lo<hi
	plot,[0,0],[0,0],/device,/noerase,/nodata, $
		xran=[0,(nx2-1)],yran=[0,(ny2-1)], xsty=1, ysty=1, $
		pos=[0,0,(nx2-1),(ny2-1)]
;
; get nominal x,y in image
	rastr=''
	decstr=''
	coostr=adstring(sndat(p).ra,sndat(p).dec,1)
	for j=0,2 do $
	    if j ne 2 then $
		    rastr=rastr+gettok(coostr,' ')+':' $
	    else    rastr=rastr+gettok(coostr,' ')
	for j=0,2 do $
	    if j ne 2 then $
		    decstr=decstr+gettok(coostr,' ')+':' $
	    else    decstr=decstr+gettok(coostr,' ')
;
; get sn coords in image
	cmd='sky2xy '+flist(i)+' '+rastr+' '+decstr
	spawn,cmd,res
;
; make sure we're not off the image
	if strpos(res,'ff') lt 0 then begin
		stb=strsplit(res,/extract)
		snx=float(stb(4))
		sny=float(stb(5))
		plots,snx*2.,sny*2.,psym=6,symsize=2.0,color=250,/dev
;
; we are off the image
    	endif
;
; get offset x,y in image
    if raoff gt 0. and decoff ge -90. then begin
	rastr=''
	decstr=''
	coostr=adstring(raoff,decoff,1)
	for j=0,2 do $
	    if j ne 2 then $
		    rastr=rastr+gettok(coostr,' ')+':' $
	    else    rastr=rastr+gettok(coostr,' ')
	for j=0,2 do $
	    if j ne 2 then $
		    decstr=decstr+gettok(coostr,' ')+':' $
	    else    decstr=decstr+gettok(coostr,' ')
;
; get sn coords in image
	cmd='sky2xy '+flist(i)+' '+rastr+' '+decstr
	spawn,cmd,res
;
; make sure we're not off the image
q=''
	if strpos(res,'ff') lt 0 then begin
		stb=strsplit(res,/extract)
		snx=float(stb(4))
		sny=float(stb(5))
		plots,snx*2.,sny*2.,psym=5,symsize=2.0,color=252,/dev
;
; we are off the image
    	endif
    endif
    read,'eh? ',q
;
; print out results
;    	print,sn,host,snty,mgf(0),mgfe(0),sbf,mgn(0),mgne(0),sbn,expt, $
;		flist(i), format='(a-10,a-15,a-10,6f7.2,2x,f9.1,2x,a)'
;    	printf,ol,sn,mgf(0),mgfe(0),mgf(1),mgfe(1),mgf(2),mgfe(2), $
;	    mgf(3),mgfe(3),sbf,mgn(0),mgne(0),mgn(1),mgne(1),mgn(2),mgne(2), $
;	    mgn(3),mgne(3),sbn,host,expt,snx,sny,flist(i),$
;	format='(a-10,18f7.2,2x,a-15,3f9.1,2x,a)'
    endfor	; loop over GALEX images
endfor	; loop over snlist
;
; close files
;free_lun,ol
;
return
end
