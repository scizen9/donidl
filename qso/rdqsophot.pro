pro rdqsophot,ifile,ra,dec,dis,fm,fme,nm,nme,obdate,expt,eclipse,opsv,tile, $
	verbose=verbose,hline=hline, stat=stat
;+
; rdqsophot - read in GALEX photometry files for the qso project
;
; INPUTS:
;
;	IFILE - photometry filename
;-
;
stat=-1
flist=file_search(ifile,count=nf)
if nf ne 1 then begin
	print,'RDQSOPHOT: Error - file not found: ',ifile
	return
endif
;
fmt='(f11.6,f12.6,f6.1,7f8.2,7f7.2,7f8.2,7f7.2,2x,a16,f7.1,i8,2x,a12,2x,a)'
;fmt='(f11.6,f12.6,f6.1,7f8.2,7f7.2,7f8.2,7f7.2)'
nmax=5000L
nap=7
ra=dblarr(nmax)
dec=dblarr(nmax)
dis=fltarr(nmax)
fm=fltarr(nmax,nap)
fme=fltarr(nmax,nap)
nm=fltarr(nmax,nap)
nme=fltarr(nmax,nap)
obdate=strarr(nmax)
expt=fltarr(nmax)
eclipse=lonarr(nmax)
opsv=strarr(nmax)
tile=strarr(nmax)
;
r=0. & d=0. & s=0.
f=fltarr(nap) 
fe=fltarr(nap)
n=fltarr(nap)
nne=fltarr(nap)
dt=''
ex=0.
ec=0L
ov=''
tl=''
i=0L
;
; read header line
openr,il,ifile,/get_lun
hline=' '
readf,il,hline
if keyword_set(verbose) then print,hline
;
rec = ''
while not eof(il) do begin
	readf,il,rec
	if strpos(rec, 'NO COVERAGE') ge 0 then begin
		free_lun,il
		return
	endif
	reads,rec,r,d,s,f,fe,n,nne,dt,ex,ec,ov,tl,format=fmt
	ra(i)=r
	dec(i)=d
	dis(i)=s
	fm(i,*)=f(*)
	fme(i,*)=fe(*)
	nm(i,*)=n(*)
	nme(i,*)=nne(*)
	obdate(i)=dt
	expt(i)=ex
	eclipse(i)=ec
	opsv(i)=ov
	tile(i)=tl
	i=i+1L
endwhile
free_lun,il
;
if keyword_set(verbose) then print,'Pts read in: ',i
ra=ra(0:(i-1))
dec=dec(0:(i-1))
dis=dis(0:(i-1))
fm=fm(0:(i-1),*)
fme=fme(0:(i-1),*)
nm=nm(0:(i-1),*)
nme=nme(0:(i-1),*)
obdate=obdate(0:(i-1))
expt=expt(0:(i-1))
eclipse=eclipse(0:(i-1))
opsv=opsv(0:(i-1))
tile=tile(0:(i-1))
;
stat=1
;
return
end
