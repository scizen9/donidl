pro get_ned1d,obj,snid=snid,mu=mu,merr=merr,dmpc=dmpc,ra=ra,dec=dec, $
		  glon=glon,glat=glat,type=type,vgsr=vgsr,silent=silent
;
; setups
mu=-9.
merr=-9.
dmpc=-9.
ra=''
dec=''
glon=-9.
glat=-99.
type=''
vgsr=-999999.
;
; read ned-1d.dat
readcol,!NED_DATA+'/ned-1d.dat',name,nmu,nmerr,nd,meth,ref,notes,nra,ndec,$
	nglon,nglat,ntyp,nv,format='a,f,f,f,a,a,a,a,a,f,f,a,f',/silent
;
; find it
t=where(strpos(name,obj) ge 0, nf)
if nf le 0 and keyword_set(snid) then $
		t=where(strpos(notes,strtrim(snid,2)) ge 0, nf)
if nf le 0 then begin
	if not keyword_set(silent) then $
		print,'GET_NED1D: Error - no records found for obj: ',obj
	return
endif

if nf eq 1 then begin
	mu=nmu(t(0))
	merr=nmerr(t(0))
	dmpc=nd(t(0))
endif else begin
	mus=nmu(t)
	muerrs=nmerr(t)
	dmpcs=nd(t)
	g=where(muerrs ge 0.,ng)
	if ng gt 0 then begin
		flx=10.^(-0.4*(mus(g)-25.0))
		err=(flx*muerrs(g))/1.0857362d0
		wflx = wmean(flx,err)
		dmpc = wmean(dmpcs,err)
		wflxe= wstdev(flx,err)
		mu = -2.5*alog10(wflx) + 25.0
		merr = 1.0857362d0 * (wflxe/wflx)
	endif else begin
		flx=10.^(-0.4*(mus-25.0))
		mflx = mean(flx)
		mu = -2.5*alog10(mflx) + 25.0
		merr = muerrs(0)
		dmpc = mean(dmpcs)
	endelse
endelse
t=t(0)
ra=nra(t)
dec=ndec(t)
glon=nglon(t)
glat=nglat(t)
type=ntyp(t)
vgsr=nv(t)
;
return
end
