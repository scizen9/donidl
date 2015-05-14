pro daoap_lcur_prep
;
root = ''
read,'dao data set root (no ext): ',root
moff=0.
read,'magnitude offset (0.0): ',moff

print,'reading: ',root+'.mag...'
rddaomag,root+'.mag',mid,mx,my,mm,ss,nframes,mchi,mshrp,var,blunder
mm = mm + moff

print,'reading: ',root+'.tfr...'
rddaotfr, root+'.tfr', aplist, id, tx, ty, indx, offsets
ndates = n_elements(aplist)
nstars = n_elements(id)

nep = n_elements(aplist)
jd=dblarr(nep)
use=intarr(nep)+1
for i=0,n_elements(aplist)-1 do begin
	jd(i)=jdfname(aplist(i))
;	if strpos('0123456789',strmid(aplist(i),1,1)) lt 0 then $
;		use(i)=0
	if jd(i) lt 10000. then begin
		yr='20'+strmid(aplist(i),0,2)
		mo=strmid(aplist(i),2,2)
		da=strmid(aplist(i),4,2)
		year=fix(yr)
		month=fix(mo)
		day=fix(da)
		juldate,[year,month,day,0.,0.,0.],juld
		jd(i)=juld
	endif
endfor

print,'reading: ',root+'.cor...'
rddaocor, root+'.cor', ids, x, y, mags, merrs, chi, sharp
mags = mags+moff
print,'Done.'

t=where(use eq 1)

jd=jd(t)
mags=mags(t,*)
merrs=merrs(t,*)
ndates=n_elements(jd)

;filts = ['v']
filts = ['c']

ofile = root + '.dat'
openw,olun,ofile,/get_lun

writeu,olun,ndates,nstars
writeu,olun,jd
writeu,olun,ids,x,y,mm,ss,nframes,chi,sharp,var,blunder,mags,merrs

free_lun,olun

print,'Wrote file: ',ofile

return
end	; pro dao_lcur_prep
