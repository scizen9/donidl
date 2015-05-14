npf=3000.
readcol,'../../info/mk06_dat.txt',idn,n,sra,sdec,f='F,A,F,F'

readcol,'../../info/mk06_d25.txt',idn,n,maj,f='F,A,F'
name=n

;rah=float(strmid(sra,0,2))
;ram=float(strmid(sra,2,2))
;ras=float(strmid(sra,4,4))
;dd=float(strmid(sdec,0,3))
;dm=float(strmid(sdec,3,2))
;ds=float(strmid(sdec,5,4))

;good=where(maj LT 60)
;rad=tenv(rah,ram,ras)*15.
;decd=tenv(dd,dm,ds)

nobj=n_elements(name)
good=findgen(nobj)
objid=idn

rad=sra
decd=sdec
;maj=fltarr(nobj)+3.
maj[where(maj LT 0)]=6.0
r=maj/60.*1.5
;good=where(maj/2 GT 3)

objid=findgen(n_elements(name))
;objid=objid[good]
cover_points,rad[good],decd[good],r[good],objid[good],allobj,allcoo
nobj=n_elements(allobj)
nf=ceil(nobj/npf)

for j=0,nf-1 do begin
  openw,1,'foot'+ts(j)+'.list'
  printf,1,'ra,dec'
  for i=j*npf,min([(j+1)*npf-1,nobj-1]) do printf,1,allcoo[i,0],allcoo[i,1],format='(F8.4,",",F8.4)'
  close,1
print,i,j
endfor

openw,2,'objmultipos.list'
printf,2,'id,ra,dec,name'
for i=0, n_elements(allobj)-1 do printf,2,allobj[i],allcoo[i,0],allcoo[i,1],name[allobj[i]],format='(I3," ",F8.4," ",F8.4," ",A)'


close,2


end
