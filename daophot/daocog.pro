pro daocog, apfl, pstar, apmax=apmax
;
; daocog - plot up dao curve of growth data
;
if n_params(0) lt 2 then pstar=0
;
rddaoap,apfl,id,x,y,mags,merrs,sky,skysig
nstr=n_elements(id)
;
; get header info
tmp=apfl
rute=gettok(tmp,'.')
h=headfits(rute+'.fits')
airmass=sxpar(h,'AIRMASS')
phot_k=sxpar(h,'PHOT_K')
exptime=sxpar(h,'EXPTIME')
filter=strmid(sxpar(h,'FILTER'),0,1)
object=strtrim(sxpar(h,'OBJECT'),2)
dateobs=strtrim(sxpar(h,'DATE-OBS'),2)
print,'OBJECT : ',object
print,'FILTER : ',filter
print,'DATEOBS: ',dateobs
print,'EXPTIME: ',exptime
print,'AIRMASS: ',airmass
print,'PHOT_K : ',phot_k
;
; read phot.opt
openr,pl,'photocog.opt',/get_lun
rec=''
apr=fltarr(12)
r=0.
for i=0,11 do begin
	readf,pl,r,format='(5x,f4.1)'
	apr(i)=r
endfor
free_lun,pl
;
dels=mags-mags
dele=merrs-merrs
dels(0,*)=100.
dele(0,*)=10.
if not keyword_set(apmax) then begin
	for i=1,11 do begin
		dels(i,*)=mags(i-1,*)-mags(i,*)
		dele(i,*)=sqrt(merrs(i-1,*)^2+merrs(i,*)^2)
	endfor
	ylab='!MD!3MAG (Ap!Di-1!N - Ap!Di!N)'
endif else begin
	for i=0,11 do begin
		dels(i,*)=mags(i,*)-mags(11,*)
		dele(i,*)=sqrt(merrs(i,*)^2+merrs(11,*)^2)
	endfor
	ylab='!MD!3MAG (Ap!Di!N - Ap!DMAX!N)'
endelse
;
maglim=14.0
plot,apr(1:*),apr(1:*),psym=4,xtitle='AP RAD (px)', ytitle=ylab, $
      xran=[18,31],xsty=1,yran=[-0.01,0.05],ysty=1,thick=3,charsi=2,charthi=3,$
	xthick=3,ythick=3, $
	title=apfl+' '+object+' '+filter+' '+dateobs+' Star: '+strn(pstar+1)
	;+' < '+strn(maglim,len=4)+' Mag'
oplot,[-100,100],[0,0]
oplot,[-100,100],[0.01,0.01],linesty=3
for i=2,11 do oplot,[apr(i),apr(i)],[-0.05,0]
oplot,[21.5,21.5],[-0.5,0],thick=5
flags=id-id
;
;for n=0,nstr-1 do begin
;    if mags(0,n) lt maglim and merrs(5,n) lt 0.5 then begin
    	oplot,apr,dels(*,pstar),psym=pstar+4
	errplot,apr,dels(*,pstar)-dele(*,pstar),dels(*,pstar)+dele(*,pstar),width=0.02
	flags(pstar)=1
;    endif
;endfor
;
good=where(flags eq 1)
avdels=fltarr(12)
avdels(0)=100.
for i=1,11 do begin
	ims,dels(i,good),mean,sigma,siglim=2.0
	avdels(i)=mean
endfor
oplot,apr,avdels
;
;
cmag=mags(*,pstar)+phot_k*(airmass-1.)+2.5*alog10(exptime)
leg=strarr(14)
leg(1) = 'PHOT_K = '+strn(phot_k,form='(f7.3)')+', AIRMASS = '+$
	strn(airmass,form='(f5.3)')+', EXPTIME = '+strn(exptime,form='(f7.3)')
leg(0) = 'M = m + PHOT_K * (AIRMASS - 1) + 2.5 * LOG!D10!N(EXPTIME)'
for i=2,13 do leg(i) = 'AP'+strn(i-1,form='(i02)')+': '+$
	strn(apr(i-2),form='(f5.2)') + 'px, M = '+ $
	strn(cmag(i-2)-25.0,form='(f7.3)')+' +- '+$
	strn(merrs(i-2,pstar),form='(f5.3)')
legend,leg,charsi=1.5,charthi=3,spac=1.5,/right,box=0
;
return
end
