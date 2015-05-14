pro get_vpx_stats,ais=ais
;
; NUV
cd,'/home/hugi/neill/soda/artif',current=cwd
;
if keyword_set(ais) then begin
	nsrchstr='*/AIS*-nd-objmask_varpix.fits'
	fsrchstr='*/AIS*-fd-objmask_varpix.fits'
	nfil = '/Users/neill/soda/artif/stats_ais_nuv.dat'
	ffil = '/Users/neill/soda/artif/stats_ais_fuv.dat'
endif else begin
	nsrchstr='*/*-nd-objmask_varpix.fits'
	fsrchstr='*/*-fd-objmask_varpix.fits'
	nfil = '/Users/neill/soda/artif/stats_nuv.dat'
	ffil = '/Users/neill/soda/artif/stats_fuv.dat'
endelse
;
flist = file_search(nsrchstr,count=nf)
;
openw,1,nfil
for i=0,nf-1 do begin
	dat=mrdfits(flist(i),0,h,/silent)
	t=where(dat gt 0., n)
	ecl=long(gettok(flist(i),'/'))
	printf,1,ecl,n,flist(i),format='(i12,i12,2x,a)'
	print,ecl,n,flist(i),format='(i12,i12,2x,a)'
endfor
close,1
print,' '
;
; FUV
flist = file_search(fsrchstr,count=nf)
;
openw,1,ffil
for i=0,nf-1 do begin
	dat=mrdfits(flist(i),0,h,/silent)
	t=where(dat gt 0., n)
	ecl=long(gettok(flist(i),'/'))
	printf,1,ecl,n,flist(i),format='(i12,i12,2x,a)'
	print,ecl,n,flist(i),format='(i12,i12,2x,a)'
endfor
close,1
;
cd,cwd
;
return
end
