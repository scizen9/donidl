pro get_vpx_stats2,ais=ais
;
; NUV
cd,'/home/hugi/neill/soda/artif2',current=cwd
;
if keyword_set(ais) then begin
	nsrchstr='*/AIS*-nd-varpix_mask.fits.gz'
	fsrchstr='*/AIS*-fd-varpix_mask.fits.gz'
	nfil = '/Users/neill/soda/artif/stats_ais_nuv.dat'
	ffil = '/Users/neill/soda/artif/stats_ais_fuv.dat'
endif else begin
	nsrchstr='*/*-nd-varpix_mask.fits.gz'
	fsrchstr='*/*-fd-varpix_mask.fits.gz'
	nfil = '/Users/neill/soda/artif/stats_nuv.dat'
	ffil = '/Users/neill/soda/artif/stats_fuv.dat'
endelse
;
flist = file_search(nsrchstr,count=nf)
;
openw,1,nfil
for i=0,nf-1 do begin
	dat=mrdfits(flist(i),0,h,/silent,/compress)
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
	dat=mrdfits(flist(i),0,h,/silent,/compress)
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
