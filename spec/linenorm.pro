pro linenorm,fspec
;
; calculate line ratios and print out a log
;
flist=file_search(fspec,count=nf)
;
;
fmt1='(a,2f9.3)'
fmt2='(a,2g11.4,f9.1)'
for i=0,nf-1 do begin
	file=flist(i)
	print,' '
	print,'Normalizing: ',file
	readcol,file,name,c1,c2,cl,v,npx,flx,flxe,s2n, $
		format='a,f,f,f,f,i,f,f,f',/silent
	rute=gettok(file,'.')
	ext=file
;
; normalize on Hbeta
	fxb=lineflx(4861.,cl,flx,flxe,s2n,c2,name,point=pb)
	if fxb gt 0. then begin
		flxo = 100. * (flx/fxb)
		flxe = 100. * (flxe/fxb)
		flxe = sqrt(flxe^2+flxe(pb)^2)
;
; sort
		s=sort(cl)
		name=name(s)
		c1=c1(s)
		c2=c2(s)
		cl=cl(s)
		v=v(s)
		npx=npx(s)
		flxo=flxo(s)
		flxe=flxe(s)
		s2n=s2n(s)
;
; print out
		openw,ol,rute+'.norm',/get_lun
		printf,ol,'# Line Norm: '+systime(0)
		printf,ol,'# '+rute
		printf,ol,'#     Line    Centr1    Centr2    LabCentr   V(kms)   Npx  Flux       Error        S/N'
		fmt='(a10,2x,4f10.3,i5,2g11.4,f7.1)'
		for j=0,n_elements(flxo)-1 do $
			printf,ol,name(j),c1(j),c2(j),cl(j),v(j),npx(j), $
			flxo(j),flxe(j),s2n(j),form=fmt
		free_lun,ol
	endif else print,'H beta not measured'
endfor
;
return
end
