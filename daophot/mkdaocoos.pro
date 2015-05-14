pro mkdaocoos,fspec,nx,ny
;
if n_params(0) lt 3 then begin
	print,'MKDAOCOOS: Usage - mkdaocoos, fspec, nx, ny'
	return
endif
;
list=findfile(fspec,count=nf)
if nf le 0 then begin
	print,'MKDAOCOOS - Error: no files found ',fspec
	return
endif
;
for i=0,nf-1 do begin
	print,list(i)
	mkdaocoo,list(i),nx,ny
endfor
;
return
end
