pro mkdaoaps,fspec,nx,ny,ap1
;
if n_params(0) lt 4 then begin
	print,'MKDAOAPS: Usage - mkdaoaps, fspec, ny, ny, ap1'
	return
endif
;
list=findfile(fspec,count=nf)
if nf le 0 then begin
	print,'MKDAOAPS - Error: no files found ',fspec
	return
endif
;
; check which SExtractor mag is wanted
test=list(0)
rute=gettok(test,'.')
mbest=(1 eq 0)
if test eq 'sex' then begin
	q=''
	read,'Mag Best? (y/N): ',q
	if strupcase(strmid(strtrim(q,2),0,1)) eq 'Y' then $
		mbest=(1 eq 1)
endif
;
for i=0,nf-1 do begin
	print,list(i)
	if mbest then $
		mkdaoap,list(i),nx,ny,ap1,/magbest $
	else	mkdaoap,list(i),nx,ny,ap1
endfor
;
return
end
