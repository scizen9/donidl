pro daocogs
;
list=file_search('*.apcog',count=nf)
;
q=''
for i=0,nf-1 do begin
	print,list(i)
	daocog,list(i)
	if !d.name ne 'PS' then $
		read,'next: ',q
endfor
;
return
end
