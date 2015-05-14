pro html_clear,ifil
;
;	html_clear - clear a file of html tokens and write out
;			a text file of the data
;
openr,il,ifil,/get_lun
tmp=ifil
rute=gettok(tmp,'.')
openw,ol,rute+'.txt',/get_lun
;
rec=''
orec=''
intok=(1 eq 0)
while not eof(il) do begin
	readf,il,rec
	for i = 0,strlen(rec)-1 do begin
		if strmid(rec,i,1) eq '<' then begin
			intok=(1 eq 1)
		endif else if strmid(rec,i,1) eq '>' then begin
			intok=(1 eq 0)
		endif else if not intok then orec=orec+strmid(rec,i,1)
	endfor
	print,orec
	printf,ol,orec
	orec=''
endwhile
;
free_lun,il,ol
;
return
end
