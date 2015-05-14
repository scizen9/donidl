pro mkdaomatscr,scrfl,mchfile,pfiles,nlev
;
; make daomatch script files
;
npfiles=n_elements(pfiles)
if npfiles le 1 then begin
	print,'MKDAOMATSCR - Error not enough files to match: ',npfiles, $
	' ',pfiles
	return
endif
;
openw,sl,scrfl,/get_lun
;
printf,sl,pfiles(0)
printf,sl,mchfile
for i=1,npfiles-1 do begin
	printf,sl,pfiles(i)
	for j=0,nlev-1 do printf,sl,'y'
	printf,sl,'n'
endfor
printf,sl,''
free_lun,sl
;
return
end
