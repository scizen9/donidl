pro cleancoo
;
ifile=''
read,'input file : ',ifile
ofile=''
read,'output file: ',ofile
;
openr,ilun,ifile,/get_lun
openw,olun,ofile,/get_lun
;
; copy header
rec = ''
for i=0,2 do begin
	readf,ilun,rec
	printf,olun,rec
endfor
;
; loop until eof
while not eof(ilun) do begin
	readf,ilun,rec
	orec = rec
;
; break out x,y and sharp
	tok 	= gettok(rec,' ')
	xstr 	= gettok(rec,' ')
	ystr 	= gettok(rec,' ')
	tok 	= gettok(rec,' ')
	sharp 	= gettok(rec,' ')
;
; test against sharp = 0.000 and x and y having .000 values
	if ( strpos(sharp,'000') lt 0 ) then $
	if ( strpos(xstr, '000') lt 0 or $
	     strpos(ystr, '000') lt 0 ) then $
	     	printf,olun,orec
endwhile
;
free_lun,ilun,olun
;
return
end	; clean
