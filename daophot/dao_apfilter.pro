pro dao_apfilter
;
ifile = ''
read,'input ap file: ',ifile
root = gettok(ifile,'.')
;
; get x,y limits
xl = 0.
xu = 0.
yl = 0.
yu = 0.
read,'input x limits(lower,upper): ',xl,xu
read,'input y limits(lower,upper): ',yl,yu
;
if xl ge xu or xl lt 0 or xu lt 0 or $
   yl ge yu or yl lt 0 or yu lt 0 then begin
   	print,'Nonsensical limits, returning'
	return
endif
;
openr,ilun,root+'.ap',/get_lun
openw,olun,root+'.apf',/get_lun
;
irec = ''
orec = ''
;
; transfer header
for i=0,3 do begin
	readf,ilun,irec
	printf,olun,irec
endfor
;
while not eof(ilun) do begin

	do_write = (1 eq 1)

	readf,ilun,irec
	orec = irec
	tok1 = gettok(irec,' ')
;
; check for id (integer number)
	while strpos(tok1,'.') ge 0 do begin
		print,'Error: malformed record'
		print,orec
		while strtrim(irec,2) ne '' and not eof(ilun) do $
			readf,ilun,irec	; read until next blank line
		if not eof(ilun) then begin
			readf,ilun,irec
			orec = irec
			tok1 = gettok(irec,' ')
		endif
	endwhile
;
; get x,y values
	if not eof(ilun) then begin
		x = float(gettok(irec,' '))
		y = float(gettok(irec,' '))
;
; compare to limits
		if x lt xl or x gt xu or $
		   y lt yl or y gt yu then do_write = (1 eq 0)

		if do_write then printf,olun,orec
;
; read rest of record
		while strtrim(irec,2) ne '' and not eof(ilun) do begin
			readf,ilun,irec
			orec = irec
			if do_write then printf,olun,orec
		endwhile
	endif

endwhile	; while not eof(ilun)

free_lun,ilun,olun

return
end	; pro rddaoap

