pro zpeg_to_glga,ifil
;+
; zpeg_to_glga - distribute zpeg batch results to glga data dirs
;
; CALLING SEQUENCE:
;	zpeg_to_glga,Ifil
;
; INPUTS:
;	Ifil	- input zpeg batch results file
;
; OUTPUTS:
;	None.
;
; SIDE EFFECTS:
;	Writes a zpeg results file for each object to the corresponding
;	glga data directory.
;
; HISTORY:
; $Id: zpeg_to_glga.pro,v 1.4 2013/08/02 22:23:29 neill Exp $
;	20-JUL-2013	Initial Revision	Don Neill
;-
common glgadb_info
common galdb_info
;
pre = 'ZPEG_TO_GLGA'
;
; log file
openw,ll,ifil+'_2glga.log',/get_lun
printf,ll,'# '+pre+': '+systime(0)
;
; counters
nread = 0L
nwrite = 0L
;
; test inputs
if file_test(ifil,/read) then begin
	;
	; open file
	openr,il,ifil,/get_lun
	;
	; input record
	rec = ''
	;
	; read header
	hdr = strarr(50)
	ihdr = 0
	readf,il,rec
	while strpos(rec,'#') eq 0 do begin
		hdr[ihdr] = rec
		ihdr += 1
		readf,il,rec
	endwhile
	;
	; start writing galaxy files
	nread += 1L
	tmp = rec
	gal = gettok(tmp,' ')
	g = gfind(gal)
	if g ge 0 then begin
		ra = galdat[g].ra
	endif else begin
		g = glfind(gal)
		if g ge 0 then $
			ra = glgadat[g].ra $
		else	ra = -9.
	endelse
	if g ge 0 then begin
		glga_dir = !GLGA_ROOT+'data/'+glga_degdir(ra)+'/seds/zpeg/'
		openw,ol,glga_dir+gal,/get_lun
		for i=0,ihdr-1 do printf,ol,hdr[i]
		printf,ol,rec
		free_lun,ol
		nwrite += 1L
		print,'Wrote: ',glga_dir+gal
	endif else printf,ll,'Missing: ',gal
	;
	; loop over the rest
	while not eof(il) do begin
		readf,il,rec
		nread += 1L
		tmp = rec
		gal = gettok(tmp,' ')
		g = gfind(gal)
		if g ge 0 then begin
			ra = galdat[g].ra
		endif else begin
			g = glfind(gal)
			if g ge 0 then $
				ra = glgadat[g].ra $
			else	ra = -9.
		endelse
		if g ge 0 then begin
			glga_dir = !GLGA_ROOT+'data/'+glga_degdir(ra)+ $
				'/seds/zpeg/'
			openw,ol,glga_dir+gal,/get_lun
			for i=0,ihdr-1 do printf,ol,hdr[i]
			printf,ol,rec
			free_lun,ol
			nwrite += 1L
			print,'Wrote: ',glga_dir+gal
		endif else printf,ll,'Missing: ',gal
	endwhile
	free_lun,il
endif else print,pre+': Error - file not found: ',ifil
;
print,'Records read: ',nread
print,'Files written: ',nwrite
printf,ll,'Read, Written: ',nread,nwrite
free_lun,ll

return
end
