pro plot_lseds,pfile,ps=ps,gif=gif,glgaout=glgaout,verbose=verbose
;+
;	plot_lseds - plot all LePhare *.spec files
;
; CALLING SEQUENCE:
;	plot_lseds,pfile
;
; INPUTS:
;	pfile	- LePhare *.para file used to generate *.spec files
;
; KEYWORDS:
;	ps	- create postscript files
;	gif	- convert postscript files to gif (implies /ps)
;	glgaout	- put gif files in appropriate !GLGA_ROOT dir (implies /ps,/gif)
;	verbose	- extra output to screen
;
; HISTORY:
;	30-jul-2011, jdn - Initial Version
;	31-jul-2011, jdn - Refined outputs
;	01-aug-2011, jdn - added glgaout keyword
;-
	if file_test(pfile) then begin
		flist = file_search('*.spec',count=nf)
		for i=0L,nf-1L do begin
			if keyword_set(verbose) then $
				print,i+1,' / ',nf,':  ',flist[i]
			plot_lspec,flist[i],pfile,ps=ps,gif=gif, $
				glgaout=glgaout,verbose=verbose
		endfor
	endif else print,'PLOT_LSEDS: Error - file not found: ',pfile
;
return
end
