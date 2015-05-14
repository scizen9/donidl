pro pltgalseds,lfil,ps=ps,png=png,pdf=pdf,glgaout=glgaout, $
	yuan13=yuan13,indices=indices,test=test,update=update,verbose=verbose
;+
;	pltgalseds - plot seds from galdat structure
;
; CALLING SEQUENCE:
;	pltgalseds,lfil,/ps,/png,/pdf,/glgaout,indices=indices,test=<int>
;
; INPUTS:
;	lfil	- list of galaxy ids, one per line
;
; KEYWORDS:
;	ps	- create postscript files
;	png	- convert postscript files to png (implies /ps)
;	pdf	- convert postscript files to pdf (implies /ps)
;	glgaout	- put png, pdf files in !GLGA_ROOT dir (implies /ps,/png,/pdf)
;	yuan13	- use extinction coefficients from Yuan et al. 2013
;	indices	- galdat indices of galaxies to plot
;	test	- integer minimum number of valid bandpasses required to plot
;	update	- only plot if GLGA photometry is newer than GLGA plot
;	verbose	- extra output to screen
;
; HISTORY:
;	29-apr-2011, jdn - Initial Version
;	19-may-2011, jdn - Added indices keyword
;	20-jul-2011, jdn - Now can test if any valid fluxes to plot
;	01-aug-2011, jdn - Rationalized keywords, outputs, removed copy script
;-
	common galdb_info
	temp = lfil
	rute = gettok(temp,'.')
	if keyword_set(indices) then $
		id = strtrim(galdat[indices].id,2) $
	else	if file_test(lfil) then $
		readcol,lfil,id,form='a',comment='#'

	ngal = n_elements(id)

	if ngal gt 0 then begin
		for i=0L,ngal-1L do begin
			g = (gfind(id[i],count=ng))[0]
			if ng eq 1 then begin
				if keyword_set(verbose) then $
					print,i+1,'/',ngal,galdat[g].id, $
						format='(i7,a,i7,2x,a-32)'
				pltgalsed,id[i],ps=ps,png=png,pdf=pdf, $
					glgaout=glgaout,yuan13=yuan13, $
					test=test,update=update,verbose=verbose
			endif	else print,'Not found: ',id[i]
		endfor
	endif	else print,'PLTGALSEDS: Error - No ids found'

	return
end

