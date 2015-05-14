function glga_inventory,gfile
;+
; glga_inventory - get list and status of stuff in !GLGA_ROOT
;-
; get list of files
if n_params(0) eq 1 then begin
	readcol,gfile,id,ra,dec,form='a,d,d'
	tmp = gfile
	rute = gettok(tmp,'.')
	ofil = rute+'.stat'
endif else begin
	ofil = 'glga_inventory.stat'
	flist=file_search(!GLGA_ROOT+'data/???D/{2mass,irac,sdss,galex,wise}/fits/*.fit*', $
		count=nf)
	id = extract_ids(flist)
endelse
;
; get status
gs = glga_status_get(id)
glga_status_print,gs,outfile=ofil
;
return,gs
end
