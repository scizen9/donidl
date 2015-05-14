pro gx_path_obs
	flist=file_search('*-paths.txt',count=nf)
	paths = gx_read_paths(flist[0],npth=npth,pat=pat,vis=vis)
	temp=flist[0]
	std=gettok(temp,'-')
	openw,ol,std+'-obs.txt',/get_lun
	for i=0,npth-1 do begin
		if pat[i] eq '' then begin
			printf,ol,0,paths[i],format='(i02,1x,a)'
		endif else begin
			if strpos(pat[i],'PAT') ge 0 then nsv = 6
			if strpos(pat[i],'GRID_5') ge 0 then nsv = 5
			if strpos(pat[i],'GRID_10') ge 0 then nsv = 10
			if strpos(pat[i],'GRID_12') ge 0 then nsv = 12
			for k=0,nsv-1 do $
				printf,ol,k+1,paths[i],form='(i02,1x,a)'
		endelse
	endfor
	free_lun,ol
	;
	return
end
