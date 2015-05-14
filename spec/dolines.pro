pro dolines,fspec,zw,llist
;
if n_params(0) lt 2 then begin
	print,'Usage: dolines, fspec, zoom_width, line_list'
	return
endif
;
; check line list
if not file_test(llist) then begin
	print,'***Line list file not found: ',llist
	return
endif
;
flist=file_search(fspec,count=nf)
;
if nf gt 0 then begin
	print,nf,' files found'
	for i=0,nf-1 do begin
		print,flist(i),': ',i+1,'/',nf
		linemeas,flist(i),zw,llist
		wait,1
	endfor
endif else print,'No files found: ',fspec
;
return
end
