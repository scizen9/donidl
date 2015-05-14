pro gx_mcat_slice_mags,fspec,ra0,dec0,rad,fmag,fmge,nmag,nmge,time
	flist=file_search(fspec,count=nf)
	if nf le 0 then begin
		print,'GX_MCAT_SLICE_MAGS - No file found: ',fspec
		return
	endif
	;
	fmag = fltarr(nf) - 99.999
	fmge = fltarr(nf) - 9.999
	nmag = fltarr(nf) - 99.999
	nmge = fltarr(nf) - 9.999
	time = dblarr(nf) - 1.
	;
	for i=0L,nf-1 do begin
		gx_mcat_mags,flist[i],ra0,dec0,rad,f,fer,n,ner,tim
		fmag[i] = f
		fmge[i] = fer
		nmag[i] = n
		nmge[i] = ner
		time[i] = tim
	endfor

	return
end
