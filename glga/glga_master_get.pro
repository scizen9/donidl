pro glga_master_get,inlist,indir=indir,qaonly=qaonly,imgonly=imgonly, $
	new=new,update=update
;+
; glga_master_get - create a script to copy glga master data to local glga dirs
;-
;
readcol,inlist,id,ra,dec,mjx,mnx,pa,ty,form='a,d,d,f,f,f,a',comment='#'
ngal = n_elements(ra)
;
frute = 'glga_get_'
if keyword_set(indir) then begin
	idir = indir
	frute= frute + 'indir_'
endif else begin
	idir = !GLGA_MS_ROOT
endelse
if keyword_set(qaonly) then begin
	frute = frute + 'qa_'
endif else if keyword_set(imgonly) then begin
	frute = frute + 'im_'
endif
donew=keyword_set(new)
doupd=keyword_set(update)
if not donew and not doupd then begin
	donew = not donew
	doupd = not doupd
endif
;
if donew then begin
	nfil = frute + 'cp_new.csh'
	filestamp,nfil
	openw,nl,nfil,/get_lun
	printf,nl,'# GLGA_MASTER_GET: NEW DATA - '+systime(0)
	printf,nl,'# SOURCE DIR: '+idir
	printf,nl,'# DESTIN DIR: '+!GLGA_ROOT
	gnfl = frute + 'new.glga'
	filestamp,gnfl
	openw,gnl,gnfl,/get_lun
	printf,gnl,'# GLGA_MASTER_GET: NEW DATA - '+systime(0)
	printf,gnl,'# SOURCE DIR: '+idir
	printf,gnl,'# DESTIN DIR: '+!GLGA_ROOT
endif
;
if doupd then begin
	ufil = frute + 'cp_upd.csh'
	filestamp,ufil
	openw,ul,ufil,/get_lun
	printf,ul,'# GLGA_MASTER_GET: UPDATED DATA - '+systime(0)
	printf,ul,'# SOURCE DIR: '+idir
	printf,ul,'# DESTIN DIR: '+!GLGA_ROOT
	gufl = frute + 'upd.glga'
	filestamp,gufl
	openw,gul,gufl,/get_lun
	printf,gul,'# GLGA_MASTER_GET: UPDATED DATA - '+systime(0)
	printf,gul,'# SOURCE DIR: '+idir
	printf,gul,'# DESTIN DIR: '+!GLGA_ROOT
endif
;
; loop over galaxies
;
for i=0l,ngal-1l do begin
	dd=string(floor(ra[i]),format='(i03)')+'D/'
	host = strtrim(id[i],2)
	;
	; keep track of actions
	gal_new = (1 eq 0)
	gal_upd = (1 eq 0)
	;
	; image data
	mr=idir+'data/'+dd+'uv/fits/'
	sr=!GLGA_ROOT+'data/'+dd+'uv/fits/'
	if keyword_set(qaonly) then $
		fspec = mr+host+'*_qa.txt' $
	else	if keyword_set(imgonly) then $
		fspec = mr+host+'-*.fit*' $
	else	fspec = mr+host+'[_-]*'
	flist=file_search(fspec,count=nf)
	if nf gt 0 then begin
		for j=0,nf-1 do begin
			fdecomp,flist[j],jnk1,jnk2,root,ex
			missing = requires_update(root+'.'+ex,mr,sr,/missing)
			if not missing then begin
			    if strpos(ex,'fit') ge 0 then begin
				hdr0 = headfits(mr+root+'.'+ex)
				hdr1 = headfits(sr+root+'.'+ex)
				nx0 = sxpar(hdr0,'naxis1')
				ny0 = sxpar(hdr0,'naxis2')
				nx1 = sxpar(hdr1,'naxis1')
				ny1 = sxpar(hdr1,'naxis2')
				if nx0 ne nx1 or ny0 ne ny1 then begin
				    if strpos(root,'-nd-int') ge 0 then begin
					print,' '
					print,host,' different img sizes: ', $
						nx0,ny0,nx1,ny1, $
						format='a-25,a,4i7'
				    endif
				    update = (1 eq 1)
				endif else	update = (1 eq 0)
			    endif else update = (1 eq 0)
			    if (requires_update(root+'.'+ex,mr,sr,/update) $
				or update) and doupd then begin
				    printf,ul,'cp -p '+flist[j]+' '+sr
				    gal_upd = (1 eq 1)
			    endif
			endif else if donew then begin
				printf,nl,'cp -p '+flist[j]+' '+sr
				gal_new = (1 eq 1)
			endif
		endfor
	endif
	;
	; dss image data
	mr=idir+'data/'+dd+'dss/fits/'
	sr=!GLGA_ROOT+'data/'+dd+'dss/fits/'
	if keyword_set(qaonly) or keyword_set(imgonly) then $
		fspec = 'XXXX' $
	else	fspec = mr+host+'_*'
	flist=file_search(fspec,count=nf)
	if nf gt 0 then begin
		for j=0,nf-1 do begin
			fdecomp,flist[j],jnk1,jnk2,root,ex
			if requires_update(root+'.'+ex,mr,sr,/update) and $
				doupd then begin
				printf,ul,'cp -p '+flist[j]+' '+sr
				gal_upd = (1 eq 1)
			endif
			if requires_update(root+'.'+ex,mr,sr,/missing) and $
				donew then begin
				printf,nl,'cp -p '+flist[j]+' '+sr
				gal_new = (1 eq 1)
			endif
		endfor
	endif
	;
	; jpgs
	mr=idir+'data/'+dd+'uv/jpg/'
	sr=!GLGA_ROOT+'data/'+dd+'uv/jpg/'
	if keyword_set(qaonly) or keyword_set(imgonly) then $
		fspec = 'XXXX' $
	else	fspec = mr+host+'_*'
	flist=file_search(fspec,count=nf)
	if nf gt 0 then begin
		for j=0,nf-1 do begin
			fdecomp,flist[j],jnk1,jnk2,root,ex
			if requires_update(root+'.'+ex,mr,sr,/update) and $
				doupd then begin
				printf,ul,'cp -p '+flist[j]+' '+sr
				gal_upd = (1 eq 1)
			endif
			if requires_update(root+'.'+ex,mr,sr,/missing) and $
				donew then begin
				printf,nl,'cp -p '+flist[j]+' '+sr
				gal_new = (1 eq 1)
			endif
		endfor
	endif
	;
	; aux data
	mr=idir+'data/'+dd+'aux/'
	sr=!GLGA_ROOT+'data/'+dd+'aux/'
	if keyword_set(imgonly) then $
		fspec = 'XXXX' $
	else	fspec = mr+host+'_*.dat'
	flist=file_search(fspec,count=nf)
	if nf gt 0 then begin
		for j=0,nf-1 do begin
			fdecomp,flist[j],jnk1,jnk2,root,ex
			if requires_update(root+'.'+ex,mr,sr,/update) and $
				doupd then begin
				printf,ul,'cp -p '+flist[j]+' '+sr
				gal_upd = (1 eq 1)
			endif
			if requires_update(root+'.'+ex,mr,sr,/missing) and $
				donew then begin
				printf,nl,'cp -p '+flist[j]+' '+sr
				gal_new = (1 eq 1)
			endif
		endfor
	endif
	;
	; photometry data
	mr=idir+'data/'+dd+'photometry/'
	sr=!GLGA_ROOT+'data/'+dd+'photometry/'
	if keyword_set(qaonly) or keyword_set(imgonly) then $
		fspec = 'XXXX' $
	else	fspec = mr+host+'_*'
	flist=file_search(fspec,count=nf)
	if nf gt 0 then begin
		for j=0,nf-1 do begin
			fdecomp,flist[j],jnk1,jnk2,root,ex
			if requires_update(root+'.'+ex,mr,sr,/update) and $
				doupd then begin
				printf,ul,'cp -p '+flist[j]+' '+sr
				gal_upd = (1 eq 1)
			endif
			if requires_update(root+'.'+ex,mr,sr,/missing) and $
				donew then begin
				printf,nl,'cp -p '+flist[j]+' '+sr
				gal_new = (1 eq 1)
			endif
		endfor
	endif
	;
	; plots
	mr=idir+'data/'+dd+'plots/'
	sr=!GLGA_ROOT+'data/'+dd+'plots/'
	if keyword_set(qaonly) or keyword_set(imgonly) then $
		fspec = 'XXXX' $
	else	fspec = mr+host+'_*'
	flist=file_search(fspec,count=nf)
	if nf gt 0 then begin
		for j=0,nf-1 do begin
			fdecomp,flist[j],jnk1,jnk2,root,ex
			if requires_update(root+'.'+ex,mr,sr,/update) and $
				doupd then begin
				printf,ul,'cp -p '+flist[j]+' '+sr
				gal_upd = (1 eq 1)
			endif
			if requires_update(root+'.'+ex,mr,sr,/missing) and $
				donew then begin
				printf,nl,'cp -p '+flist[j]+' '+sr
				gal_new = (1 eq 1)
			endif
		endfor
	endif
	;
	; check actions
	if gal_upd and doupd then $
		printf,gul,id[i],ra[i],dec[i],mjx[i],mnx[i],pa[i],ty[i], $
			form='(a-25,2f13.8,3f9.2,2x,a)'
	if gal_new and donew then $
		printf,gnl,id[i],ra[i],dec[i],mjx[i],mnx[i],pa[i],ty[i], $
			form='(a-25,2f13.8,3f9.2,2x,a)'
	;
	print,string(13B),i+1,'/',ngal,host,format='($,a1,i5,a,i5,2x,a-16)'
endfor
;
print,' '
;
if donew then free_lun,nl,gnl
if doupd then free_lun,ul,gul
;
return
end
