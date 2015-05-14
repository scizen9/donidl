pro glga_master_put
;+
; glga_master_put - create a script to copy local glga data into glga dirs
;-
common galdb_info
;
frute = 'glga_put_hld_'
;
odir = !GLGA_MS_ROOT
;
nfil = frute + 'cp_new.csh'
filestamp,nfil
openw,nl,nfil,/get_lun
printf,nl,'# GLGA_MASTER_PUT: NEW HLEDA DATA - '+systime(0)
printf,nl,'# SOURCE DIR: '+!GLGA_ROOT
printf,nl,'# DESTIN DIR: '+odir
;
ufil = frute + 'cp_upd.csh'
filestamp,ufil
openw,ul,ufil,/get_lun
printf,ul,'# GLGA_MASTER_PUT: UPDATED HLEDA DATA - '+systime(0)
printf,ul,'# SOURCE DIR: '+!GLGA_ROOT
printf,ul,'# DESTIN DIR: '+odir
;
; loop over galdat
ngal=n_elements(galdat)
;
for i=0,ngal-1 do begin
    if galdat[i].hlind ge 0 then begin
;
; get dir and host
	if galdat[i].g_ra ge 0. then $
		ra = galdat[i].g_ra $
	else	ra = galdat[i].hra
	dd=string(floor(ra),format='(i03)')+'D/'
	host = strtrim(galdat[i].hlname,2)
	;
	; qa data (we don't put image data)
	mr=odir+'data/'+dd+'uv/fits/'
	sr=!GLGA_ROOT+'data/'+dd+'uv/fits/'
	fspec = sr+host+'*_qa.txt'
	flist=file_search(fspec,count=nf)
	if nf gt 0 then begin
		for j=0,nf-1 do begin
		    if strpos(flist[j],'_sdss_') lt 0 and $
		       strpos(flist[j],'_2mass_') lt 0 then begin
			fdecomp,flist[j],jnk1,jnk2,root,ex
			if requires_update(root+'.'+ex,sr,mr,/update) then begin
;			if file_exist(mr+root+'.'+ex) then begin
				printf,ul,'rm '+mr+root+'.'+ex
				printf,ul,'cp -p '+flist[j]+' '+mr
			endif
			if requires_update(root+'.'+ex,sr,mr,/missing) then $
				printf,nl,'cp -p '+flist[j]+' '+mr
		   endif
		endfor
	endif
	;
	; aux data
	mr=odir+'data/'+dd+'aux/'
	sr=!GLGA_ROOT+'data/'+dd+'aux/'
	flist=file_search(sr+host+'_*.dat',count=nf)
	if nf gt 0 then begin
		for j=0,nf-1 do begin
		    if strpos(flist[j],'_sdss_') lt 0 and $
		       strpos(flist[j],'_2mass_') lt 0 then begin
			fdecomp,flist[j],jnk1,jnk2,root,ex
			if requires_update(root+'.'+ex,sr,mr,/update) then begin
;			if file_exist(mr+root+'.'+ex) then begin
				printf,ul,'rm '+mr+root+'.'+ex
				printf,ul,'cp -p '+flist[j]+' '+mr
			endif
			if requires_update(root+'.'+ex,sr,mr,/missing) then $
				printf,nl,'cp -p '+flist[j]+' '+mr
		   endif
		endfor
	endif
	;
	print,string(13B),i+1,'/',ngal,host,format='($,a1,i5,a,i5,2x,a-16)'
    endif	; hlind ge 0
endfor
;
print,' '
;
free_lun,nl,ul
;
return
end
