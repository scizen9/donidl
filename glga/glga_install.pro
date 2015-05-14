pro glga_install
;+
; glga_install - make a script that will install data into GLGA system
;
; $Id: glga_install.pro,v 1.1 2014/01/27 19:38:10 neill Exp $
;-
; GLGA info
common glgadb_info
;
; start
pre = 'GLGA_INSTALL'
version = repstr('$Revision: 1.1 $ $Date: 2014/01/27 19:38:10 $','$','')
;
; get file list
flist = file_search('*.*',count=nf)
if nf le 0 then begin
	print,'No files found'
	return
endif
;
; get output
ofil = 'glga_install_ow_cp.csh'
openw,ol,ofil,/get_lun
printf,ol,'# '+pre+'  '+version
printf,ol,'# Run on '+systime(0)
printf,ol,'# FILE STATUS: Exist'
nfil = 'glga_install_new_cp.csh'
openw,nl,nfil,/get_lun
printf,nl,'# '+pre+'  '+version
printf,nl,'# Run on '+systime(0)
printf,nl,'# FILE STATUS: New'
;
; loop over files
for k = 0,nf-1 do begin
	;
	; get gal id and file type tail
	p = strpos(flist[k],'_')
	id = strmid(flist[k],0,p)
	tail = strmid(flist[k],p)
	g = glfind(id,count=ng)
	;
	; did we find it?
	if ng eq 1 then begin
		g = g[0]
		;
		; base data degree directory
		bdir = !GLGA_ROOT + 'data/' + glga_degdir(glgadat[g].ra) + '/'
		;
		; data type dir
		ddir = ''
		;
		; what type of data?
		if stregex(tail,'_.UV') ge 0 then $
			ddir = 'galex/'
		if stregex(tail,'_[ugriz]') ge 0 then $
			ddir = 'sdss/'
		if stregex(tail,'_[jkh]') ge 0 then $
			ddir = '2mass/'
		if stregex(tail,'_w.') ge 0 then $
			ddir = 'wise/'
		if stregex(tail,'_dss2_red') ge 0 then $
			ddir = 'dss/'
		;
		; where should it go?
		if strpos(tail,'.dat') ge 0 then $
			pdir = 'photometry/'
		if strpos(tail,'.fits.gz') ge 0 then $
			pdir = ddir+'fits/'
		if strpos(tail,'.jpg') ge 0 then $
			pdir = ddir+'jpg/'
		;
		; aux files
		if strpos(tail,'pointsrc.dat') ge 0 or $
		   strpos(tail,'roi.dat') ge 0 or $
		   strpos(tail,'ellipse.dat') ge 0 or $
		   strpos(tail,'mask.fits.gz') then pdir = 'aux/'
		;
		; plot files
		if strpos(tail,'profile') ge 0 or $
		   strpos(tail,'images') ge 0 then pdir = 'plots/'
		;
		; does it already exist?
		if file_test(bdir+pdir+flist[k]) then begin
			spawn,'diff '+flist[k]+' '+bdir+pdir+flist[k],res
			if strlen(res[0]) eq 0 then begin
				print,'Identical file: ',flist[k]
				printf,ol,'# Identical file: ',flist[k]
			endif else begin
				printf,ol,'cp '+flist[k]+' '+bdir+pdir
			endelse
		;
		; new file
		endif	else	printf,nl,'cp '+flist[k]+' '+bdir+pdir
	endif else if ng gt 1 then begin
		print,'Ambiguous id: ',id
	endif else $
		print,'Id not found: ',id
endfor
free_lun,ol
return
end
