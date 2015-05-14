pro makellipsepars,verbose=verbose,master=master,local=local
;+
; makellipsepars - create ellipsepars.dat
;-
pdir = !GLGA_ROOT
if keyword_set(master) then begin
	pdir = !GLGA_MS_ROOT
	fil = 'master_ellipsepars.dat'
endif else begin
	pdir = !GLGA_ROOT
	fil = 'ellipsepars.dat'
endelse
;
if keyword_set(local) then $
	ofil = fil $
else	ofil = !GALS_DATA + 'ellipsepars.dat'
;
;
;
flist=file_search(pdir+'data/???D/photometry/*_ellipsepar.dat',count=nf)
if nf le 0 then begin
	print,'No *_ellipsepar.dat files found, returning'
	return
endif else if keyword_set(verbose) then print,'Found this many: ',nf
;
filestamp,ofil,/arch
openw,ol,ofil,/get_lun
printf,ol,'# MAKELLIPSEPARS - '+systime(0)
printf,ol,'#name                           ra_cen      dec_cen   SemiMajAx   SemiMinAx       PA    FILE'
;
last = 'junk'
first = (1 eq 1)
max_mt = 0L
for i=0l,nf-1 do begin
	; check file time
	fi = file_info(flist[i])
	mt = fi.mtime
	; parse host name
	p0=strpos(flist[i],'/',/reverse_search)+1
	nc=strpos(flist[i],'_')-p0
  	host=strmid(flist[i],p0,nc)
	p2=strpos(flist[i],'data')+4
	ddir=strmid(flist[i],p2,6)
	; new host?
	new = (strcmp(last,host) eq 0)
	if new then begin
		; first time through skip output
		if first then begin
			first = (1 eq 0)
		; else write out the most recent data for last host
		endif else begin
			readcol,ifile,ra,dec,smaj,smin,pa,form='d,d,f,f,f', $
				/silent
			printf,ol,last,ra,dec,smaj,smin,pa,ifile, $
				format='(a-25,2f13.8,2f12.3,f9.3,4x,a)'
		endelse
		; update to new host
		last = host
		; reset max time, most recent file for new host
		max_mt = mt
		ifile = flist[i]
	; not new so check file time
	endif else begin
		if mt gt max_mt then begin
			ifile = flist[i]
			max_mt = mt
		endif
	endelse

        if keyword_set(verbose) then $
		print,string(13B),i+1,'/',nf,host,ddir, $
			format='($,a1,i5,a,i5,2x,a-26,2x,a)'

endfor
; write out last host
readcol,ifile,ra,dec,smaj,smin,pa,form='d,d,f,f,f',/silent
printf,ol,last,ra,dec,smaj,smin,pa,ifile,format='(a-25,2f13.8,2f12.3,f9.3,4x,a)'
;
free_lun,ol
;
if keyword_set(verbose) then print,' '
;
return
end
