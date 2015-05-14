pro mkglgalist,ifile,indices=indices
;+
;	make a list in GLGA format
;-
;
; check for indices
if keyword_set(indices) then begin
	ngal = n_elements(indices)
	if ngal le 0 then begin
		print,'Empty indices list'
		return
	endif
endif else begin
;
; read in file
	readcol,ifile,id,ra,dec,form='a,d,d'
	id=strtrim(id,2)
;
; galaxy count
	ngal=n_elements(ra)
endelse
;
; output file
tmp=ifile
rute=gettok(tmp,'.')
ofile=rute+'.glga'
filestamp,ofile,/arch
openw,ol,ofile,/get_lun
printf,ol,'# MKGLGALIST - '+systime(0)
printf,ol,'# ID                        RAdeg      DECdeg        MAJamin  MINamin  PAdeg   Type'
;
; Hyper LEDA
restore,!CAT_DATA+'hl_master_structs.sav'
;
; find hyper leda, if it exists
rlim = 0.1
for i=0,ngal-1 do begin
	;
	; get hldata
	if keyword_set(indices) then $
		w=indices[i] $
	else	w=hlfind(ra[i],dec[i],hldata=hldata,name=id[i],diam=rlim)
	;
	; coord match
	if w ge 0 then begin
		majax = 10.^hldata[w].logd25 * .1 	; arcmin
		axrat = 10.^hldata[w].logr25
		minax = majax / axrat
		pa    = hldata[w].pa
		if finite(pa) eq 0 then pa = 0.
		type  = hldata[w].type
		name  = hldata[w].objname
		hra   = hldata[w].al2000*15.d0
		hdec  = hldata[w].de2000
	;
	; not found
	endif else begin
		majax = 0.5
		minax = 0.5
		pa    = 0.
		type  = '-'
		if keyword_set(indices) then begin
			name = strn(indices[i])+'_notfound'
			hra  = -99.99
			hdec = -99.99
		endif else begin
			name  = id[i]
			hra   = ra[i]
			hdec  = dec[i]
		endelse
	endelse
	;
	; print
	printf,ol,name,hra,hdec,majax,minax,pa,type, $
		format='(a-25,2f13.8,3f9.3,2x,a)'
	;
	; status
	print,string(13B),i+1,'/',ngal,name,format='($,a1,i5,a,i5,2x,a-25)'
endfor
free_lun,ol
print,' '
;
return
end
