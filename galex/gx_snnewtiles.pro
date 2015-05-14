pro gx_snnewtiles
;
;
dlist=[ $
	'IIL', $
	'IIP', $
	'II', $
	'IIb', $
	'IIn', $
	'I', $
	'Ia', $
	'Ib_c', $
	'Ib', $
	'Ic' $
]
maxrec=20000L
nf=n_elements(dlist)
for i=0,nf-1 do begin
	;
	; cd into each type directory
	cd,dlist(i)
	;
	; remove old new list
	spawn,'rm *_gximlist.txt_new',out,err
	;
	; get list of files
	flist=file_search(dlist(i)+'_gximlist.txt*',count=nf)
	;
	; should be one there!
	if nf le 0 then $
		print,'No *_gximlist.txt files found' $
	;
	; if only one they must all be new
	else	if nf eq 1 then $
	spawn,'awk ''{if (NF > 0) print $0}'' '+flist(0)+' > '+flist(0)+'_new' $
	;
	; compare old and new
	else begin
		newsn = 0L
		newtiles = 0L
		; open output file
		openw,ol,flist(0)+'_new',/get_lun
		;
		; read next most recent file
		openr,l1,flist(nf-1),/get_lun
		rec = ''
		old=strarr(maxrec)
		p=0L
		while not eof(l1) do begin
			readf,l1,rec
			old(p) = rec
			p = p + 1L
		endwhile
		free_lun,l1
		;
		; read in the new file one line at a time
		openr,l0,flist(0),/get_lun
		nnew = 0L
		newrec = strarr(maxrec)
		snrec = ''
		while not eof(l0) do begin
			readf,l0,rec
			;
			; are we on a new SN?
			if strmid(rec,0,2) eq 'SN' then begin
				;
				; print out the previous new records (if any)
				if nnew gt 0 and strlen(snrec) gt 0 then begin
					printf,ol,snrec
					for j=0,nnew-1 do printf,ol,newrec(j)
					newsn = newsn + 1
					newtiles = newtiles + nnew
				endif
				;
				; reset for new SN
				snrec = rec
				nnew = 0L
				newrec = strarr(maxrec)
			;
			; still reading tile records
			endif else begin
				newrec(nnew) = rec	; store just in case
				;
				; get tile name
				for j=0,2 do tile=gettok(rec,' ') ; 3rd item
				;
				; look for tile in old list
				list=strpos(old,tile)
				t=where(list ge 0, nt)
				;
				; if it wasn't found we've got a new one
				if nt le 0 then nnew = nnew + 1
			endelse
		endwhile
		;
		; close up the files
		free_lun,ol,l0
		;
		; print report
		print,dlist(i),': found  ',newtiles,' new tiles for ', $
			newsn,' new SNe', format='(a4,a,i7,a,i7,a)'
	endelse
	cd,'..'
endfor
;
return
end
