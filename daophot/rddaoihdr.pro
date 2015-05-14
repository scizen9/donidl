pro rddaoihdr,file,h,nrec
;+
;	read daophot headers (IRAF format)
;-
;
openr,lun,file,/get_lun
nrec=0
rec='#'
rec2=''
rec3=''
col=0
while strmid(rec,0,1) eq '#' do begin
	if strlen(rec) gt 1 then begin
		type = strmid(gettok(rec,' '),1,1)
		case type of

		'K':	begin
			key = gettok(rec,' ')
			junk= gettok(rec,' ')
			sval = gettok(rec,' ')
			if valid_num(sval,ival,/integer) then $
				val = ival $
			else if valid_num(sval,fval) then $
				val = fval $
			else val = sval
			cmnt= gettok(rec,' ')
			sxaddpar,h,key,val,cmnt
			end

		'N':	begin
			readf,lun,rec2 & junk = gettok(rec2,' ') & nrec = nrec+1
			readf,lun,rec3 & junk = gettok(rec3,' ') & nrec = nrec+1
			nc = 0
			val = gettok(rec,' ')
			cmnt = gettok(rec2,' ')
			fmt = gettok(rec3,' ')
			while strtrim(val,2) ne '\' do begin
				key = 'COL_'+strtrim(col,2)
				key2= 'FMT_'+strtrim(col,2)
				sxaddpar,h,key,val,cmnt
				sxaddpar,h,key2,fmt,cmnt
				nc = nc + 1
				col = col + 1
				val = gettok(rec,' ')
				cmnt = gettok(rec2,' ')
				fmt = gettok(rec3,' ')
			endwhile
			end

		'U':	begin
			end

		'F':	begin
			end

		endcase
		sxaddpar,h,'NCOL',col,'number of columns'
	endif
	readf,lun,rec & nrec = nrec+1
endwhile
nrec = nrec - 1
;
free_lun,lun
return
end
