pro rdmag,magf,data,h
;
rddaoihdr,magf,h,nhrec
;
data = fltarr(7,20000)
rec = ''
nrec = 0
openr,ilun,magf,/get_lun
for i=0,nhrec-1 do readf,ilun,rec
;
;
while not eof(ilun) do begin
    problem = (1 eq 0)
    for i=0,4 do begin	; loop over records for one star
	readf,ilun,rec
	case i of

	0:	begin
		if nrec eq 0 then begin
			im = gettok(rec,' ')+'.fits'
			sxaddpar,h,'IMNAME',im,'original image filename'
		endif else junk = gettok(rec,' ')
		for j=0,1 do junk=gettok(rec,' ')
		id = float(gettok(rec,' '))
		end

	1:	begin
		x = float(gettok(rec,' '))
		y = float(gettok(rec,' '))
		for j=0,5 do err = gettok(rec,' ')
		problem = (err ne 'NoError')
		end

	2:	begin
		sky = float(gettok(rec,' '))
		skysig = float(gettok(rec,' '))
		for j=0,4 do err = gettok(rec,' ')
		problem = (problem or (err ne 'NoError'))
		end

	3:	begin
		if nrec eq 0 then begin
			exptime = float(gettok(rec,' '))
			temp = gettok(rec,' ')
			if strpos(temp,'INDEF') lt 0 then $
				airmass = float(temp) $
			else	airmass = 0.
			filter  = gettok(rec,' ')
			sxaddpar,h,'EXPOSURE',exptime,'seconds'
			sxaddpar,h,'AIRMASS',airmass,'airmass'
			sxaddpar,h,'FILTER',filter,'filter'
		endif
		end

	4:	begin
		for j=0,3 do junk=gettok(rec,' ')
		smag = strtrim(gettok(rec,' '),2)
		if valid_num(smag) then $
			mag = float(smag) $
		else	mag = -99.99
		smerr= gettok(rec,' ')
		if not problem and valid_num(smerr) then $
			merr = float(smerr) $
		else	merr = -99.99
		end

	else: print,'RDMAG: Error'
	endcase
    endfor	; end looping over records for one star
    data(0,nrec) = id
    data(1,nrec) = x
    data(2,nrec) = y
    data(3,nrec) = sky
    data(4,nrec) = skysig
    data(5,nrec) = mag
    data(6,nrec) = merr
    nrec = nrec + 1
endwhile
;
data = data(*,0:(nrec-1))
;
free_lun,ilun
;
return
end
