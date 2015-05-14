pro get_dao_refs, pref, rids
;
openr,ilun,pref+'.lst',/get_lun
rddaohdr,ilun
;
rec = ''
rx = fltarr(100)
ry = fltarr(100)
nref = 0
;
while not eof(ilun) do begin
	readf,ilun,rec
	junk = gettok(rec,' ')
	x = float(gettok(rec,' '))
	y = float(gettok(rec,' '))
	rx(nref) = x
	ry(nref) = y
	nref = nref + 1
endwhile
rx = rx(0:nref-1)
ry = ry(0:nref-1)
;
free_lun,ilun
;
openr,ilun,pref+'.alf',/get_lun
rddaohdr,ilun
;
rids = intarr(100)
nrids = 0
;
while not eof(ilun) do begin
	readf,ilun,rec
	rid = fix(gettok(rec,' '))
	x = float(gettok(rec,' '))
	y = float(gettok(rec,' '))
	;
	rs = sqrt( (rx - x)^2 + (ry - y)^2 )
	t = where(rs lt 1.0, nfound)
	if nfound eq 1 then begin
		rids(nrids) = rid
		nrids = nrids + 1
	endif
endwhile
rids = rids(0:nrids-1)
;
free_lun,ilun
;
return
end
