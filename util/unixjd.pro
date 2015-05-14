function unixjd,usec
;+
;	unixjd - convert unix seconds to julian date
;-
	mos = 'JanFebMarAprMayJunJulAugSepOctNovDec'
	dstr=systime(0,usec,/utc)
	dy = gettok(dstr,' ')
	mo = gettok(dstr,' ')
	mo = fix(strpos(mos,mo)/3. + 1)
	dy = fix(gettok(dstr,' '))
	hr = fix(gettok(dstr,':'))
	mn = fix(gettok(dstr,':'))
	sc = fix(gettok(dstr,' '))
	yr = fix(dstr)
	return,julday(mo,dy,yr,hr,mn,sc)
end
