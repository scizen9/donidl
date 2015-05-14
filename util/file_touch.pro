pro file_touch, fnam, unix_time=unix_time, cal_time=cal_time, $
	access=access, modify
;+
; file_touch - issue a unix 'touch' command 
tstr = systime(0,dbrec.mtime)
sta  = strsplit(tstr,' ',/extract)
yr   = sta[4]
mos  = ['','Jan','Feb','Mar','Apr','May','Jun', $
	   'Jul','Aug','Sep','Oct','Nov','Dec']
mi   = where(strcmp(mos,sta[1]) eq 1)
day  = fix(sta[2])
tstr = sta[3]
hr   = gettok(tstr,':')
mn   = gettok(tstr,':')
sec  = tstr
cmd  = 'touch ' + ofil + ' -m -t
;
return
end
