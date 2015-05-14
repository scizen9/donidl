function date_parse,dtstr
;+
; date_parse - parse a date string of form YYYY-MM-DD HH:MM into MJD
;-
tmp=dtstr
yr=long(gettok(tmp,'-'))
mo=long(gettok(tmp,'-'))
dy=long(gettok(tmp,' '))
hr=long(gettok(tmp,':'))
mn=long(gettok(tmp,' '))
sc=0.0
;
juldate,[yr,mo,dy,hr,mn,sc],jd
return,jd
end
