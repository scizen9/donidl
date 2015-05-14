function jdfname,jdfile
;
if strpos(jdfile,'EP') ge 0 then begin
    yr=2000+fix(strmid(jdfile,strpos(jdfile,'EP')+2,2))
    mo=fix(strmid(jdfile,strpos(jdfile,'EP')+4,2))
    da=fix(strmid(jdfile,strpos(jdfile,'EP')+6,2))
    jdcnv,yr,mo,da,0,jd
    mjd = jd-2400000.5d0
endif else begin
    str = jdfile
    pmax = strlen(jdfile)
    p = 0
    ndigts = 0
;
; trim off any leading filter designation
    while strpos('0123456789',strmid(str,p,1)) lt 0 do p = p + 1
;
; get number
    jl = 0L
    while strpos('0123456789',strmid(str,p,1)) ge 0 and p lt pmax do begin
	jl = jl * 10L + strpos('0123456789',strmid(str,p,1))
	p = p + 1
	ndigts = ndigts + 1
    endwhile
    mjd=double(jl)/(10.d0^(ndigts-5))
endelse
;
; reduced jd has 5 day digits
return,mjd
end
