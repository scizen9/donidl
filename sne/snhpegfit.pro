pro snhpegfit,snid,site=site,silent=silent,minfilts=minfilts
;+
;	snpegfit - write out a catalog and fit SED with zpeg
;
; INPUTS:
;	snid	- the sn host to fit
;-
common lowz_sne_info
;
; get sn
if n_params(0) lt 1 then begin
	sn=''
	read,'Enter SN id: ',sn
endif else sn = snid
s = snfind(sn)
if s lt 0 then begin
	print,'Not found: ',sn
	return
endif
;
; call snhpegfits
samname=strtrim(sndat[s].id,2)
snhpegfits,s,site=site,silent=silent,samname=samname,minfilts=minfilts
;
return
end
