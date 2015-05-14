function get_good_peg,fcat,count=count,silent=silent
;+
; get_good_peg - return fcat indices to good host fits
;-
readcol,!SNE_DATA+'pegase_bad.list',sne,format='a',/silent
;
t = where(strpos(tag_names(fcat),'SEDFITS') ge 0, nt)
if nt ge 1 then $
	zpeg = fcat.sedfits $
else	zpeg = fcat
for i=0,n_elements(sne)-1 do begin
	p=where(strpos(zpeg.id,sne[i]) ge 0, n)
	if n gt 0 then $
		zpeg[p[0]].chi2 = 10000.
endfor
;
gp=where(zpeg.nsol ge 1 and zpeg.nbands gt 2 and $
	     zpeg.chi2/(zpeg.nbands-1) lt 50., count)
if not keyword_set(silent) then print,'Ngood: ',count
return,gp
end
