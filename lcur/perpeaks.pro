pro perpeaks,pow,npks,pkis
;+
;	get periodogram peaks
;-
npts = n_elements(pow)
pw = pow
pkis = lonarr(npks)
q=''
for i=0,npks-1 do begin
	s = reverse(sort(pw))
	pkis(i) = s(0)
	pki = s(0)
	pkpow = pw(pki)
	if pki lt npts-1 then $
		while pki lt npts-1 and pw(pki) gt pw(pki+1) do begin
			pw(pki) = 0
			pki = pki + 1 < (npts-2)
		endwhile
	pki = s(0) - 1 > 1
	while pw(pki) gt pw(pki-1) and pki gt 1 do begin
		pw(pki) = 0
		pki = pki - 1
	endwhile
endfor

return
end
