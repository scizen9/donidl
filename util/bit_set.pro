function bit_set,bits,high_bit=high_bit,silent=silent
;+
;	bit_set	- return number having bits in list set
;
; INPUTS:
;	bits	- list of bits to set, numbered from 0 to MSB
;
; KEYWORDS:
;	high_bit - highest bit, if not set assume highest in BIT_SET
;	silent - set to suppress output
;
; HISTORY:
;	25-apr-2011 - Written, jdn
;-
	d = 0L
	nb = n_elements(bits)
	if keyword_set(high_bit) then $
		hbit = high_bit $
	else	hbit = max(bits)
	bpat = ''
	for i=0,hbit do begin
		t=where(bits eq i, nt)
		if nt gt 0 then begin
			d = d + 2L^i
			bpat = bpat + '1'
		endif else bpat = bpat + '0'
	endfor
	if not keyword_set(silent) then print,d,bpat,form='(i,2x,a)'

	return,d
end

