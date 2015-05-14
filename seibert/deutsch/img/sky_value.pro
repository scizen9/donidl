	PRO SKY_VALUE,p,bitpix,percent,sky,maxval
;+
; INPUTS:
;	p - your image
;	bitpix - number of bits/pixel
;	percent - percent of borders to ignore in the computation
;
; OUTPUTS:
;	sky - estimate sky value
;	maxval - estimated upper value
;
;
; Modified May 2, 1991 DJL  added percent parameter (which used to be hard
;		coded at 10.0%)
;
; COMPUTE SKY
;-

	AVE=FLTARR(20)
	s = size(p) & ns=s(1) & nl=s(2)
	skips = fix(ns*(percent/100.0))
	skipl = fix(nl*(percent/100.0))
	is1 = skips
	is2 = ns-skips-1
	il1 = skipl
	il2 = nl-skipl-1
	nskip = fix(nl-skipl*2)/20
	FOR I=0,19 DO BEGIN
		DATA = float(p(is1:is2,i*nskip+il1))
		if bitpix eq 16 then begin
			bad = where(data lt -5000)
			if !err gt 0 then data(bad)=data(bad)+65536.
		end
		non_zero = where(data ne 0)
		if !err gt 20 then data=data(non_zero)
		sdata = data(sort(data))
		dmin = sdata(10)			;allow up to 10 bad
							;ccd columns
		minpos = where(data eq dmin) & minpos = minpos(0)
		center = minpos < (N_ELEMENTS(DATA)-4) > 3
		AVE(I) = TOTAL(data(center-3:center+3))/7.0
	END
	AVE=AVE(SORT(AVE))
	sky=AVE(4)
	if n_params(0) lt 4 then return
;
; compute maximum value
;
	max_line = fltarr(nl)		;max for each line
	max_samp = fltarr(ns)		;max for each column
	for i=il1,il2 do begin
		data = float(p(*,i))

		if bitpix eq 16 then begin
			bad = where(data lt -5000)
			if !err gt 0 then data(bad)=data(bad)+65536.
		end
		bad = where(data eq 32767)
		if !err gt 0 then data(bad) = 0
		max_samp = max_samp>data
		max_line(i) = max(data(is1:is2))
	end
	max_line = max_line(il1:il2)
	max_line = max_line(sort(max_line))
	max_samp = max_samp(is1:is2)
	max_samp = max_samp(sort(max_samp))
	is = (is2-is1+1)-20
	il = (il2-il1+1)-20
	maxval = max_samp(is)<max_line(il)
	PRINT,'SKY_VALUE SKY,MAX',SKY,MAXVAL
	RETURN
END
