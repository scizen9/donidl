;-------------------------------------------------------------
;+
; NAME:
;       FIXANG
; PURPOSE:
;       Fix angle discontinuety at 0-360.
; CATEGORY:
; CALLING SEQUENCE:
;       b = fixang(a)
; INPUTS:
;       a = input array of angles.                         in
; KEYWORD PARAMETERS:
; OUTPUTS:
;       b = array of angles with discontinueties removed.  out
; COMMON BLOCKS:
; NOTES:
;       Notes: Assumes no valid delta angle GT 180 deg.
;         Looks for large jumps in the angle.  Offset each
;         section to match one before.  Angles may not be in
;         the range 0 to 360 when done.
; MODIFICATION HISTORY:
;       Ray Sterner  13 Aug, 1985.
;       R. Sterner, 14 May, 1993 --- Cleaned up a little bit.
;       Johns Hopkins University Applied Physics Laboratory.
;
; Copyright (C) 1985, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
	function fixang,a, help=hlp
 
	if (n_params(0) lt 1) or keyword_set(hlp) then begin
	  print,' Fix angle discontinuety at 0-360.'
	  print,' b = fixang(a)'
	  print,'   a = input array of angles.                         in'
	  print,'   b = array of angles with discontinueties removed.  out'
	  print,' Notes: Assumes no valid delta angle GT 180 deg.'
	  print,'   Looks for large jumps in the angle.  Offset each'
	  print,'   section to match one before.  Angles may not be in'
	  print,'   the range 0 to 360 when done.'
	  return, -1
	endif
 
	x = [0.,a(1:*)-a]
	w = where(abs(x) gt 180, c)
	if c eq 0 then return, a
 
	b = a
	n = n_elements(w)
 
	for i = 0, n-1 do begin
	  in = w(i)
	  sn = x(in)
	  if sn lt 0. then b(in) = b(in:*) + 360.
	  if sn gt 0. then b(in) = b(in:*) - 360.
	endfor
 
	return, b
 
	end
