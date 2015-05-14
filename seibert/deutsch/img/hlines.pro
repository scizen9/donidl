pro hlines,nlines
;+
; NAME:
;   HLINES
; PURPOSE:
;   Print out some optical Hydrogren lines.
; CALLING SEQEUNCE:
;   hlines,[nlines]
; INPUT:
;   NLINES  Number of lines desired in each series
;-

  if (n_elements(nlines) eq 0) then nlines=6

  for n2=1d,3d do begin
    for n1=n2+nlines+1d,n2+1d,-1d do begin
      if (n1 eq n2+nlines+1) then n1a=100 else n1a=n1
;      E=13.63825d*(1d/n2^2-1d/n1a^2)
;      print,strn(fix(n1a)),' --> ',strn(fix(n2)), $
;	':  ',6.63d-27*3d10/(E*1.6d-12)*1d8
;      E=13.605698140d*(1d/n2^2-1d/n1a^2)
      E=13.602239d*(1d/n2^2-1d/n1a^2)
      print,strn(fix(n1a)),' --> ',strn(fix(n2)), $
	':  ',6.626075540d-27*2.99792458d10/(E*1.6021773349d-12)*1d8
      endfor
    endfor

  return

end
