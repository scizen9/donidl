pro pslandfix,filename
;+
; NAME:
;   PSLANDFIX
;
; PURPOSE:
;   Fix the upside-down landscape that IDL generates.  Probably only works
;   with the output of setps,/land
;
; CALLING SEQEUNCE:
;   pro pslandfix,[filename]
;
; INPUT:
;   FILENAME   Name the idl PostScript file to fix.  Default is 'idl.ps'
;
; OUTPUT:
;   Fixed PostScript file
; NOTES:
;   none
; HISTORY:
;   10-AUG-95 Version 1 written    E. Deutsch
;-

  if (n_elements(filename) eq 0) then filename='idl.ps'

  openr,1,filename
  lin=''
  replinz=strarr(100)
  linno=lonarr(100)
  ctr=0 & linctr=0L

  while not EOF(1) do begin
    readf,1,lin
    if (strmid(lin,0,20) eq 'save $IDL_DICT begin') then begin
      print,lin
      strput,lin,'576 20',21
      strput,lin,' 90',58
      print,lin
      print,'---'
      replinz(ctr)=lin
      linno(ctr)=linctr
      ctr=ctr+1
      endif
    linctr=linctr+1
    endwhile
  close,1

  openr,1,filename
  openw,2,filename+':pslandfix'
  lin=''
  plinctr=linctr
  ctr=0 & linctr=0L

  while not EOF(1) do begin
    readf,1,lin
    if (linctr ne linno(ctr)) then begin
      printf,2,lin
    endif else begin
      printf,2,replinz(ctr)
      ctr=ctr+1
      endelse
    linctr=linctr+1
    if (linctr gt plinctr+5) then goto,BAIL
    endwhile

BAIL:
  close,1,2
  spawn,'/usr/bin/rm '+filename
  spawn,'/usr/bin/mv '+filename+':pslandfix'+' '+filename

  return
end
