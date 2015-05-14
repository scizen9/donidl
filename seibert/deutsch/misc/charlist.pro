pro charlist,dummy
;+
; NAME:
;   CHARLIST
; PURPOSE:
;   This program opens a file called 'charlist.dat' and then writes out lines
;   0 through 255, where each line contains somthing like 'ascii  65 = "A"'.
;   It is sometimes handy to have a file which contains every possible ascii
;   character.
; CALLING SEQEUNCE:
;   charlist
;-

  openw,1,'charlist.dat'

  for i=0,255 do begin
    printf,1,'ascii ',strn(i,length=3),' = "',string(byte(i)),'"'
    endfor

  close,1
  return

end
