pro printpath,dummy
;+
; NAME:
;   PRINTPATH
; PURPOSE:
;   This program prints out the current !PATH in a column on unix machines.
;   (or other "colon-separated !PATH variables)
; CALLING SEQEUNCE:
;   printpath
;-


  len=strlen(!path)
  i=0 & en=0

  while (en ne 9999) do begin
    en=strpos(!path,':',i)
    if (en eq -1) then en=9999
    print,strmid(!path,i,en-i)
    i=en+1
    endwhile
      
  return
end
