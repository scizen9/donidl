; $Id: homanycommas.pro,v 1.1 2010/05/14 16:32:56 neill Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function howmanycommas,instr
len = strlen(instr)
n = 0l
for i = 0l,len-1 do begin
  thisChar = strmid(instr,i,1)
  if thisChar eq ',' then n = n+1
endfor
return,n
end
