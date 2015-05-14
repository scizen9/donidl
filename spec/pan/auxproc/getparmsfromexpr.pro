; $Id: getparmsfromexpr.pro,v 1.2 2012/02/10 19:46:23 neill Exp $
; getparmsfromexpr.pro
;
; This function returns an array of parameter
function getparmsfromexpr,expr
; Determine each occurence of the string combination 'p[.]' where
; the . represents a number beginning with zero.  The output here
; is a string array of parameters.

len = strlen(expr)
first = strpos(expr,'p[')
if first eq (-1) then return,(-1)

last = strpos(expr,']')
if last eq (-1) then return,(-1)
parms = strmid(expr,first,last-first+1)

while (first ne (-1)) and (last ne (-1)) do begin
  nextPos = last+1
  first = strpos(expr,'p[',nextPos)
  last = strpos(expr,']',nextPos)
  if (first ne (-1)) and (last ne (-1)) then begin
    newParm = strmid(expr,first,last-first+1)
    parms = [parms,newParm]
  endif
endwhile

; Now that we have the complete array, determine the unique
; members and sort them.
nparms = n_elements(parms)
index = intarr(nparms)
for i = 0l,nparms-1 do begin
  last = strpos(parms[i],']')
  number = fix(strmid(parms[i],2,last-2+1))
  index[i] = number
endfor
isort = sort(index)
parms = parms[isort]
output = parms[uniq(parms,sort(parms))]

return,output
end
