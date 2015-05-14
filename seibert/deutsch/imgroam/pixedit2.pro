pro pixedit2,image,mx,my

;+
; PIXEDIT is a primitive Cosmic Ray remover.  I replaces the 3x3 section
; of the image centered on the given X,Y with some random continuum-level stuff
;   Call: pixedit,image,mx,my
;-

COMMON PIXEDIT2,replacement,repl3x3,v2

  if (n_elements(replacement) eq 0) then replacement=0
  if (n_elements(repl3x3) eq 0) then repl3x3=intarr(3,3)

  x=fix(mx+.5) & y=fix(my+.5)
  print,mx,my,'   ---->   ',x,y

  print,"Press 'D'efine replacement value or 'R'eplace this value"
  print,"Press 'E'xtract 3x3 block or 'B'lock replace 3x3"
  key1=strupcase(get_kbrd(1))

  if (key1 eq 'D') then replacement=image(x,y)
  if (key1 eq 'R') then image(x,y)=replacement

  if (key1 eq 'E') then repl3x3=image(x-1:x+1,y-1:y+1)
  if (key1 eq 'B') then image(x-1:x+1,y-1:y+1)=repl3x3

  return

end











