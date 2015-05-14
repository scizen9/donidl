pro imstat,image,mx,my
;+
; NAME:
;   IMSTAT
; DESCRIPTION:
;   This procedure prints out various image "statistics" namely mean, max,
;   minimum, and a sky value of a 20x20 pixel box centered at the supplied
;   X,Y coorddinates.
; INPUT:
;   X         The X pixel coordinate
;   Y         The Y pixel coordinate
; OUTPUT:
;   Printed statistics
;   All passed variables remain unchanged
; HISTORY:
;   27-MAY-94 Header added to old code by Eric W. Deutsch
;-

  x=fix(mx+.5) & y=fix(my+.5)

  tmp1=extrac(image,x-10,y-10,20,20)
  skyline,tmp1,skyv,rms

  avg1=avg(tmp1) & sig=stdev(tmp1)
  maxv=max(tmp1) & minv=min(tmp1)

  print,'IMAGE(',strn(x-10),':',strn(x+19),',',strn(y-10),':',strn(y+19),')'
  print,'SkyLine ',vect(['SkyVal','RMS']),': ',vect([skyv,rms])
  print,vect(['Mean','Sigma']),': ',vect([avg1,sig])
  print,vect(['MaxVal','MinVal']),': ',vect([maxv,minv])
  print,' '

  return

end











