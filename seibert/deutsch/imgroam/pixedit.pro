pro pixedit,image,mx,my,boxsize

;+
; PIXEDIT is a primitive Cosmic Ray remover.  I replaces the nxn section
; of the image centered on the given X,Y with some random continuum-level stuff
;   Call: pixedit,image,mx,my
;-

  if (n_elements(boxsize) eq 0) then boxsize=3
  hsz=fix(boxsize)/2
  boxsize=hsz*2+1

  x=fix(mx+.5) & y=fix(my+.5)
  print,vect([mx,my]),'  ---->   ',vect([x,y,boxsize])

  tmp1=extrac(image,x-(boxsize+2),y-(boxsize+2),(boxsize+2)*2+1,(boxsize+2)*2+1)
  skyline,tmp1,skyv,rms

  tmp1=extrac(image,x-hsz,y-hsz,boxsize,boxsize)
  tmp2=randomu(seed,n_elements(tmp1))*rms*1.5-rms*.5
  tmp1(0)=skyv+tmp2

  imgput,image,tmp1,x-hsz,y-hsz

  return

end











