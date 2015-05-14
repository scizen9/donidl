FUNCTION polysphere,ra,dec,r,np=np,psi=psi

alpha=double(!DTOR*ra)
delta=double(!DTOR*dec)

if keyword_set(psi) EQ 0 then psi=0d

;define hexagonal vertices in a coordinate system centered on the
;point (theta,phi)=0 (i.e. the positive z axis)
;default to a hexagon if not specified 

;reg=360 MOD np
reg=0
if ((keyword_set(np) EQ 0) or (reg NE 0)) then np=6.

theta=!DTOR*(dblarr(np)+r)
phi=!DTOR*(dindgen(np)*360d/np)

;get cartesian coordinates, add the center point

x=[0,sin(theta)*cos(phi)]
y=[0,sin(theta)*sin(phi)]
z=[1,cos(theta)]

hex=[[x],[y],[z]]


;ROTATE around the sphere

;determine rotation matrix that transforms (0,0) to (ra,dec)
;this has idl's crazy row-column notation indexing

;first, rotate around  x (latitude)

ax=delta

Rx=dblarr(3,3)
Rx[*,0]=[1,0d,0d]
Rx[*,1]=[0d,cos(ax),sin(ax)]
Rx[*,2]=[0d,(0d)-sin(ax), cos(ax)]


hexprime=dblarr(np+1,3)
for ip=0,np do $
  hexprime[ip,*]=transpose(Rx) ## reform(hex[ip,*])

;next, rotate around  y axis (longitude)
ay=alpha

Ry=dblarr(3,3)
Ry[*,0]=[cos(ay),0d,(0d)-sin(ay)]
Ry[*,1]=[0d,1d,0d]
Ry[*,2]=[sin(ay), 0d, cos(ay)]

hexpp=dblarr(np+1,3)
for ip=0,np do $
  hexpp[ip,*]=transpose(Ry) ## reform(hexprime[ip,*])


;convert back from transformed cartesian coordinates to ra, dec

xprime=reform(hexpp[*,0])
yprime=reform(hexpp[*,1])
zprime=reform(hexpp[*,2])

rprime=sqrt(xprime^2+yprime^2+zprime^2)

phiprime=atan(xprime,zprime)/!DTOR
nv=where(phiprime LT 0)
if nv[0] GE 0 then phiprime[nv]=(360d)+phiprime[nv]

thetaprime=acos(yprime)/!DTOR-90.


return,[[phiprime],[thetaprime]]


end
