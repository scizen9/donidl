function zvwgt,z0,z1,h,om,ol
;
; volume weighted z
;
dz=0.001
dvz=fltarr(2000)
ib=fix(z0*1000)>0
ie=fix(z1*1000)<1999
for i=ib,ie do begin
	dvz(i)=dvol(i*dz,dz,1.,h,om,ol)
	if i gt ib then dvz(i) = dvz(i)+dvz(i-1)
endfor
dvz=dvz/dvz(ie)

; median
med=where(dvz ge 0.5)

return,med(0)/1000.
end
