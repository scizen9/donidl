PRO cover_points,ra,dec,r,objid,allobj,allcoo,twomass=twomass,wise=wise

;PURPOSE - get a series of concentric polygons that 
;          'evenly' cover a circular region 

nobj=n_elements(objid)
dr=0.16379d ;the minimum dimension of an SDSS image in degrees
if keyword_set(twomass) then $
	dr=0.14222d	; 2MASS minimum dimension in degrees
if keyword_set(wise) then $
	dr=0.77611d	; 2MASS minimum dimension in degrees
dr=dr/2.

;calculate number of concentric circles required to cover the object
;with the minimum separation
nr=ceil(r/dr)

allcoo=fltarr(1,2)
allobj=0.
for iobj=0,nobj-1 do begin
  np=fltarr(nr[iobj])
  ;calculate number of points around the circle to get a spacing ~dr
  for i=0d,nr[iobj]-1 do np[i]=ceil(2.*!PI*sin(dr*(i+1)*!DTOR)/(dr*!DTOR))
;  while total((360 MOD np)) NE 0 do begin
;    odd=where((360 mod np) NE 0)
;    np[odd]=np[odd]+1
;  endwhile
  coo=transpose([ra[iobj],dec[iobj]])
  for i=0d,nr[iobj]-1 do coo=[coo,(polysphere(ra[iobj],dec[iobj],dr*(i+1),np=np[i]))[1:*,*]]
  allcoo=[allcoo,coo]
  ntc=n_elements(coo)/2.
  obj=fltarr(ntc)+objid[iobj]
  allobj=[allobj,obj]
  
endfor


allcoo=allcoo[1:*,*]
allobj=allobj[1:*]

;stop

end
