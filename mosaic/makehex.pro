PRO cover_points,ra,dec,r,objid


;get a series of concentric polygons that 'evenly' cover a circular region 

dr=0.16379d ;the minimum dimension of an SDSS image in degrees

;calculate number of concentric circles required to cover the object
;with the minimum separation
nr=r/dr

allcoo=0.
allobj=0.
for iobj=0,nobj-1 do begin
  np=fltarr(nr[iobj])
  ;calculate number of points around the circle to get a spacing ~dr
  for i=0d,nr[iobj]-1 do np[i]=ceil(2.*!PI*sin(dr*(i+1)*!DTOR)/(dr*!DTOR))
  while total((360 MOD np)) NE 0 do begin
    odd=where((360 mod np) NE 0)
    np[odd]=np[odd]+1
  endwhile
  coo=0
  for i=0d,nr[iobj]-1 do coo=[coo,vertices(ra[i],dec[i],dr*(i+1),np=np[i])]
  coo=coo[1,*]
  allcoo=[allcoo,coo]
  ntc=n_elements(coo)
  obj=fltarr(ntc)+objid[iobj]
  allobj=[allobj,obj]
  
endfor

