; $Id: mkdatadirs.pro,v 1.1 2013/11/20 23:36:55 neill Exp $
;subdirectory designation (000D to 359D)
deg = string(indgen(360), format='(i3.3)')+'D'

root='/users/neill/glga/data'

for i=0,n_elements(deg)-1 do begin

 spawn,'mkdir '+root+'/'+deg[i]
 spawn,'mkdir '+root+'/'+deg[i]+'/aux'
 spawn,'mkdir '+root+'/'+deg[i]+'/plots'
 spawn,'mkdir '+root+'/'+deg[i]+'/photometry'
 spawn,'mkdir '+root+'/'+deg[i]+'/seds'
 spawn,'mkdir '+root+'/'+deg[i]+'/seds/lephare'
 spawn,'mkdir '+root+'/'+deg[i]+'/seds/zpeg'
 spawn,'mkdir '+root+'/'+deg[i]+'/thumbs'
 spawn,'mkdir '+root+'/'+deg[i]+'/galex'
 spawn,'mkdir '+root+'/'+deg[i]+'/galex/fits'
 spawn,'mkdir '+root+'/'+deg[i]+'/galex/jpg'
 spawn,'mkdir '+root+'/'+deg[i]+'/dss'
 spawn,'mkdir '+root+'/'+deg[i]+'/dss/fits'
 spawn,'mkdir '+root+'/'+deg[i]+'/dss/jpg'
 spawn,'mkdir '+root+'/'+deg[i]+'/sdss'
 spawn,'mkdir '+root+'/'+deg[i]+'/sdss/fits'
 spawn,'mkdir '+root+'/'+deg[i]+'/sdss/jpg'
 spawn,'mkdir '+root+'/'+deg[i]+'/2mass'
 spawn,'mkdir '+root+'/'+deg[i]+'/2mass/fits'
 spawn,'mkdir '+root+'/'+deg[i]+'/2mass/jpg'
 spawn,'mkdir '+root+'/'+deg[i]+'/wise'
 spawn,'mkdir '+root+'/'+deg[i]+'/wise/fits'
 spawn,'mkdir '+root+'/'+deg[i]+'/wise/jpg'

endfor

end
