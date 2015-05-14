pro sdss_fix_wcs,efil
;+
; sdss_fix_wcs - fix Astrometry errors in sdss data
;
; efil - error file from glga_deploy.pro
;-
;
readcol,efil,host,format='a',comment='#'
ng=n_elements(host)
;
sdir=!GLGA_SDSS_DATA+'data/sort/'
flts=['u','g','r','i','z']
nflt=n_elements(flts)
;
; loop over astr
for j=0,ng-1 do begin
    print,string(13B),j+1,'/',ng,host[j],form='($,a1,i7,a1,i7,2x,a)'
    for i=0,nflt-1 do begin
	wcs_flip,sdir+host[j]+'/'+host[j]+'_'+flts[i]+'.fits.gz'
	wcs_flip,sdir+host[j]+'/'+host[j]+'_'+flts[i]+'_ivar.fits.gz'
    endfor
    if not file_test(sdir+host[j]+'/bad_wcs',/directory) then $
	file_mkdir,sdir+host[j]+'/bad_wcs'
    spawn,'mv '+sdir+host[j]+'/*.fits.gz_* '+sdir+host[j]+'/bad_wcs'
    spawn,'gzip '+sdir+host[j]+'/'+host[j]+'*.fits'

endfor

;
return
end
