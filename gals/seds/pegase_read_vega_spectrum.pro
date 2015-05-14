pro pegase_read_vega_spectrum,lambdavega,fluxvega
;+
;; reads the vega spectrum
;-

;openr,uni,'/home/leborgne/astro/pegase/PEGASE2new/VegaLCB.dat',/get_lun,error=myerror
;if (myerror ne 0) then begin
    myroot=getenv('ZPEG_ROOT')
    openr,uni,myroot+'/data/VegaLCB_IR.dat',/get_lun,error=myerror
;endif    
nlambdavega=1
readf,uni,nlambdavega
lambdavega=(fluxvega=dblarr(nlambdavega))
for i=0,nlambdavega-1 do begin
    l=0.
    f=0.
    readf,uni,l,f
    lambdavega(i)=l
    fluxvega(i)=f
endfor
close,uni
free_lun,uni
end

