pro pegase_calibrate_filter,filter
;+
;;compute the fields
;;area areanu lambdamean lambdaeff calib ABvega fluxvega 
;-

if (filter.calibtype le 0) then filter.calibtype=2 ;; AB is default
if (filter.calibtype gt 2) then begin
    print,'Warning : filter calibration is invalid : must be 1 (Vega) or 2 (AB). Set to AB in the following....'
    filter.calibtype=2
endif

c=2.99792458d18
fluxtrans=0.d0
filter.areanu=0.d0
filter.area=0.d0
filter.lambdamean=0.d0

for j=0,filter.nlambda-2 do begin
    lambdamed=(filter.lambda(j)+filter.lambda(j+1))/2. 
    transmed=(filter.trans(j)+filter.trans(j+1))/2.
    filter.lambdamean=filter.lambdamean+lambdamed^(1.+filter.transtype)$
      *transmed*(filter.lambda(j+1)-filter.lambda(j))

    filter.area=filter.area+transmed*$
      (filter.lambda(j+1)-filter.lambda(j))*$
      lambdamed^filter.transtype

    filter.areanu=filter.areanu+transmed*$
      (filter.lambda(j+1)-filter.lambda(j))*$
      lambdamed^filter.transtype/lambdamed^2*c
endfor
filter.lambdamean=filter.lambdamean/filter.area

;;vega calib etc
pegase_read_vega_spectrum,lambdavega,fluxvega

;get the flux density
lambdaeff=0.
calculflux,filter.lambda(0:filter.nlambda-1),$
  filter.trans(0:filter.nlambda-1),filter.transtype,$
  filter.area,lambdavega,fluxvega,[1.],[0.],fluxtrans,lambdaeff=lambdaeff

filter.lambdaeff=lambdaeff

filter.fluxvega=fluxtrans

filter.ABVega=-2.5d0*alog10(fluxtrans*filter.area/filter.areanu)-48.60

case filter.calibtype of
    1: filter.calib=2.5d0*alog10(fluxtrans)+0.03 ;;Vega
    2: filter.calib=2.5d0*alog10(fluxtrans)+filter.ABVega ;; AB
endcase


end

