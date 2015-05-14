pro pegase_read_onefilter,file_filter,filter
;+
;;defined filter structure and read lambda,trans and transtype from
;;file
;-

filter={filename:'unknown', lambda:dblarr(10000), trans:dblarr(10000), $
        transtype:0, nlambda:0L, $
        calibtype:-1, $
        area:0.0, areanu:0.0,calib:0.0, lambdamean:0., lambdaeff:0., $
        fluxvega:0., fluxsol:0., ABVega:0.}

filter.filename=file_filter
myfilefilter=file_filter
;print,myfilefilter
openr,ufilt,myfilefilter,/get_lun,error=myerror
if (myerror ne 0) then begin
    myroot=getenv('ZPEG_ROOT')
    myfilefilter=myroot+'/data/filters/'+myfilefilter
    openr,ufilt,myfilefilter,/get_lun
endif
;print,myfilefilter


;; skip comment lines ?
a='#'
nlines_header=0
while (strmid(a,0,1) eq '#') do begin
   readf,ufilt,a
   a1=(strsplit(a,' ',/extract))(0)
   a=strtrim(a1,2)
   if strmid(a,0,1) eq '#' then nlines_header++
endwhile
close,ufilt
openr,ufilt,myfilefilter        ; onen file again from the beggining.
for i=0,nlines_header-1 do readf,ufilt,a


anint=0
areal=0.

;readf,ufilt,anint

readf,ufilt,areal

if (areal ne 0 and areal ne 1) then begin ; bad : transmission type undefined
    anint=0
    close,ufilt
    openr,ufilt,myfilefilter ; onen file again from the beggining.
    for i=0,nlines_header-1 do readf,ufilt,a
 endif else begin
    anint=round(areal)
 endelse

filter.transtype=anint

filter.nlambda=0
l=0.
f=0.
while(not(eof(ufilt))) do begin
    readf,ufilt,l,f
    filter.lambda(filter.nlambda)=l
    filter.trans(filter.nlambda)=f
    filter.nlambda=filter.nlambda+1
endwhile
close,ufilt
free_lun,ufilt

end

