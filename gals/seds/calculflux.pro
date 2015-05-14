pro calculflux,lambdafilter,trans,typetrans,area,$
               lambda,flux,lambdaline,fluxline,$
               fluxfilter,lambdaeff=lambdaeff,error=error
;+
;;return the flux density in the filter
;;(lambdafilter,trans,typetrans) in erg/s/A
;-

error=0
fluxfilter=0.

nlambdafilter=n_elements(lambdafilter)
nlambda=n_elements(lambda)
nlines=n_elements(lambdaline)

;we check the wavelength scale is sorted
isort=bsort(lambda)
if (total(isort ne indgen(nlambda)) ne 0) then begin
    error=1
    print,'error: wavelength scale not sorted by increasing wavelenth !'
    return
endif

fluxtrans=0.d0
if (lambdafilter(0) lt lambda(0) or $
    lambdafilter(nlambdafilter-1) gt lambda(nlambda-1)) then begin
;    print,lambdafilter(nlambdafilter-1),lambda(nlambda-1)
    error=1
    return
endif                           ; else begin
k=0
while(lambda(k+1) lt lambdafilter(0)) do  k=k+1    
p=0
lambdainf=lambdafilter(p)
transinf=0.d0
fluxinf=0.d0
lambdaeff=0.
lambdasup=lambdainf
while(p+1 le nlambdafilter-1 and k+1 le nlambda-1) do begin
;--------------------------------------------------
    if (lambdafilter(p+1) lt lambda(k+1)) then begin
        lambdasup=lambdafilter(p+1)
        if (flux(k)*flux(k+1) eq 0) then begin
            fluxsup=0.
        endif else begin
            fluxsup=10^(interpol(alog10([flux(k),flux(k+1)]),$
                                 alog10([lambda(k),lambda(k+1)]),$
                                 alog10(lambdasup)))(0)
        endelse
        transsup=trans(p+1)*lambdasup^typetrans
        p=p+1
    endif else begin
        lambdasup=lambda(k+1)
        fluxsup=flux(k+1)
        transsup=(interpol([trans(p),trans(p+1)],$
                           [lambdafilter(p),lambdafilter(p+1)],$
                           [lambdasup]))(0)$
          *lambdasup^typetrans
        k=k+1
    endelse
    fluxtrans=fluxtrans+(transinf*fluxinf+transsup*fluxsup)$
      *(lambdasup-lambdainf)/2.

    lambdaeff=lambdaeff+(transinf*fluxinf+$
                         transsup*fluxsup)*$
      (lambdasup-lambdainf)/2.*(lambdasup+lambdainf)/2.
    
    lambdainf=lambdasup
    fluxinf=fluxsup
    transinf=transsup
endwhile

lambdaeff=lambdaeff/fluxtrans

for k=0,nlines-1 do begin
    p=0
    if ((lambdaline(k) gt lambdafilter(0)) and $
        (lambdaline(k) lt lambdafilter(nlambdafilter-1)))  then begin
        while (not((lambdaline(k) ge lambdafilter(p))$
                   and (lambdaline(k) lt lambdafilter(p+1)))) do p=p+1
        transline=(trans(p)$
                   +(lambdaline(k)-lambdafilter(p)) $
                   *(trans(p+1)-trans(p))$
                   /(lambdafilter(p+1)-lambdafilter(p)))*$
          lambdaline(k)^typetrans
        fluxtrans=fluxtrans+transline*fluxline(k)
    endif
endfor          
fluxfilter=fluxtrans/area
;endelse
end



