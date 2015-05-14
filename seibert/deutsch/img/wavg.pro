function wavg,array,worerr,weights=weights,errors=errors
;+
; quicky weighted average
;
; results=wavg(array,weights,/weights)
; or
; results=wavg(array,errors,/errors)
;
;-

  if (n_params(0) lt 2) then begin
    print,'Call> results=wavg(array,weights,/weights)'
    print,' or'
    print,'Call> results=wavg(array,errors,/errors)'
    return,-1
    endif

  if (n_elements(weights) eq 0) then weights=0
  if (n_elements(errors) eq 0) then errors=0

  if (weights+errors eq 0) then begin
    print,'WAVG: Error.  either /weights or /errors must be set'
    return,-1
    endif

  if (n_elements(array) ne n_elements(worerr)) then begin
    print,'WAVG: Error.  array and errors must have same dimensions'
    return,-1
    endif

  if (errors ne 0) then begin
    npts=n_elements(worerr)
    errorarr=worerr
    weightarr=1/errorarr^2
    wav1=total(array*weightarr)/total(weightarr)
    wer1=sqrt(1/total(1/worerr^2))
    return,[wav1,wer1]

;    wer1=sqrt( (total(array^2*weightarr)/total(weightarr) - wav1^2) * $
;              (npts/(npts-1)) ) / sqrt(npts)
;      1/sqrt(total(1/worerr^2))]
;      sqrt(total(worerr^2*weightarr))/total(weightarr)] ; fixed 980312
    endif

  weightarr=worerr
  return,total(array*weightarr)/total(weightarr)

end

  
