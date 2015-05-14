function imscl,img,min,max,linearmax,top=top
;+
;
; IMSCL is similar to BYTSCL except is uses a half-linear and half-logarithmic
; scale.
;
; Call> imscl,img-min,0,max,linearmax,top=
; e.g.: imscl,img-300,0,10000,500,top=48
;
; The example call produces a linear stretch of 300-500 for half of the
; dynamic range, and a logarithmic stretch from 500-10000 for the other
; half of the dynamic range.  This allows fine detail visible at low levels
; without burning out stars..  top=48 works the same way as bytscl
;
;-

  if (n_elements(top) eq 0) then top=255

;#### if no linearmax is supplied, just run this bit; it mimics bytscl ######
  if (n_elements(linearmax) eq 0) then begin
    x=findgen(max-min+1)
    y=byte(x*(top*1.0/(max-min)))
    img2=y((img-min>0)<(max-min))
    endif

;#### if linearmax is supplied, use a fancier algorithm ######
  if (n_elements(linearmax) eq 1) then begin

    x=findgen(max-min+1)
    y=byte(x*(top*1.0/(linearmax-min)))

    x1=x(linearmax/2:max-min)-linearmax/2
    x1=x1/max(x1)*1000
    fun1=alog10(x1+30)
    fun2=fun1-min(fun1) & fun2=fun2/max(fun2)
    y(linearmax/2:max-min)=fun2*(top/2.0)+top/2.0
;    plot,x,y,yr=[0,256],ysty=1
    img2=y((img-min>0)<(max-min))
    endif




  return,img2

end








