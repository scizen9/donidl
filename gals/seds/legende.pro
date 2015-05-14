pro legende,num,psym=psym,linestylep1=linestylep1,$
            chaine,charsize=charsize,thick=thick,symsize=symsize,$
            xpos=xpos,nosym=nosym,linelen=linelen,color=color,$
            chcolor=chcolor,errbar=errbar,errcolor=errcolor,$
            errstyle=errstyle,xdecal=xdecal,_extra=_extra,shaded=shaded

nlines=20.
if (not(keyword_set(charsize))) then charsize=0.7
if (not(keyword_set(color))) then color=0
if (not(keyword_set(thick))) then thick=1.
if (not(keyword_set(symsize))) then symsize=1.

if (not(keyword_set(psym)) and $
    not(keyword_set(linestylep1)) and $
    n_elements(linelen) eq 0) then linelen=1.

if (n_elements(linelen) eq 0) then begin
    if (keyword_set(psym)) then linelen=1. else linelen=3.
endif

if (n_elements(xdecal) eq 0) then xdecal=1.
if (n_elements(xpos) eq 0) then begin
    xpos=!x.crange(0)
    if (!x.type eq 1) then xpos=10.^xpos
endif
frac=(!x.crange(1)-!x.crange(0))/15.

if (!x.type eq 0) then begin
    x0=xpos+frac*xdecal
    x1=x0+linelen*frac
    x2=x0+(linelen+0.5)*frac
endif else begin
    x0=xpos*10^(frac*xdecal)
    x1=x0*10^(linelen*frac)
    x2=x0*10^((linelen+0.5)*frac)
;    print,'x0,x1,x2=',x0,x1,x2
endelse



if (!y.type eq 0) then begin
    y0=!y.crange(0)+(nlines-num)/nlines*(!y.crange(1)-!y.crange(0))
    y1=!y.crange(0)+(0.5+nlines-num)/nlines*(!y.crange(1)-!y.crange(0))
    dysup=(!y.crange(1)-!y.crange(0))/nlines
    dyinf=dysup
endif else begin
    y0=10^!y.crange(0)*10^((nlines-num)/nlines*(!y.crange(1)-!y.crange(0)))
    y1=10^!y.crange(0)*10^((0.5+nlines-num)/nlines*(!y.crange(1)-!y.crange(0)))
    dysup=10.^(!y.crange(1)-!y.crange(0))/nlines-y0
    dyinf=y0-y0/10.^(!y.crange(1)-!y.crange(0))/nlines
endelse

if (not(keyword_set(nosym))) then begin

    if (keyword_set(linestylep1)) then oplot,[x0,x1],[y0,y0],$
      linestyle=linestylep1-1,$
      thick=thick,color=color

    if (keyword_set(psym))      then begin
;        if psym gt 0 then myx0=x0 else myx0=(x0+x1)/2.
       myx0=(x0+x1)/2.
        if (keyword_set(errbar)) then begin
            if (n_elements(errstyle) eq 0) then errstyle=0
            if (n_elements(errcolor) eq 0) then errcolor=0
            oploterror,[myx0],[y0],[dysup],psym=psym,symsize=symsize,color=color,errcolor=errcolor,errstyle=errstyle,/hibar
            oploterror,[myx0],[y0],[dyinf],psym=psym,symsize=symsize,color=color,errcolor=errcolor,errstyle=errstyle,/lobar
        endif else begin
            oplot,[myx0],[y0],psym=psym,symsize=symsize,color=color
        endelse
    endif

    if n_elements((shaded)) ne 0 then begin
       polyfill,[x0,x1,x1,x0],[y0,y0,y1,y1],color=shaded       
    endif

endif

if (not(keyword_set(chcolor))) then begin
    xyouts,x2,y0,chaine,charsize=charsize,_extra=_extra
endif else begin
    xyouts,x2,y0,chaine,charsize=charsize,color=chcolor,_extra=_extra
endelse
end
;----------------------------------------------------------------
