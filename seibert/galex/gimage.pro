PRO gimage, image,h, type=type, $
           log=log, putinfo=putinfo, min=min, $
           max=max, sqr=sqr, asinh = asinh, width=width, height=height, $
           name=name,win=win, ps=ps, true=true, $
           bgcolor=bgcolor, textcolor=textcolor,$
           ctable=ctable, invert=invert,title=title,charsize=charsize,$
           ticklen=ticklen

; ps device does not like WINDOW or CONGRID
; set /PS flag for use under set_plot,'ps'

IF NOT keyword_set(width) THEN width = 512
IF NOT keyword_set(height) THEN height = 512
IF NOT keyword_set(win) THEN win = 0
IF NOT keyword_set(name) THEN name = ''
if not keyword_set(true) then true=0
if keyword_set(sqr) and keyword_Set(log) then $
 print,'Warning: squareroot and log scaling both set'
if not keyword_set(ps) then begin
  ps=0
  if !d.name eq 'X' then begin 
   if !d.window eq win and !d.y_size eq height then wset,win $
   else window,win,xsize=width,ysize=height 
  endif 
endif

if keyword_set(ctable) then loadct,ctable else loadct,0
if keyword_set(invert) then rct

!p.background=0
if keyword_set(bgcolor) then !p.background=bgcolor


image1=image
rmin=min(image1)
rmax=max(image1)
print,'ACTUAL:  min value=',rmin ,'  max value=',rmax

IF n_elements(max) EQ 0 THEN max = rmax
IF n_elements(min) EQ 0 THEN min = rmin

image1 = image1>min
image1 = image1<max

print,'SCALED:  min value=',min(image1) ,'  max value=',max(image1)

if keyword_set(sqr) then begin 
 image1 = sqrt(image1)
 print,'SQRT:   min value=',min(image1) ,'  max value=',max(image1)
endif 

if keyword_set(asinh) then begin 
 image1 = asinh(image1)
 print,'ASINH:   min value=',min(image1) ,'  max value=',max(image1)
endif 


if keyword_set(log) then begin
 ;q = alog10(lindgen(32767)+1)
 image1=alog10(image1 + min(image1) +1)
 print,'LOG:    min value=',min(image1) ,'  max value=',max(image1)
endif
 
sz=size(image1)
if sz[0] eq 3 then begin
 image2=fltarr(sz[2],sz[3])
 image2[*,*] = image1[0,*,*]
 imsetplot, image2, h,type=type,name=name,/nodata,color=textcolor,charsize=charsize,ticklen=ticklen
endif else if sz[0] eq 2 then imsetplot, image1,h,type=type,$
 name=name,/nodata,color=textcolor,title=title,charsize=charsize,ticklen=ticklen


PX = !X.window * !D.X_VSIZE    ;Get size of plot window in device pixels.
PY = !Y.window * !D.Y_VSIZE
SX = PX[1] - PX[0] + 1         ;Desired size of image in pixels.
SY = PY[1] - PY[0] + 1

if keyword_set(true) then true=3
case (ps + true) of

 0: TVSCL, CONGRID(image1, SX, SY), PX[0], PY[0],top=!D.TABLE_SIZE-2
 1: TVSCL, image1, PX[0], PY[0],xsize=sx,ysize=sy,top=!D.TABLE_SIZE-2
 3: TVSCL, CONGRID(image1,sz[0],SX,SY),PX[0],PY[0],top=!D.TABLE_SIZE-2,/true
 4: TVSCL, image1, PX[0], PY[0],xsize=sx,ysize=sy,top=!D.TABLE_SIZE-2,/true

endcase


if n_elements(pmulti) ne 1 then !p.multi[0]=!p.multi[0]+1

if sz[0] eq 3 then imsetplot, image2,h,type=type,name=name,$
  /nodata,/noerase,color=textcolor,title=title,charsize=charsize,ticklen=ticklen

if sz[0] eq 2 then imsetplot, image1,h,type=type,name=name,$
 /nodata,/noerase,color=textcolor,title=title,charsize=charsize,ticklen=ticklen

if n_elements(pmulti) ne 1 then !p.multi[0]=!p.multi[0]-1

end
