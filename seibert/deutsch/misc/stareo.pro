;+
; NAME:
;   STAREO
; PURPOSE:
;   STAREO is a main program which dabbles in 3D pattern pictures.
; CALLING SEQEUNCE:
;   .run stareo
; INSTRUCTIONS:
;   Align you eyes to inifinity.  The two dots will now we discrernable as
;   four dots.  Slowly bring your eyes together until there are only three
;   dots (i.e. the two center ones merge.)  If you carefully 'notice' the
;   rest of the image, you will eventually be able to see a rectangle that
;   appears to float a bit above the rest of the noise.  It works okay but
;   not wonderfully...  once you got it, try setting objtype to 1 and 2 for
;   different images.
;         -E. Deutsch 2/16/93
;-



if (n_elements(PS) eq 0) then PS=0

if (n_elements(objtype) eq 0) then objtype=0
				; 0 = box		1 = circle
				; 2 = surprise
winsize=512
seed=105L


; **** set up random pattern ******************************************
  window,0,xs=winsize,ys=winsize
  plot,[0,winsize-1],[0,winsize-1],/nodata,xsty=5,ysty=5,pos=[0,0,1,1]

  for iters=0,1 do begin
    for i=0,30000L do begin
      coord=randomu(seed,2)
      plots,[coord(0)*winsize],[coord(1)*winsize],psym=3,/device
      endfor
    endfor

; **** read object data section **************************************
  object=tvrd(300,6,150,500)

; **** read destination data section *********************************
  dest=tvrd(300-150,6,150,500)

; **** manipulate image for type of object ***************************
  if (objtype eq 0) then dest(*,100:399)=object(*,100:399)

  if (objtype eq 1) then begin
    xdist=fltarr(150,500)
    for i=0,499 do xdist(*,i)=indgen(150)
    ydist=fltarr(150,500)
    for i=0,149 do ydist(i,*)=indgen(500)
    dist=sqrt(abs(xdist-75)^2+(abs(ydist-250)/2)^2)
    dest(where(dist lt 75))=object(where(dist lt 75))
    endif

  if (objtype eq 2) then begin
    window,9,xs=250,ys=250
    IMGtext,100,100,'Kerry','arial24'
    tmp=tvrd(100,90,110,45)
    wdelete,9
    tmp=bytscl(tmp)
    tmp=rotate(tmp,1)
    tmp=congrid(tmp,45*3,110*3)
    tmp2=intarr(150,500)
    tmp2(5:139,100:429)=tmp
    dest(where(tmp2 gt 0))=object(where(tmp2 gt 0))
;    dest(where(tmp2 gt 0))=255
    endif


; **** redisplay 3D image ********************************************
  tv,dest,300-150,6

  plotsym,0,/fill
  plots,[375,225],[450,450],psym=8,symsize=2,/device

  if (PS eq 1) then psout,0,/inv,5,5,1.75,3

end




