pro imgbord,xsizein,ysizein,xoffset,yoffset,color,thick,suprs
;+
; NAME:
;   IMGBORD
; DESCRIPTION:
;     This procedure puts a border of the specified color and thickness
;   around an image of the specified size and offset WITHOUT writing over
;   the image.  Therefore, an image with an offset of 0,0 cannot have a
;   border.  Move the image over.
; INPUT:
;   COLOR     the color for the border.  depends on the color table.  def=255
;   THICK     the thickness of the border lines (in pixels)  def=1
;   XSIZE     the Xsize of the image to be framed
;   YSIZE     the Ysize of the image to be framed
;   XOFF      the Xoffset of the image to be framed
;   YOFF      the Yoffset of the image to be framed
; OUTPUT:
;   The border itself
;   all passed variables remain unchanged
; HISTORY:
;   30-MAY-90 Version 1 written  Eric W. Deutsch
;-


  arg=n_params(0)
  if (arg lt 4) or (arg gt 7) then begin
    print,'Call: IDL> IMGBORD,xsize,ysize,xoffset,yoffset    - size & off for the IMAGE'
    print,'      IDL> IMGBORD,xsize,ysize,xoffset,yoffset,color    - w/ color'
    print,'      IDL> IMGBORD,xsize,ysize,xoffset,yoffset,color,thick    - w/ c&thickness'
    print,'e.g.: IDL> IMGBORD,200,300,50,50,0,3'
    goto,BRK
    endif

  if (arg lt 5) then color=255
  if (arg lt 6) then thick=1
  if (arg lt 7) then suprs=0
  noup=suprs and 1 & noright=suprs and 2
  nodown=suprs and 4 & noleft=suprs and 8

  xoff=xoffset & yoff=yoffset
  xsize=xsizein & ysize=ysizein

  if (xoff lt 0) then begin
    xsize=xsize+xoff
    xoff=0 & noleft=1
    endif
  if (yoff lt 0) then begin
    ysize=ysize+yoff
    yoff=0 & nodown=1
    endif

  if (thick gt xoff) and (xoff ge 0) then begin
    xsize=xsize-thick
    xoff=thick
    endif
  if (thick gt yoff) and (yoff ge 0) then begin
    ysize=ysize-thick
    yoff=thick
    endif

  if (xoff+xsize ge !d.x_size) then noright=1
  if (yoff+ysize ge !d.y_size) then noup=1

  on_error,2

  xx=bytarr(xsize+2*thick,thick)
  xx(*,*)=color
  if (nodown eq 0) then tv,xx,xoff-thick,yoff-thick
  if (noup eq 0) then tv,xx,xoff-thick,yoff+ysize

  xx=bytarr(thick,ysize+2*thick)
  xx(*,*)=color
  if (noleft eq 0) then tv,xx,xoff-thick,yoff-thick
  if (noright eq 0) then tv,xx,xoff+xsize,yoff-thick

  on_error,0

BRK:
  return
end
