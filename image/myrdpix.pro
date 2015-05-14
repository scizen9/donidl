; $Id: myrdpix.pro,v 1.3 2011/09/15 18:44:04 neill Exp $
;
; Copyright (c) 1989-2000, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.

Pro myrdpix, Image, X0, Y0, Mag, Xr, Yr, Ret
;	Read the value of the pixel under the cursor
;	Display x,y and the pixel value under the cursor
;+
; NAME:
;	MYRDPIX
;
; PURPOSE:
;	Interactively display the X position, Y position, and pixel value
;	of the cursor.
;
; CATEGORY:
;	Image display.
;
; CALLING SEQUENCE:
;	MYRDPIX, Image [, X0, Y0, Mag, Xr, Yr, Ret]
;
; INPUTS:
;	Image:	The array that represents the image being displayed.  This
;		array may be of any type.  Rather reading pixel values from
;		the display, they are taken from this parameter, avoiding
;		scaling difficulties.
;
; OPTIONAL INPUT PARAMETERS:
;	X0, Y0:	The location of the lower-left corner of the image area on
;		screen.  If these parameters are not supplied, they are
;		assumed to be zero.
;	Mag:	The magnification factor of the coordinates.
;	Xr, Yr:	The reference position from which R is calculated.
;
; OUTPUTS:
;	Ret:	Returns 1 for left button, 2 for middle button, 3 for right
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	The X, Y, and value of the pixel under the cursor are continuously
;	displayed.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Instructions are printed and the pixel values are printed as the
;	cursor is moved over the image.
;
;	Press the left or center mouse button to create a new line of output,
;	saving the previous line.
;
;	Press the right mouse button to exit the procedure.
;
; MODIFICATION HISTORY:
;	DMS, Dec, 1987.
;	Rob Montgomery (rob@hao.ucar.edu), 9/21/92;
;		Correct indices for case of !order = 1
;
;-

COMPILE_OPT strictarr
on_error,2              ;Return to caller if an error occurs
;print,'Press left or center mouse button for new output line."
;print,'... right mouse button to exit.'
s = size(image)
if s[0] ne 2 then message, 'Image parameter not 2d.'
s[1] = s[1]-1		;To n-1
s[2] = s[2]-1
!mouse.button=0
if n_elements(x0) le 0 then x0 = 0
if n_elements(y0) le 0 then y0 = 0
if n_elements(mag) le 0 then mag = 1.0
if s[s[0]+1] ge 4 then form = 'F9.2' else form = 'I5'
case !version.os_family of
        'Windows': cr = string("15b)+string("12b)	; carriage and new line
        'MacOS': cr = string("15b)			; carriage return
        'unix': cr = string("15b)			; carriage (for BC on
							; UNIX use CR rather
							; than CR/LF)
        else: cr = string("15b)				; carriage return
endcase
form="($,'r=',f7.2,', x=',f7.2,', y=',f7.2,', value=',"+form+",a)"
while !mouse.button ne 1 and !mouse.button ne 2 and !mouse.button ne 4 do begin
	CURSOR,x,y,2,/dev
;	if (!mouse.button eq 3) or (!mouse.button eq 4) then begin ;New line?
;	   print,form="($,a)",string("12b)
;	   while (!mouse.button ne 0) do begin wait,.1 & CURSOR,x,y,0,/dev & end
;	endif

	xc = (x/float(mag)) + x0 & yc = (y/float(mag)) + y0
	r = sqrt( (xc-xr)^2 + (yc-yr)^2 )
	if (x le s[1]) and (y le s[2]) and (x ge 0) and (y ge 0) then begin
	   if (!order eq 1) then yy = s[2] - y else yy = y
	   print,form = form, r, xc,yc,Image[x,yy],cr
	endif
endwhile
ret = !mouse.button
print,form="(/)"
end
