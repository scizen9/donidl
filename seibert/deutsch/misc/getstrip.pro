function GetStrip,img,xcent,ycent,angle,length,xvect,yvect
;+
; NAME:
;   GETSTRIP
; PURPOSE:
;   This procedure returns a profile through an image at any angle or position.
;   First the theoretical line is calculated given the center, angle and length
;   in pixels.  Then pixel in the profile is bilinearly interpolated from the
;   pixels in the image.
; CALLING SEQEUNCE:
;   result = GetStrip(img,xcent,ycent,angle,length,xvect,yvect)
; INPUT:
;   IMG       This is the 2D image array from which the profile is to be taken.
;   XCENT     This is the X coordinate of the center of the profile.  It does
;               not need to be an exact integer.  nn.0 references the exact
;               X center of a pixel.
;   YCENT     This is the Y coordinate of the center of the profile.  It does
;               not need to be an exact integer.  nn.0 references the exact
;               Y center of a pixel.
;   ANGLE     This is the angle of the profile in degrees counterclockwise
;               from the positive X axis.
;   LENGTH    This is the length in elements (and pixels) of the returned
;               profile.  Therefore no matter what the angle is, the returned
;               vector will have LENGTH elements and is a profile with a
;               physical length of LENGTH image pixels.
; OUTPUT:
;   result       The returned profile array.
; OPTIONAL OUTPUT:
;   XVECT     This variable returns the array of positions of each profile
;               pixel.  These returned values are floating.  This returned
;               vector is especially useful for averaging along wide strips.
;   YVECT     This variable returns the array of positions of each profile
;               pixel.  These returned values are floating.
; EXAMPLE:    In a North Up and East Left image of size 512x512 and pixelsize
;               of 1.5", the following call:
;                 IDL> TMP=GetStrip(img,256,256,45,51)
;               returns a 51 element vector where tmp(25)=img(256,256) and
;               elements less than 25 are to the Southeast and elements greater
;               than 25 increase toward the Northwest.  The pixel size of the
;               strip TMP is 1.5" also.
;                 IDL> tmp=GetStrip(img,256,256,0,51)
;               is equivalent to...
;                 IDL> tmp=img(256-25:256+25,256)
; HISTORY:
;   27-JUL-92 Added header and spiffed up this procedure.   (E. Deutsch)
;   12-NOV-92 Removed extra +.5's inside the interpolate().  (E. Deutsch)
;-

  theta=angle/!radeg
  pt1=[xcent+.5*(length-1)*cos(theta),ycent+.5*(length-1)*sin(theta)]
  pt2=[xcent-.5*(length-1)*cos(theta),ycent-.5*(length-1)*sin(theta)]

  len=indgen(length)
  xvect=pt2(0)+len*cos(theta)
  yvect=pt2(1)+len*sin(theta)

  strip=interpolate(img,xvect,yvect)

  return,strip

end
