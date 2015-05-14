;+
; NAME:
;   MIMAP_MMS_LOADIMS
;
; VERSION:
;   0.0 (Mar, 2008)
;
; PURPOSE:
;   given a list of image names, load them into a 3-D array, Nx X Ny X
;   Nim.  Images must all have the same size
;
; REFERENCE:
;   Smiley, G. 2008 ApJ
;
; CATEGORY:
;   Image Manipulation
;
; CALLING SEQUENCE:
;   result=routine(arg1,arg2,arg3,[ARG4=,OUT1=,/SWITCH1, /SWITCH2])
;
; INPUTS:
;    arg1 - the first argument
;    arg2 - the second argument.  It might have a long description in
;       which case the indentation would be like this.  just like this
;       with so many spaces.
;
; OPTIONAL INPUTS:
;    arg3 - the optional input
;
; KEYWORD PARAMETERS:
;    ARG4 - the keyword that must have a value
;    SWITCH1 -  a switch that can be set
;
;
; OUTPUT:
;    Describe the returned result.
;
;    OUT1 - the other returned output
;
; COMMENTS:
;    Describe useful info
;
; REVISION HISTORY:
;    Oct 2008 - written, B. Johnson
;
;--------------------------------------------------------------------

FUNCTION mimap_mms_loadims,imnames,mask=mask

nim=n_elements(imnames)

im=mrdfits(imnames[0],/silent)
sz=(size(im))[1:2]
mask=intarr(sz)+1
bad=where(im EQ 0 or (finite(im) EQ 0),count)
if count GT 0 then mask[bad]=0

for i=1,nim-1 do begin
  nn=mrdfits(imnames[i],/silent)
  im=[[[im]],[[nn]]]
  bad=where(nn EQ 0 or (finite(nn) EQ 0),count)
  m=intarr(sz)+1
  if count GT 0 then m[bad]=0
  mask=[[[mask]],[[m]]]
endfor

return,im

end
  
