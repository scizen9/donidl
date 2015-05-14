;+
; NAME:
;   MIMAP_MMS_APLLY_TWEAK
;
; VERSION:
;   0.0 (Mar, 2008)
;
; PURPOSE:
;   Apply the sky tweaks found from MIMAP_MMS_TWEAKS for a set of
;   overlapping calibrated images to those images, updating the headers.
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

PRO mimap_mms_apply_tweak,imnames,tweaks,outnames=outnames

nim=n_elements(imnames)
if keyword_set(outnames) EQ 0 then outnames=imnames

for i=0,nim-1 do begin
  im=mrdfits(imnames[i],0,hdr,/silent)
  im=im-tweaks[i]
  
  sxaddpar,hdr,'SKY_TWEAK',tweaks[i],'Value (in BUNIT/pix) subtracted from image by MMS' 
  date=string(systime(/julian),format='(C(CMOI2.2, "/", CDI2.2, "/", CYI))')
  sxaddpar,hdr,'HISTORY',' Modified by MIMAP_MATCH_SKY to include sky value offsets from comparison'
sxaddpar,hdr,'HISTORY','   of overlapping images, '+strcompress(date,/remove_all)

  mwrfits,im,outnames[i],hdr,/create
endfor

end
