;+
; NAME:
;   MIMAP_MMS_PREP
;
; VERSION:
;   0.0 (Mar, 2008)
;
; PURPOSE:
;   Use swarp to project images onto a common pixel grid in
;   preparation for sky matching (tweak)
;
; REFERENCE:
;   Smiley, G. 2008 ApJ
;
; CATEGORY:
;   Image Manipulation
;
; CALLING SEQUENCE:
;   =routine(arg1,arg2,arg3,[ARG4=,OUT1=,/SWITCH1, /SWITCH2])
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
;    Mar 2008 - written, B. Johnson
;
;--------------------------------------------------------------------
PRO mimap_mms_prep, 
