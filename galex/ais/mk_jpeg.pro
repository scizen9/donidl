;+-------------------------------------------------------------
; NAME:
;       MK_JPEG
; PURPOSE:
;        make a bit-map color jpeg file of an IDL window for (viewable)
;	 insertion into MS Word, or Powerpoint documents.
; CATEGORY:
;       Graphics, Publication
; CALLING SEQUENCE:
;       IDL> mk_jpeg, FILENAME=filename, WINDOW=window, /VERBOSE
; INPUTS:
;       None required 
; KEYWORD PARAMETERS:
;       Optional Inputs:
;	  filename - Name of output file; Default='idljpeg.jpg'
;	  Window   - window number to dump to file
;   	  VERBOSE - If set, print debugging information
; OUTPUTS:
;       Just the file               			out
; COMMON BLOCKS:
;       NONE
; EXAMPLE:
;	IDL> LoadCT, 5
;	IDL> TH_IMAGE_CONT, dist(400)	; make a color contour with color bar
;	IDL> mk_jpeg, filename='myjpeg.jpg'
; NOTES:
; 	do not call mk_color, or other things that limit the number of colors,
; 	before running this, so you get 256 colors.
;
;	You must have priviledges to write the file.
; MODIFICATION HISTORY:
;	28-Jul-2010 add status to detect stale NFS handles
;	09-Aug-2009 Allow filename to be first argument
;	30-Aug-2004 Added test for truecolor (24-bit color), which seems
;		    to be needed for Windows
;       22-Feb-00 Written by Bill Davis, PPPL; with thanks to Dave Fanning 
;--------------------------------------------------------------
pro mk_jpeg, arg1, FILENAME=filename, Image=Image, WINDOW=window, $
             quality=quality, $
	     VERBOSE=verbose,  debug=debug, status=status

if n_elements( debug ) eq 0 then debug = 0
if n_elements( verbose ) eq 0 then begin
   if debug then verbose = 1 else verbose = 0
endif

status=1
Error_status=0
; Establish error handler. When errors occur, the index of the
; error is returned in the variable Error_status:
if not debug then CATCH, Error_status

;This statement begins the error handler:
IF Error_status NE 0 THEN BEGIN
   PRINT, 'FROM MK_JPEG:'
   PRINT, 'Error index: ', Error_status
   PRINT, 'Error message: ', !ERROR_STATE.MSG
   status=0
   return
ENDIF
;;;ON_ERROR,2          ;RETURN TO CALLER IF ERROR

IF N_ELEMENTS( window ) GT 0 THEN wset, window

if n_params() eq 1 then filename= arg1

IF  N_ELEMENTS( filename ) EQ 0 THEN filename = 'idljpeg.jpg'

if n_elements( Image ) gt 0 then begin
   ainfo=size( Image )
   if ainfo[0] eq 3 then trueColor=1 else trueColor=0
   thisImage=Image
endif else begin
   truecolor = 0
   if !d.name eq 'X' OR !d.name eq 'WIN' then begin
      Device, retain=2
      Device, Get_Visual_Depth=thisDepth   
      IF thisDepth GT 8 THEN BEGIN   
	 Device, Decomposed=0   
	 truecolor = 1   
      ENDIF
   endif
   thisImage = TVRD(true=truecolor)
endelse

if NOT trueColor then begin
   TVLCT, red, green, blue, /GET
   s = SIZE( thisImage )
   image3d = BYTARR(3, s(1), s(2))
   image3d(0, *, * ) = red( thisImage )
   image3d(1, *, * ) = green( thisImage )
   image3d(2, *, * ) = blue( thisImage )
    WRITE_JPEG, filename, image3d, TRUE=1, QUALITY=quality
endif else begin
   WRITE_JPEG, filename, thisImage, TRUE=1, QUALITY=quality
endelse
   
IF  verbose THEN PRINT, ' >>> trueColor='+strtrim(trueColor,2)+$
                         ' JPEG file write to ', filename

end
