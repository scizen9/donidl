pro irafrd,im,hd,filename, SILENT=silent    ;Read in IRAF image array and header array
;+
; NAME:
;	IRAFRD
; PURPOSE:
;	Read an IRAF (OIF) file and read into an IDL image and header arrays.
;	Program will prompt for the file name if not supplied.
;
; CALLING SEQUENCE:
;	IRAFRD, im, hdr, filename, [/SILENT ]  
;
; OPTIONAL INPUT:
;	FILENAME -  Character string giving the name of the IRAF image 
;		header.  If omitted, then program will prompt for the 
;		file name.  IRAFRD always assumes the header file has an 
;		extension '.imh'.    IRAFRD will automatically locate the
;		".pix" file containing the data by parsing the contents of 
;		the .imh file.   (If the parse is unsuccesful, then IRAFRD looks
;		in the same directory as the .imh file.)
; OUTPUTS:
;	IM - array containing image data
;	HDR - string array containing header.  Basic information in the
;		IRAF header is converted to a FITS style header
;
; OPTIONAL INPUT KEYWORDS:
;	/SILENT  - If this keyword is set and non-zero, then messages displayed
;		while reading the image will be suppressed.  
;
; SIDE EFFECTS:
;	IF FILENAME is not supplied, then the program will check that
;	the image and header variable do not already contain data.
;
; RESTRICTIONS:
;	(1)  Image size and history sections of the IRAF header are copied 
;		into the FITS header HDR.  Other information (e.g. astrometry)
;		might not be included unless it is also in the history section
;	(2)  IRAFRD ignores the node name when deciphering the name of the
;		IRAF ".pix" file.
;	(3)  Certain FITS keywords ( DATATYPE, IRAFNAME) may appear more than
;		once in the output name
;
; NOTES:
;	IRAFRD obtains dimensions and type of image from the IRAF header.
;
; PROCEDURES CALLED:
;	FDECOMP, SXADDPAR, SXPAR()
;
; MODIFICATION HISTORY:
;	Written W. Landsman, STX January 1989
;	Converted to IDL Version 2.  M. Greason, STX, June 1990
;	Updated for DecStation compatibility   W. Landsman   March 1992
;	Don't leave an open LUN  W. Landsman   July 1993
;	Don't overwrite existing OBS-DATE  W. Landsman  October 1994
;	Don't bomb on very long FITS headers W. Landsman  April 1995
;	Work on Alpha/OSF and Linux      W. Landsman     Dec 1995
;	Remove /VMSIMG keyword, improve efficiency when physical and
;		image dimensions differ   W. Landsman     April 1996
;-
 On_error,2                    ;Return to caller
 npar = N_params() 

 if ( npar EQ 0 ) then begin 
   print,'Syntax - irafrd, im, hdr, [filename, /SILENT ]'
   return
 endif 

 big_endian = (!VERSION.OS EQ "vms") or (!VERSION.ARCH EQ "mipsel") or $
              (!VERSION.OS EQ "OSF") or (!VERSION.ARCH EQ "x86")

 warn = string(7b) + 'IRAFRD: WARNING - '

 if ( npar EQ 1 ) then begin        ;No output header array supplied
   ans = ''
   print,warn,'A name for the header array was not supplied.'
   read,'Continue the procedure to read the image array only [YES]? ',ans
   if ( strmid(strupcase(ans),0,1) EQ 'N' ) then return
 endif

 if (npar lt 3) and (N_elements(im) NE 0 ) then begin ;Already contain data?
   ans = ''
   print,warn,'Image array contains data that will be erased'
   read,'Continue the procedure to read the image array [YES]? ',ANS
   if ( strmid(strupcase(ans),0,1) EQ 'N' ) then return    
 endif

 if ( npar EQ 3 ) then $
    if ( N_elements(filename) EQ 0 ) then message, $
        'Third parameter (IRAF Header file name) must be a character string' $
    else begin  
     	 file_name = filename
	 goto,FINDER
    endelse 

  file_name = ''  ;Get file name if not supplied
  read,'Enter name of IRAF data file (no quotes, no extensions): ',file_name    
  if ( file_name EQ '' ) then return

FINDER: 
  fdecomp,file_name,disk,dir,name,ext,ver  
  if !VERSION.OS NE 'vms' then $           ;In Unix, assume extension is part
    if ( ext NE '' ) then name = name + '.'+ext          ;of a compound name

  fname = findfile(disk + dir + name + '.imh', COUNT = n)  
  if (n EQ 0 ) then message, $
     'Unable to find IRAF header file '+ spec_dir(disk+dir+name+'.imh') 

  openr, lun1, fname(0), /STREAM, /GET_LUN  ;Open the IRAF header file

; Get image size and name from IRAF header

 tmp = assoc(lun1,bytarr(572))
 hdr = tmp(0)
 hdr2 = hdr
if not big_endian then begin
    byteorder,hdr,/sswap
    byteorder,hdr,/lswap
 endif

 hdrlen =   fix(hdr,12)         ;Length (in words) of header
 datatype = fix(hdr,16)         ;IRAF datatype
 ndim =     fix(hdr,20)         ;Number of dimensions
 if ( ndim GT 5 ) then $
    message,'Too stupid to do more than 5 dimensions'

 dimen = long(hdr2,24,ndim)       ;Get vector of image dimensions 
 physdim = long(hdr2,52,ndim)     ;Get vector of physical dimensions

 if big_endian then pixname = string( hdr(412+indgen(80)*2) ) else $
                    pixname = string( hdr2(413+indgen(80)*2) )
 expos = strpos(pixname,'!')
 pixname = strmid(pixname,expos+1,strlen(pixname))

 if strmid(pixname,0,4) eq 'HDR$' then begin
	if disk + dir EQ '' then begin 
		cd, CURRENT = curdir 
		if !VERSION.OS NE 'vms' then curdir = curdir + '/'
	endif else curdir = disk+dir
	pixname = curdir +  strmid(pixname,4,strlen(pixname))
 endif

;  Use file name found in header to open .pix file.  If this file is not
;  found then look for a .pix file in the same directory as the header  
 
 openr, lun2, pixname, ERROR=err, /STREAM, /GET_LUN     ; ...on given directory

 if ( err LT 0 ) then begin
     openr,lun2, name + '.pix',/STREAM, ERROR = err, /GET_LUN   
     if ( err LT 0 ) then goto, NOFILE   
 endif 

 if not keyword_set(SILENT) then begin 
                                            
	sdim = strtrim(dimen(0),2)
	if ( ndim GT 1 ) then for i = 1,ndim-1 do $
		sdim = sdim + ' by ' + strtrim(dimen(i),2) $
	else sdim = sdim + ' element' 
	message,'Now reading '+sdim+' IRAF array', /INFORM
 endif 

;       Convert from IRAF data types to IDL data types

 CASE datatype OF
	1: begin & dtype = 1  & bitpix = 8 & end            ;Byte 
	3: begin & dtype = 2  & bitpix = 16 & end            ;Integer*2 
	4: begin & dtype = 3  & bitpix = 32 & end            ;Integer*4 
	5: begin & dtype = 3  & bitpix = 32 & end            ;Integer*4 
	6: begin & dtype = 4  & bitpix = -32 & end           ;Real*4 
	7: begin & dtype = 5  & bitpix = -64 & end            ;Real*8 
	else: message,'Unknown Datatype Code ' + strtrim(datatype,2)
 endcase 

; Read the .pix file, skipping the first 1024 bytes.   The last physical 
; dimension can be set equal to the image dimension.

 physdim(ndim-1) = dimen(ndim-1)
 tmp = assoc (lun2, make_array(DIMEN = physdim, TYPE= dtype, /NOZERO), 1024)
 im = tmp(0)

; If the physical dimension of an IRAF image is larger than the image size,
; then extract the appropriate subimage

 dimen = dimen - 1
 pdim = physdim - 1
 case ndim of
	1 :
	2 : if dimen(0) LT pdim(0) then im = im( 0:dimen(0), *)
	3 : if total(dimen LT pdim) then im = im( 0:dimen(0), 0:dimen(1), * )
	4 : if total(dimen LT pdim) then $
		im = im( 0:dimen(0), 0:dimen(1), 0:dimen(2), * )
	5 : if total(dimen LT pdim) then $
		im = im( 0:dimen(0), 0:dimen(1), 0:dimen(2), 0:dimen(3), *)
 endcase

 hd = strarr(ndim + 5) + string(' ',format='(a80)')      ;Create empty FITS hdr
 hd(0) = 'END' + string(replicate(32b,77))
  
 sxaddpar, hd, 'SIMPLE', 'T',' Written by IDL:  '+ systime()
 sxaddpar, hd, 'BITPIX', bitpix
 sxaddpar, hd, 'NAXIS', ndim        ;# of dimensions
 if ( ndim GT 0 ) then $
   for i = 1, ndim do sxaddpar,hd,'NAXIS' + strtrim(i,2),dimen(i-1)+1

 sxaddpar,hd,'irafname',strupcase( name + '.imh')   ;Add history records

 if ( hdrlen GT 513 ) then begin    ;Add history records

	nfits = (hdrlen*4l - 2054)/162
	tmp = assoc(lun1,bytarr(hdrlen*4l))
	hdr = tmp(0)
	if not big_endian then byteorder, hdr, /SSWAP 
SKIP1:	endline = where( strmid(hd,0,8) EQ 'END     ')     
        endline = endline(0)
	endfits = hd(endline)
	hd = [ hd(0:endline-1), strarr(nfits+1) ]
        index = indgen(80)*2

	for i = 0l,nfits-1 do $
		hd(endline+i) = string( hdr( 2052 + 162*i + index) )
	hd(endline + nfits) = endfits         ;Add back END keyword
	object = string( hdr(732 + indgen(67)*2) ) 
	if (object EQ '') then object = ' '
	sxaddpar, hd, 'OBJECT', object, ' ', 'OBJECT NAME'    ; Add object name record

	history = string(hdr ( 892 + indgen(580)*2))
	st1 = gettok( history, string(10B))             
	if big_endian then $
		origin = gettok( strmid( st1, 1, strlen(st1)),"'") else $
		origin = gettok( strmid( st1, 0, strlen(st1)),"'")
	sxaddpar, hd, 'ORIGIN', origin, ' ', 'IRAFNAME'   ; Add 'ORIGIN" record

        test = sxpar(hd,'HISTORY', COUNT = N)
        if N EQ 0 then begin
	 while (strpos(history,string(10B)) GE 0) do begin

		 hist_rec = gettok( history, string(10B) ) ; Add history comment strings
		 sxaddpar, hd, 'HISTORY', hist_rec
	 endwhile
       endif
 endif

 free_lun,lun1,lun2

 return                        ;Successful return

NOFILE:  

 message,'Unable to find IRAF pixel file ' + pixname,/CON
 free_lun,lun1
 return

 end 
