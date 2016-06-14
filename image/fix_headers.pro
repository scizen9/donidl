;$Id: fix_headers.pro | Tue Mar 3 16:45:54 2015 -0800 | Don Neill  $
;
; Copyright (c) 2013, California Institute of Technology. All rights
;	reserved.
;+
; NAME:
;	FIX_HEADERS
;
; PURPOSE:
;	Fix FITS headers
;
; CALLING SEQUENCE:
;	FIX_HEADERS,Flist,Hmf
;
; INPUTS:
;	Flist	- list of fits files to fix
;
; OPTIONAL INPUTS:
;	Hmf	- header modifications file (see below), prompts for if missing
;
; KEYWORDS:
;	VERBOSE	- print extra output
;
; OUTPUTS:
;	None.
;
; SIDE EFFECTS:
;	Modifies the headers of all files specified in flist.
;
; PROCEDURE:
;	It is always a good idea to copy the original data files 
;	into a working directory in case anything bad happens.
;
;	The Hmf file that is read in by FIX_HEADERS has four SPACE
;	DELIMITED columns (no tabs):
;
;	Column 1: KEYWORD	this is the header keyword to update or add
;	Column 2: VALUE		this is the value for the keyword
;	Column 3: TYPE		this is the IDL type code for the input:
;				1 - for bytes
;				2 - for integers
;				3 - for long integers
;				4 - for reals
;				5 - for doubles
;				7 - for strings
;				other types are illegal
;	Column 4: AFTER		the keyword after which to insert a new keyword
;				must set to '-' if not needed
;
;	This file is read in and all relevant headers are modified.
;	In addition, two keywords are added recording the file used for the 
;	header modification and the time-stamp of the modification.
;
; HISTORY:
;	Written by:	Don Neill (neill@caltech.edu)
;	2016-JUN-10	Initial version (copied from pcwi_format_headers.pro)
;-
pro fix_headers, flist, hmf, verbose=verbose
;
; program setup
pre = 'FIX_HEADERS'
;
; check inputs
if n_params(0) lt 2 then begin
	hmf = ''
	read,'Header modification file: ',hmf
endif
;
; test header mod file
if not file_test(hmf) then begin
	print,'Header modification file not found: ',hmf
	return
endif
nf = n_elements(flist)
;
; check if we found anything
if nf le 0 then begin
	print,pre,' ERROR - no files found'
	return
endif
print,pre,' INFO - number of images: ',nf
;
; read in header modification file
readcol,hmf,mkeys,val,tcode,after,form='a,a,i,a',comment='#',delim=' '
nmk = n_elements(mkeys)
;
print,pre,' INFO - number of keyword modifications: ',nmk
;
; count number fixed
nfix = 0L
;
; loop over images (flist and imgnos)
for j=0,nf-1 do begin
;
; read in header
	h = headfits(flist[j],errmsg=errmsg)
	;
	; was header read correctly?
	if strlen(errmsg) le 0 then begin
		if keyword_set(verbose) then $
			print,j+1,' / ',nf,'modifying ',flist[j], $
				format='(i,a,i,2x,a,a)'
		;
		; loop over modification records
		for i=0,n_elements(mkeys)-1 do begin
			;
			; log it, if requested
			if keyword_set(verbose) then $
				print,mkeys[i],' to ',val[i], $
					form='(a-10,a,a-30)'
			;
			; convert value string to appropriate type
			case tcode[i] of
				1:	oval = byte(val[i])
				2:	oval = fix(val[i])
				3:	oval = long(val[i])
				4:	oval = float(val[i])
				5:	oval = double(val[i])
				7:	oval = repstr(val[i],'_',' ')	; handle blanks
				else:	begin
					print,'Error - illegal type code: ',tcode[i]
					return
					end
			endcase
			;
			; apply this keyword update
			if strtrim(after[i],2) ne '-' then $
				sxaddpar,h,mkeys[i],oval,after=after[i] $
			else	sxaddpar,h,mkeys[i],oval
		endfor	; loop over modification records
		;
		; count it
		nfix += 1L
		;
		; log modification
		sxaddpar,h,'HFIXFILE',hmf,' file used to modify header'
		sxaddpar,h,'HFIXDATE',systime(0),' header modify date'
		;
		; update fits file with new header
		modfits,flist[j],0,h
		if keyword_set(verbose) then $
				print,' '
	;
	; header not read properly
	endif else begin
		print,'Bad header, skipping ',flist[j]
	endelse
endfor	; loop over images
;
; report
print,'Fixed '+strn(nfix)+' headers'
;
return
end
