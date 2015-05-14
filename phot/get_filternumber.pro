;+
;NAME
; get_filternumber
;PURPOSE
; Returns the filter number corresponding to the filter in
; master_filter (from filter info common block)
;USAGE
; filtnum = get_filternumber( shortid )
;INPUTS
; shortid         The shortid of the filter.  For example, 'U'.
;                  Can also be an array
;RETURNS
; An index such that master_filter[ index ] is a filter structure
;  containing info about the desired filter.  Returns -1 if not
;  found, and will only return the first matching index if there
;  is more than one match (which is probably a bad sign in any case)
;  If shortid is an array, then an array is returned.
;COMMON BLOCKS
; filter_info      Unmodified
;RESTRICTIONS
; The filter has to have already been loaded into filter_info --
;  see read_filter_file
;USAGE
; Ufiltnumber = get_filternumber('U')
;MODIFICATION HISTORY
; Author: Alex Conley
;-
FUNCTION get_filternumber,shortid

COMMON filter_info,master_filter

ON_ERROR,2

nfilt = N_ELEMENTS( shortid )

IF SIZE(shortid,/TYPE) NE 7 THEN BEGIN
    errstr = "ERROR in get_filternumber -- argument must be a string or " + $
             "an array of strings"
    MESSAGE,errstr
ENDIF
    
retarr = intarr( nfilt )

FOR i=0,nfilt-1 DO BEGIN
    wfilt = where( shortid[i] EQ master_filter.shortid )
    retarr[i] = wfilt[0]
ENDFOR

;;Return an array if passed an array, a scalar if a scalar
IF ( SIZE(shortid,/DIMENSION) EQ 0 ) THEN RETURN,retarr[0] ELSE RETURN,retarr

END
