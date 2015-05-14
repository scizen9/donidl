;+
;NAME
; extract_filter
;PURPOSE
; extracts the filterresponse given the filter number
;USAGE
; extract_filter,filternumber,wave,response
;INPUTS
; filter            Which filter you want.  Either a string shortid
;                    or a filter number in the usual system.
;RETURNS
; wave              Wavelength array of response
; response          Response
;COMMON BLOCKS
; filter_info       Not modified by this routine
;AUTHOR
; Mark Sullivan
;-
PRO extract_filter,filter,wave,response

;; MS August 2004

;; extracts filter response from master array

COMMON filter_info, master_filter

IF SIZE( filter, /TYPE ) EQ 7 THEN $
  filternumber = get_filternumber( filter ) ELSE filternumber = filter

IF(filternumber GE N_ELEMENTS(master_filter)) THEN BEGIN
    PRINT,"ERROR -- extract_filter in filter.pro"
    PRINT,"Filter ",filternumber," not known."
    STOP
ENDIF

npoints=master_filter[filternumber].npoints
wave=master_filter[filternumber].wave[0:npoints-1]
response=master_filter[filternumber].response[0:npoints-1]

;;Get rid of unused points
junk=REFORM(response,npoints,/OVERWRITE)
junk=REFORM(wave,npoints,/OVERWRITE)

RETURN
END
