;+
;NAME
; mean_wavelength
;PURPOSE
; Returns the mean wavelength of the filter
;  This version does not use common blocks
;USAGE
; mnwav = mean_wavelength(filternum)
;INPUTS
; filternum      Number of filter in usual system (i.e., index
;                 of filter in filter_info:master_filter)
;RETURNS
; Mean wavelength int( S(l)*l*dl / S(l)*dl )
;COMMON BLOCKS
; filter_info    Not modified
;AUTHOR
; Mark Sullivan
;-
FUNCTION mean_wavelength,filternum

COMMON filter_info, master_filter

IF(N_ELEMENTS(filternum) EQ 0)THEN BEGIN
   PRINT,'ERROR in mean_wavelength - you must pass some filters'
   RETURN,!Values.D_NAN
ENDIF

mean_l=DBLARR(N_ELEMENTS(filternum))
FOR i=0L,N_ELEMENTS(filternum)-1 DO BEGIN
   npoints = master_filter[filternum[i]].npoints
   filter_wave = master_filter[filternum[i]].wave[0:npoints-1]
   filter_response = master_filter[filternum[i]].response[0:npoints-1]
   
   filter_upper=filter_wave*filter_response ; i.e. = l*S(l)
   
   upper=TSUM(filter_wave,filter_upper)
   lower=TSUM(filter_wave,filter_response)
   
   mean_l[i]=upper/lower
ENDFOR

IF ( SIZE(filternum,/DIMENSION) EQ 0 ) THEN RETURN, mean_l[0] ELSE RETURN,mean_l
END
