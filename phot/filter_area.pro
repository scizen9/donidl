;+
;NAME
; filter_area
;PURPOSE
; Calculate the 'area' of a filter in Angstroms
;USAGE
; area=filter_area(filternum)
;INPUTS
; filternum      Number of filter in usual system (i.e., index
;                 of filter in filter_info:master_filter)
;RETURNS
; area of filter in Angstroms -- that is, the integral of
; the filter response over wavelength.
;COMMON BLOCKS
; filter_info    Not modified
;RESTRICTIONS
; Assumes that wavelengths are in Angstroms and are evenly spaced.
;  Should be used with Vega magnitudes
;AUTHOR
; Mark Sullivan
;-
FUNCTION filter_area,filternum

COMMON filter_info, master_filter

npoints = master_filter[filternum].npoints

; trapezoidal integration
RETURN,TSUM(master_filter[filternum].wave[0:npoints-1],$
            master_filter[filternum].response[0:npoints-1] )
END
