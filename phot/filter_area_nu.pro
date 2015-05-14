;+
;NAME
; filter_area_nu
;PURPOSE
; Calculate the 'area' of a filter in Hertz
;USAGE
; area=filter_area_nu(filternum)
;INPUTS
; filternum        Filter number in usual system (i.e., index in
;                   filter_info:master_filter)
;RETURNS
; area of filter in Hertz
;COMMON BLOCKS
; filter_info      Not modified
;RESTRICTIONS
; The filter wavelength must be in Angstroms.
; Should be used with AB mags.
;AUTHOR
; Mark Sullivan
;-
FUNCTION filter_area_nu,filternum

COMMON filter_info, master_filter

npoints = master_filter[filternum].npoints

c = 2.99792458d18
wave = master_filter[filternum].wave[0:npoints-1]
response = c * master_filter[filternum].response[0:npoints-1]/wave^2

RETURN,TSUM( wave, response )

END
