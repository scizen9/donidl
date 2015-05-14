;+
;NAME
; calc_ab_zp
;PURPOSE
; Cacluate AB zeropoint for a given filter
;USAGE
; abzp = calc_ab_zp(filternum)
;INPUTS
; filternum      Number of filter in usual system (i.e., index
;                 of filter in filter_info:master_filter)
;COMMON BLOCKS
; filter_info    Not modified
;PROCEDURES CALLED
; filter_integ
;NOTE
; abzp is NOT set by this function, just returned
;DESCRIPTION
; The idea here is to deal with the filter normalization.  Then
; if one wants to find the flux of some source,
; one integrates the flux through the filter and
; divides by this zeropoint.  That is:
;
;   f = filter_integ(filternum,sed_wave,
;         sed_flux_lambda, 0.0) / calc_ab_zp( filternum )
;
; And the AB mag is given by
;
;   AB = -2.5 log_10 f - 48.60
;
; As described by Oke & Gunn, 1983.  The system is defined so that
; a source with f_nu = 3.63078e-20 erg s^-1 cm^-2 Hz^-1 has AB_nu = 0
; at all frequencies (this is 3630.78 Jy).  Recall that 
; we use f_lambda units of erg s^-1 cm^-2 A^-1.
;
; This is equal to c \int R \lambda^{-2}, which is what you
; want to divide the results of filter_integ by (assuming that
; the flux passed in is in f_lambda units) to make things equivalent
; to the f_nu integral that defines the AB broadband system from
; Fukugita et al. 1996.
;AUTHOR
; Alex Conley
;-

FUNCTION calc_ab_zp,filternum

COMPILE_OPT IDL2, STRICTARRSUBS
COMMON filter_info, master_filter

;;Set up f_lambda to be a system with f_nu = 1 erg s^-1 cm^-2 Hz^-1
;wave = DINDGEN(30000)+500.d0
wave = master_filter[filternum].wave[0:master_filter[filternum].npoints-1]
f_lambda = 2.99792458d18 / wave^2  ;;Convert from Hz^-1 to A^-1

RETURN,filter_integ(filternum,wave,f_lambda,0.d0, /NODOUBLESAMPLE)

END
