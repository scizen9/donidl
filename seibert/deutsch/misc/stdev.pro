; $Id: stdev.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

Function STDEV, Array, Mean
;
;+
; NAME:
;	STDEV
;
; PURPOSE:
;	Compute the standard deviation and, optionally, the
;	mean of any array.
;
; CATEGORY:
;	G1- Simple calculations on statistical data.
;
; CALLING SEQUENCE:
;	Result = STDEV(Array [, Mean])
;
; INPUTS:
;	Array:	The data array.  Array may be any type except string.
;
; OUTPUTS:
;	STDEV returns the standard deviation (sample variance
;	because the divisor is N-1) of Array.
;		
; OPTIONAL OUTPUT PARAMETERS:
;	Mean:	Upon return, this parameter contains the mean of the values
;		in the data array.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Mean = TOTAL(Array)/N_ELEMENTS(Array)
;	Stdev = SQRT(TOTAL((Array-Mean)^2/(N-1)))
;
; MODIFICATION HISTORY:
;	DMS, RSI, Sept. 1983.
;	[7/17/95] E. Deutsch writes: RSI has done away with this function in
;		IDL 4.0 in favor of a function called moment().  I keep this
;		old routine around for backwards-compatibility and convenience
;		sake.  [ stdev(var) is clearer than sqrt((moment(var)(0)) ]
;-
	on_error,2		;return to caller if error
	n = n_elements(array)	;# of points.
	if n le 1 then message, 'Number of data points must be > 1'
;
        mean = total(array)/n	;yes.
        return,sqrt(total((array-mean)^2)/(n-1))

       end

