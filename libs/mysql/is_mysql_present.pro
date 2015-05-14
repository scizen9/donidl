;+
; NAME: is_mysql_present
;
; 	Is there a mysql database here? 
; 	This routine lets you run the code either with or without mysql,
; 	depending on the local machine functionality.
;
; 	Uncomment whichever of the two answers is appropriate for your setup!
; 		
;	TODO: make this autodetect mysql somehow (tricky to make all cases work...)
;
; INPUTS:
; KEYWORDS:
; OUTPUTS:
;
; HISTORY:
; 	Began 2006-07-08 17:55:58 by Marshall Perrin 
;-

FUNCTION is_mysql_present

;	return, 0

	return,1

end
