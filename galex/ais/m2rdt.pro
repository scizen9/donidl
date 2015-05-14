function m2rdt,m

;
;  NAME:
;         M2RDT
;
;  PURPOSE:
;         Transform direction cosines matrix into 
;         RA, dec, twist of GALEX satellite in degrees
;
;  INPUT:
;         M - 3 x 3 direction cosines matrix
;
;  OUTPUTS:
;         RDT - three vector containing ra, dec, twist in degrees
;		
; METHOD:
;
; REVISION HISTORY:
;       Written by D. Schiminovich, December 2001
;

if ( N_params() lt 1 ) then begin  
     print,'Syntax - result = m2rdt(m)'
     print,'M      - 3 x 3 direction cosines matrix'
     print,'RESULT - three vector containing ra, dec, twist in degrees' 
     return, -1
endif 

; calculation of ra, cos, twist (of spacecraft +X axis)
; based on m matrix.

vec=reform(m[2,0:2])
return,[atan(vec[1],vec[0]),asin(vec[2]),atan(m[1,2],m[0,2])]*!RADEG
end










