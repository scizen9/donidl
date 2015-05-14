function q2rdt,q
;
;  NAME:
;         Q2RDT
;
;  PURPOSE:
;         Transform quaternion 4-vector into
;         RA, dec, twist of GALEX satellite
;
;  INPUTS:
;         quaternion 4-vector
;
;  OUTPUTS:
;         RDT - three vector containing ra, dec, twist in degrees
;		
;
; METHOD:
;
; REVISION HISTORY:
;       Written by D. Schiminovich, December 2001
;

if ( N_params() lt 1 ) then begin  
     print,'Syntax - result = q2rdt(q)'
     print,'Q      - quaternion 4-vector'          
     print,'RESULT - three vector containing ra, dec, twist in degrees'     
     print,'       - ra, dec J2000'
     print,'       - twist rotation of +X about +Z boresite, deg E of N'       
     return, -1
endif 

return,m2rdt(q2m(q))
end
